;;; global-modeline.el --- Persistent global modeline in the echo area -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))
;; Keywords: mode-line, convenience

;;; Commentary:

;; `global-modeline-mode' displays the output of the functions in
;; `global-modeline-items', right-aligned, in the echo area -- and keeps
;; it visible even while regular messages are shown.
;;
;; How co-existence with messages works (the tricky part):
;;
;; The echo area can display three different buffers:
;;
;;   1. " *Echo Area 0*" / " *Echo Area 1*" -- used while a `message'
;;      is being displayed.
;;   2. " *Minibuf-0*" -- the *default* echo-area content, shown
;;      whenever no message is active and the minibuffer is not in use.
;;   3. " *Minibuf-1*" (and deeper) -- the active minibuffer.
;;
;; So instead of fighting `message' (re-echoing, advice wars, flicker),
;; we cover all three surfaces:
;;
;;   - We keep a whole-buffer overlay with an `after-string' in both
;;     echo-area buffers.  Whatever a message prints, our text is
;;     appended after it by the redisplay engine itself -- the message
;;     is never touched, never erased, and both are visible at once.
;;   - We write our text directly into " *Minibuf-0*", so when the echo
;;     area is "empty", Emacs's idea of "empty" *is* our modeline.
;;   - On `minibuffer-setup-hook' we drop a zero-width overlay at the
;;     end of the active minibuffer, so the modeline also stays visible
;;     while you're in M-x, find-file, etc.
;;
;; Right-alignment is done with a display spec
;; `(space :align-to (- right-fringe WIDTH))', which redisplay resolves
;; against the current frame width, so it survives frame resizing for
;; free.

;;; Code:

(require 'seq)

(defgroup global-modeline nil
  "Persistent, right-aligned modeline in the echo area."
  :group 'mode-line
  :prefix "global-modeline-")

(defcustom global-modeline-items
  '(global-modeline-item-git-branch global-modeline-item-time)
  "List of functions whose results are displayed in the echo area.
Each function is called with no arguments, with the buffer of the
selected window current, and should return a string (possibly
propertized) or nil to contribute nothing.

Keep these functions cheap: they run on every command (and on the
timer, if `global-modeline-update-interval' is non-nil)."
  :type '(repeat function))

(defcustom global-modeline-separator "  "
  "String inserted between segments."
  :type 'string)

(defcustom global-modeline-right-padding 1
  "Number of columns between the modeline text and the right edge."
  :type 'natnum)

(defcustom global-modeline-show-in-minibuffer nil
  "If non-nil, also display the modeline inside the active minibuffer.
When nil (the default), the modeline is hidden while the minibuffer
is in use (M-x, `find-file', completion sessions, ...) and reappears
when you exit.  Keep this nil if you use `vertico-posframe' or
similar packages that re-display the minibuffer contents elsewhere;
otherwise the modeline string gets dragged into the posframe.

Takes effect immediately; no need to restart the mode."
  :type 'boolean)

(defcustom global-modeline-update-interval 1
  "Seconds between periodic refreshes, or nil to disable the timer.
The modeline always refreshes via `post-command-hook'; the timer
additionally keeps time-like segments current while Emacs is idle."
  :type '(choice (const :tag "Update only on commands" nil) number))

(defface global-modeline-face
  '((t :inherit shadow))
  "Default face applied to the whole echo-area modeline.
Segments may override it with their own text properties.")

;;;; Internals

(defvar global-modeline--overlays nil
  "Overlays carrying the modeline `after-string'.")

(defvar global-modeline--timer nil)

(defvar global-modeline--cache ""
  "Last rendered (already right-aligned) modeline string.")

(defun global-modeline--collect ()
  "Run `global-modeline-items' and join the results."
  (let ((parts (delq nil
                     (mapcar (lambda (fn)
                               (let ((s (and (fboundp fn)
                                             (ignore-errors (funcall fn)))))
                                 (and (stringp s)
                                      (not (string-empty-p s))
                                      s)))
                             global-modeline-items))))
    (string-join parts global-modeline-separator)))

(defun global-modeline--render (text)
  "Return TEXT preceded by a stretch-space that right-aligns it."
  (if (string-empty-p text)
      ""
    (let ((text (if (text-property-not-all 0 (length text) 'face nil text)
                    text
                  (propertize text 'face 'global-modeline-face))))
      (concat
       ;; The `cursor' property keeps point rendering sanely when this
       ;; string sits at the end of an active minibuffer.
       ;;
       ;; Width is measured in pixels (`(N)' in a display spec means N
       ;; pixels, a bare N means columns).  `string-width' counts
       ;; columns and goes subtly wrong for propertized text whose
       ;; faces/glyphs render wider than the default font (bold,
       ;; unicode, themed mode-line segments...), causing overflow.
       (propertize " " 'cursor 1
                   'display `(space :align-to
                                    (- right-fringe
                                       (,(string-pixel-width text))
                                       ,global-modeline-right-padding)))
       text))))

(defun global-modeline--display ()
  "Push the cached string onto every echo-area surface."
  ;; 1. Overlays in the echo-area buffers (and active minibuffer):
  ;;    appended after whatever message is currently shown.
  (dolist (ov global-modeline--overlays)
    (when (overlay-buffer ov)
      (overlay-put ov 'after-string
                   (unless (string-empty-p global-modeline--cache)
                     global-modeline--cache))))
  ;; 2. " *Minibuf-0*": the echo area's content when it is "empty".
  (with-current-buffer (get-buffer-create " *Minibuf-0*")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert global-modeline--cache))))

(defun global-modeline-update (&rest _)
  "Recompute `global-modeline-items' and refresh the echo area."
  (when global-modeline-mode
    (let ((text (with-current-buffer (window-buffer (selected-window))
                  (global-modeline--collect))))
      (setq global-modeline--cache (global-modeline--render text)))
    (global-modeline--display)))

;;;; Minibuffer handling

(defun global-modeline--minibuffer-setup ()
  "Attach the modeline to the freshly activated minibuffer (if enabled)."
  (when global-modeline-show-in-minibuffer
    (let ((ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'priority 1)
      (push ov global-modeline--overlays))
    (global-modeline--display)))

(defun global-modeline--minibuffer-exit ()
  "Drop the overlay belonging to the minibuffer being exited."
  (setq global-modeline--overlays
        (seq-remove (lambda (ov)
                      (when (eq (overlay-buffer ov) (current-buffer))
                        (delete-overlay ov)
                        t))
                    global-modeline--overlays)))

;;;; Lifecycle

(defun global-modeline--delete-overlays ()
  (mapc #'delete-overlay global-modeline--overlays)
  (setq global-modeline--overlays nil))

(defun global-modeline--enable ()
  (global-modeline--delete-overlays)
  ;; Messages alternate between these two buffers; cover both with a
  ;; whole-buffer, rear-advancing overlay so `after-string' always ends
  ;; up after the message text.
  (dolist (name '(" *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create name)
      (remove-overlays (point-min) (point-max))
      (push (make-overlay (point-min) (point-max) nil nil t)
            global-modeline--overlays)))
  (add-hook 'post-command-hook #'global-modeline-update)
  (add-hook 'minibuffer-setup-hook #'global-modeline--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'global-modeline--minibuffer-exit)
  (when global-modeline-update-interval
    (setq global-modeline--timer
          (run-with-timer 0 global-modeline-update-interval
                          #'global-modeline-update)))
  (global-modeline-update))

(defun global-modeline--disable ()
  (remove-hook 'post-command-hook #'global-modeline-update)
  (remove-hook 'minibuffer-setup-hook #'global-modeline--minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'global-modeline--minibuffer-exit)
  (when (timerp global-modeline--timer)
    (cancel-timer global-modeline--timer)
    (setq global-modeline--timer nil))
  (global-modeline--delete-overlays)
  (setq global-modeline--cache "")
  (when-let* ((buf (get-buffer " *Minibuf-0*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))))
  ;; Force the echo area to pick up the now-empty state.
  (message nil))

;;;###autoload
(define-minor-mode global-modeline-mode
  "Show `global-modeline-items' persistently in the echo area.
The content is right-aligned and co-exists with regular messages:
when something is printed with `message' (or by eldoc, isearch,
etc.), the modeline is appended after it instead of disappearing.
Completely independent of the per-window mode line."
  :global t
  :group 'global-modeline
  (if global-modeline-mode
      (global-modeline--enable)
    (global-modeline--disable)))

;;;; Example segment functions

(defun global-modeline-item-time ()
  "Current time, HH:MM."
  (format-time-string "%H:%M"))

(defun global-modeline-item-git-branch ()
  "Current Git branch, read cheaply from the buffer-local `vc-mode'.
No git process is spawned, so this is safe for `post-command-hook'."
  (when (and (bound-and-true-p vc-mode) (stringp vc-mode))
    (let ((s (substring-no-properties vc-mode)))
      (when (string-match "Git[:-]\\(.+\\)" s)
        (concat "\u2387 " (match-string 1 s))))))

(provide 'global-modeline)
;;; global-modeline.el ends here
