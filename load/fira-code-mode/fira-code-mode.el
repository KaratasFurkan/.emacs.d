;;; fira-code-mode.el --- Minor mode for Fira Code ligatures using prettify-symbols.

;; This file is not part of GNU Emacs.

;;; License:

;; fira-code-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fira-code-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fira-code-mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Minor mode for Fira Code ligatures, built from these instructions:
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-prettify-symbols
;; 
;; NOTE: Requires installing the Fira Code Symbol font from here:
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632

;;; Code:
(defgroup fira-code-ligatures nil
  "Fira Code ligature settings."
  :version "0.0.1"
  :group 'faces)

(defcustom fira-code-mode-disabled-ligatures ()
  "Add a string to this list to prevent it from being displayed with a ligature.

After editing this variable, any buffers that previously had fira-code-mode enabled
will need to disable and re-enable fira-code-mode in order for the edits to take effect."
  :type '(repeat string) ;; TODO: Make this of type `set'
  :group 'fira-code-ligatures)

(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (delete nil
     (mapcar
      (lambda (s)
	(setq idx (1+ idx))
	(when s
	  (let* ((code (+ #Xe100 idx))
		 (width (string-width s))
		 (prefix ())
		 (suffix '(?\s (Br . Br)))
		 (n 1))
	    (while (< n width)
	      (setq prefix (append prefix '(?\s (Br . Bl))))
	      (setq n (1+ n)))
	    (cons s (append prefix suffix (list (decode-char 'ucs code)))))))
      list))))

(defconst fira-code-mode--all-ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "[]" "::"
    ":::" ":=" "!!" "!=" "!==" "-}" "--" "---" "-->" "->" "->>" "-<"
    "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_(" ".-"
    ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/==" "/>"
    "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>" "++" "+++" "+>"
    "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">=" ">=>"
    ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<!--"
    "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-"
    "<<=" "<<<" "<~" "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>"
    "%%" "x" ":" "+" "+" "*"))

(defun fira-code-mode--ligatures ()
  "Generate a list of all ligatures not disabled via fira-code-mode-disabled-ligatures."
  (mapcar
   (lambda (s)
     (if (member s fira-code-mode-disabled-ligatures)
	 nil ;; The list must retain the same number of elements, with `nil' in-place for disabled ligatures.
       s))
   fira-code-mode--all-ligatures))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append
				      (fira-code-mode--make-alist (fira-code-mode--ligatures))
				      fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter  "  \xe15b"
  :group 'fira-code-ligatures
  (when (not (display-graphic-p))
    (display-warning '(fira-code-ligatures) "Warning: fira-code-mode probably won't work for non-graphical displays!"))
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

;; The following function isn't used directly by fira-code-mode, but it's left here for utility's sake
(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols font."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
  (message "Finished setting up the Fira Code Symbol font."))

(provide 'fira-code-mode)
;;; fira-code-mode.el ends here
