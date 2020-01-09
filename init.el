(defvar config-org (expand-file-name "README.org" user-emacs-directory))
(defvar config-el (expand-file-name "README.el" user-emacs-directory))

(if (file-exists-p config-el)
    (load-file config-el)
  (org-babel-load-file config-org))
