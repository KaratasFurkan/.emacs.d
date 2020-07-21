(defconst config-org (expand-file-name "README.org" user-emacs-directory))
(defconst config-el (expand-file-name "config.el" user-emacs-directory))

(if (file-exists-p config-el)
    (load-file config-el)
  (org-babel-tangle-file config-org config-el))
