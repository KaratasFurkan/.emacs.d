(defconst config-org (locate-user-emacs-file "README.org"))
(defconst config-el (locate-user-emacs-file "config.el"))

(unless (file-exists-p config-el)
  (require 'org)
  (org-babel-tangle-file config-org config-el))

(load-file config-el)
