(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(undecorated . t))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode)

(defvar default-font-size 90)
(set-face-attribute 'default nil :family "Source Code Pro" :height default-font-size)
