;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("weal" "(with-eval-after-load '$1\n  $0)"
                        "with-eval-after-load" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/with-eval-after-load"
                        nil nil)
                       ("wb" "(with-current-buffer $0)" "with-current-buffer"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/with-current-buffer"
                        nil nil)
                       ("w" "(when $0)" "when" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/when"
                        nil nil)
                       ("upc" "(${1:face} ((t (:${2:attribute}$0))))"
                        "use-package-custom-face" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/use-package-custom-face"
                        nil nil)
                       ("upb" ":bind (\"${1:binding}\" . ${2:function-name})"
                        "use-package binding" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/use-package-binding"
                        nil nil)
                       ("u" "(use-package $0)" "usepackage" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/use-package"
                        nil nil)
                       ("un" "(unless $0)" "unless" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/unless"
                        nil nil)
                       ("sg"
                        ":straight (:host github :repo \"${1:username/repo}\"${2: :branch \"${3:branch}\"})"
                        "straight-github" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/straight-github"
                        nil nil)
                       ("sbi" ":straight (:type built-in)" "straight-built-in"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/straight-built-in"
                        nil nil)
                       ("s" "(setopt $0)" "setopt" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/setopt"
                        nil nil)
                       ("face"
                        "(set-face-attribute '${1:face} nil ${0:attributes})"
                        "set-face-attribute" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/set-face-attribute"
                        nil nil)
                       ("se" "(save-excursion $0)" "save-excursion" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/save-excursion"
                        nil nil)
                       ("sb" "(save-buffer $0)" "save-buffer" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/save-buffer"
                        nil nil)
                       ("re" "(region-end)" "region-end" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/region-end"
                        nil nil)
                       ("rb" "(region-beginning)" "region-beginning" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/region-beginning"
                        nil nil)
                       ("rap" "(region-active-p)" "region-active-p" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/region-active-p"
                        nil nil)
                       ("pm" "(point-min)" "point-min" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/point-min"
                        nil nil)
                       ("px" "(point-max)" "point-max" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/point-max"
                        nil nil)
                       ("p" "(point)" "point" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/point"
                        nil nil)
                       ("o" "(or $0)" "or" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/or"
                        nil nil)
                       ("nth" "(nth ${1:index} ${2:list})" "nth" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/nth"
                        nil nil)
                       ("n" "(not $0)" "not" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/not"
                        nil nil)
                       ("m" "(message \"${1:%s}\"${2: format-args})" "message"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/message"
                        nil nil)
                       ("l" "(let${1:*} (($0))\n  )" "let" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/let"
                        nil nil)
                       ("lam" "(lambda ($1) ${2:(interactive) }${3:($4)})"
                        "lambda" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/lambda"
                        nil nil)
                       ("kb" "(kill-buffer $0)" "kill-buffer" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/kill-buffer"
                        nil nil)
                       ("kbd" "(kbd \"$0\")" "kbd" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/kbd"
                        nil nil)
                       ("in" "(interactive)" "interactive" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/interactive"
                        nil nil)
                       ("i" "(insert $0)" "insert" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/insert"
                        nil nil)
                       ("if" "(if ${1:condition}\n    ${2:($3)}\n  ${4:($5)})"
                        "if" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/if"
                        nil nil)
                       ("gsk" "(keymap-global-set \"${1:key}\" '$0)\n"
                        "keymap-global-set" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/global-set-key"
                        nil nil)
                       ("f" "(format \"${1:%s}\" ${2:format-args})" "format" nil
                        nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/format"
                        nil nil)
                       ("efn"
                        "(expand-file-name \"${2:file}\" ${1:user-emacs-directory})"
                        "expand-file-name" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/expand-file-name"
                        nil nil)
                       ("defvar"
                        "(defvar ${1:symbol} ${2:initvalue} \"${3:docstring}\")"
                        "defvar" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/defvar"
                        nil nil)
                       ("def"
                        "(defun ${1:fk/${2:func}} (${3:orig-func &rest args})\n  \"${4:docstring}\"\n  ${5:(interactive)}\n  $0)"
                        "defun" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/defun"
                        nil nil)
                       ("ks" "(keymap-set ${1:keymap} \"${2:key}\" '$0)"
                        "keymap-set" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/define-key"
                        nil nil)
                       ("defcustom"
                        "(defcustom ${1:symbol} ${2:standard} \"${3:docstring}\"${4: args})"
                        "defcustom" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/defcustom"
                        nil nil)
                       ("cb" "(current-buffer)" "current-buffer" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/current-buffer"
                        nil nil)
                       ("const"
                        "(defconst ${1:name} ${2:value}${3: \"${4:docstring}\"})$0"
                        "defconst" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/const"
                        nil nil)
                       ("cond" "(cond\n (${1:condition} ${2:body})$0)" "cond"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/cond"
                        nil nil)
                       ("ci" "(call-interactively '$0)" "call-interactively" nil
                        nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/call-interactively"
                        nil nil)
                       ("bsnp"
                        "(buffer-substring-no-properties ${1:start} ${2:end})"
                        "buffer-substring-no-properties" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/buffer-substring-no-properties"
                        nil nil)
                       ("bs" "(buffer-string)" "buffer-string" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/buffer-string"
                        nil nil)
                       ("bfn" "(buffer-file-name)" "buffer-file-name" nil nil
                        nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/buffer-file-name"
                        nil nil)
                       ("ao" "(apply orig-func args)" "apply-orig" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/apply-orig"
                        nil nil)
                       ("a" "(and $0)" "and" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/and"
                        nil nil)
                       ("aa" "(advice-add '$1 :${2:override} '$3)$0"
                        "advice-add" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/advice-add"
                        nil nil)
                       ("ah" "(add-hook '${1:hook} ${2:'${3:function}})$0\n"
                        "add-hook" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/emacs-lisp-mode/add-hook"
                        nil nil)))


;;; Do not edit! File generated at Mon Mar 17 09:53:42 2025
