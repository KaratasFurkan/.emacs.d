;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("vp"
                        "#+BEGIN_SRC verb :wrap src ob-verb-response\nPOST /$0\n\n{\n  \"foo\": \"bar\"\n}\n#+END_SRC\n"
                        "verb post" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/verb_post"
                        nil nil)
                       ("v"
                        "#+BEGIN_SRC verb :wrap src ob-verb-response\nGET /$0\n#+END_SRC\n"
                        "verb" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/verb"
                        nil nil)
                       ("u"
                        "#+BEGIN_SRC emacs-lisp\n(use-package $0)\n#+END_SRC\n"
                        "use-package" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/use-package"
                        nil nil)
                       ("to" ";; TODO: $0\n" "todo2" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/todo2"
                        nil nil)
                       ("t" "  ; TODO: $0\n" "todo" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/todo"
                        nil nil)
                       ("ti" "#+TITLE: $0" "title" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/title"
                        nil nil)
                       ("s" "#+BEGIN_SRC ${1:emacs-lisp}\n$0\n#+END_SRC"
                        "source code" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/src"
                        nil nil)
                       ("rp"
                        "#+BEGIN_SRC restclient\nPOST $0\n\n{\n  \"foo\": \"bar\"\n}\n#+END_SRC"
                        "restclient post" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/restclient_post"
                        nil nil)
                       ("rg" "#+BEGIN_SRC restclient\nGET $0\n#+END_SRC"
                        "restclient get" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/restclient_get"
                        nil nil)
                       ("r" "#+BEGIN_SRC restclient\n$0\n#+END_SRC" "restclient"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/restclient"
                        nil nil)
                       ("q" "#+begin_quote\n$0\n#+end_quote" "quote" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/quote"
                        nil nil)
                       ("li" "[[${1:link}][${2:description}]$0\n" "link" nil nil
                        nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/link"
                        nil nil)
                       ("im"
                        "${1:#+ATTR_ORG: :width 500px}\n[[${2:image_path}]]$0"
                        "image" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/image"
                        nil nil)
                       ("e" "#+BEGIN_EXAMPLE\n$0\n#+END_EXAMPLE" "example" nil
                        nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/exampleblock"
                        nil nil)
                       ("d"
                        "- [ ] Stretching$0\n- [ ] Put phone away\n- [ ] Clock-in as \"Todo management\"\n- [ ] Email & Slack check\n- [ ] Todo Planning\n- [ ] Time blocking\n- [ ] Give an update on slack if necessary\n- [ ] Fill up worksheet\n\nTodos:\n- [ ] X"
                        "daily checklist" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/org-mode/daily-checklist"
                        nil nil)))


;;; Do not edit! File generated at Mon Mar 17 09:53:42 2025
