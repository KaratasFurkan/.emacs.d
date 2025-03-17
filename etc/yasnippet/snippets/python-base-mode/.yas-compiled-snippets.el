;;; Compiled snippets and support files for `python-base-mode'
;;; contents of the .yas-setup.el support file:
;;;
(require 'yasnippet)
(defvar yas-text)

(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))


(add-hook 'python-mode-hook #'yasnippet-snippets--fixed-indent)
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-base-mode
                     '(("with" "with ${1:expr}${2: as ${3:alias}}:\n    $0"
                        "with" nil ("control structure") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/with"
                        nil nil)
                       ("wh" "while ${1:True}:\n    $0" "while" nil
                        ("control structure") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/while"
                        nil nil)
                       ("urlspy"
                        "from django.urls import path\n\napp_name = '`(file-name-nondirectory (string-remove-suffix \"/\" default-directory))`$0'\n\nurlpatterns = [\n    path('', SomeListView.as_view(), name='some_list'),\n]\n"
                        "urlspy" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/urlspy"
                        nil nil)
                       ("uf" "update_fields=['$0']" "update_fields" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/update_fields"
                        nil nil)
                       ("try" "try:\n    $0\nexcept $1:\n    $2\nelse:\n    $3"
                        "tryelse" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/tryelse"
                        nil nil)
                       ("try" "try:\n    $0\nexcept ${1:Exception}:\n    $2"
                        "try" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/try"
                        nil nil)
                       ("ta" "@transaction.atomic" "transaction_atomic" nil nil
                        nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/transaction_atomic"
                        nil nil)
                       ("su" "super().$0" "super" nil ("object oriented") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/super"
                        nil nil)
                       ("str" "def __str__(self):\n    return $0" "__str__" nil
                        ("dunder methods") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/str"
                        nil nil)
                       ("sm" "@staticmethod\ndef ${1:func}($0):\n" "static" nil
                        nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/static"
                        nil nil)
                       ("sn" "self.$1 = $1" "selfassign" nil ("object oriented")
                        nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/selfassign"
                        nil nil)
                       ("s" "self" "self_without_dot" nil ("object oriented")
                        nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/self_without_dot"
                        nil nil)
                       ("." "self.$0" "self" nil ("object oriented") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/self"
                        nil nil)
                       ("sc" "class ${1:class}(${2:super-class}):\n    $0\n"
                        "subclass" nil ("object oriented") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/scls"
                        nil nil)
                       ("suf" "save(update_fields=['$0'])" "save_update_fields"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/save_update_fields"
                        nil nil)
                       ("saveclean"
                        "def save(self, *args, **kwargs):\n    self.full_clean()\n    super().save(*args, **kwargs)\n\ndef clean(self):\n    if self._state.adding:\n        $0'do_something_if_created'\n    else:\n        'do_something_if_updated'"
                        "save+clean" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/save+clean"
                        nil nil)
                       ("save"
                        "def save(self, *args, **kwargs):\n    self.full_clean()\n    super().save(*args, **kwargs)\n"
                        "save" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/save"
                        nil nil)
                       ("r" "return $0" "return" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/return"
                        nil nil)
                       ("pudb" "import pudb; pudb.set_trace()" "pudb trace" nil
                        ("debug") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/pudb"
                        nil nil)
                       ("prop" "@property\ndef ${1:property}(self):\n    $0\n"
                        "property" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/property"
                        nil nil)
                       ("p" "print($0)" "print" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/print"
                        nil nil)
                       ("post"
                        "def post(self, request, *args, **kwargs):\n    $0\n    return super().post(request, *args, **kwargs)"
                        "post" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/post"
                        nil nil)
                       ("pdb" "import pdb; pdb.set_trace()" "pdb trace" nil
                        ("debug") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/pdb"
                        nil nil)
                       ("migration"
                        "from django.db import migrations\n\n\ndef do_something(apps, schema_editor):\n    ${1:model$$(capitalize yas-text)} = apps.get_model('app_name', '${1:$(capitalize yas-text)}')\n    for ${1:$(downcase yas-text)} in ${1:$(capitalize yas-text)}.objects.all():\n        ${1:$(downcase yas-text)}.something = 'xyz'\n        ${1:$(downcase yas-text)}.save(update_fields=['something'])\n\n\nclass Migration(migrations.Migration):\n\n    dependencies = [\n        ('APP_NAME', 'THE_MIGRATION_BEFORE_THIS_ONE'),\n    ]\n\n    operations = [\n        migrations.RunPython(do_something, reverse_code=migrations.RunPython.noop),\n    ]\n"
                        "migration" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/migration"
                        nil nil)
                       ("m" "def ${1:method}(self${2:, $3}):\n    $0" "method"
                        nil ("object oriented") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/method"
                        nil nil)
                       ("meta" "class Meta:\n    $0" "meta" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/meta"
                        nil nil)
                       ("li" "[${1:el} for $1 in ${2:list}]\n$0" "list" nil
                        ("definitions") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/list"
                        nil nil)
                       ("lam" "lambda ${1:x}: $0" "lambda" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/lambda"
                        nil nil)
                       ("ipdb" "import ipdb; ipdb.set_trace()" "ipdb trace" nil
                        ("debug") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/ipdb"
                        nil nil)
                       ("init"
                        "def __init__(self, *args, **kwargs):\n    super().__init__(*args, **kwargs)"
                        "init" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/init"
                        nil nil)
                       ("imp" "import ${1:lib}${2: as ${3:alias}}\n$0" "import"
                        nil ("general") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/import"
                        nil nil)
                       ("ifm" "if __name__ == '__main__':\n    ${1:main()}"
                        "ifmain" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/ifmain"
                        nil nil)
                       ("ife" "if $1:\n    $2\nelse:\n    $0\n" "ife" nil
                        ("control structure") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/ife"
                        nil nil)
                       ("if" "if ${1:cond}:\n    $0\n" "if" nil
                        ("control structure") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/if"
                        nil nil)
                       ("gettext"
                        "from django.utils.translation import gettext_lazy as _"
                        "gettext" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/gettext"
                        nil nil)
                       ("get_queryset"
                        "def get_queryset(self, *args, **kwargs):\n    $0\n    return super().get_queryset(*args, **kwargs)"
                        "get_queryset" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/get_queryset"
                        nil nil)
                       ("context2"
                        "def get_context_data(self, *args, **kwargs):\n    context = super().get_context_data(*args, **kwargs)\n    context['$1'] = $2\n    return context\n"
                        "get_context_data2" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/get_context_data2"
                        nil nil)
                       ("context"
                        "def get_context_data(self, *args, **kwargs):\n    return {\n        **super().get_context_data(*args, **kwargs),\n        '$1': $2,\n    }\n"
                        "get_context_data" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/get_context_data"
                        nil nil)
                       ("get"
                        "def get(self, request, *args, **kwargs):\n    $0\n    return super().get(request, *args, **kwargs)"
                        "get" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/get"
                        nil nil)
                       ("f" "def ${1:fun}(${2:args}):\n    $0\n" "function" nil
                        ("definitions") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/function"
                        nil nil)
                       ("fr" "from ${1:lib} import ${2:funs}" "from" nil
                        ("general") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/from"
                        nil nil)
                       ("form_valid2"
                        "def form_valid(self, form):\n    response = super().form_valid(form)\n    $0\n    return response\n"
                        "form_valid2" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/form_valid2"
                        nil nil)
                       ("form_valid"
                        "def form_valid(self, form):\n    ${1:form.instance.user = self.request.user}$0\n    return super().form_valid(form)"
                        "form_valid" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/form_valid"
                        nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0"
                        "for ... in ... : ..." nil ("control structure") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/for"
                        nil nil)
                       ("file"
                        "with open(${1:\"${2:filename}\"}${3:, \"$4\"}) as ${5:file}:\n    ${6:for line in $5:\n        $0}"
                        "file" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/file"
                        nil nil)
                       ("ef"
                        "${1:$(upcase yas-text)} = '${1:choice$$(replace-regexp-in-string \" \" \"_\" yas-text)}', _('${1:$(replace-regexp-in-string \"[_-]\" \" \" (capitalize yas-text))}')"
                        "enum_field" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/enum_field"
                        nil nil)
                       ("enum"
                        "class Status(models.TextChoices):\n    PENDING = 'pending', _('Pending')\n    COMPLETE = 'complete', _('Complete')"
                        "enum" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/enum"
                        nil nil)
                       ("d" "\"\"\"$0\n\"\"\"" "doc" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/doc"
                        nil nil)
                       ("dispatch"
                        "def dispatch(self, request, *args, **kwargs):\n    $0\n    return super().dispatch(request, *args, **kwargs)"
                        "dispatch" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/dispatch"
                        nil nil)
                       ("dt" "def test_${1:long_name}(self):\n    $0" "deftest"
                        nil ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/deftest"
                        nil nil)
                       ("cls" "class ${1:class}:\n    $0\n" "class" nil
                        ("object oriented") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/cls"
                        nil nil)
                       ("clean"
                        "def clean(self):\n    if self._state.adding:\n        $0'do_something_if_created'\n    else:\n        'do_something_if_updated'"
                        "clean" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/clean"
                        nil nil)
                       ("cm" "@classmethod\ndef ${1:meth}(cls, $2):\n    $0"
                        "classmethod" nil ("object oriented") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/classmethod"
                        nil nil)
                       ("b" "breakpoint()" "breakpoint" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/breakpoint"
                        nil nil)
                       ("at" "self.assertTrue($0)" "assertTrue" nil ("testing")
                        nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertTrue"
                        nil nil)
                       ("ar" "with self.assertRaises(${1:Exception}):\n    $0\n"
                        "assertRaises" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertRaises.with"
                        nil nil)
                       ("ar" "self.assertRaises(${1:Exception}, ${2:fun})"
                        "assertRaises" nil ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertRaises"
                        nil nil)
                       ("an" "self.assertNotIn(${1:member}, ${2:container})"
                        "assetNotIn" nil ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertNotIn"
                        nil nil)
                       ("ane" "self.assertNotEqual($1, $2)" "assertNotEqual" nil
                        ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertNotEqual"
                        nil nil)
                       ("ai" "self.assertIn(${1:member}, ${2:container})"
                        "assertIn" nil ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertIn"
                        nil nil)
                       ("af" "self.assertFalse($0)" "assertFalse" nil
                        ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertFalse"
                        nil nil)
                       ("ae" "self.assertEqual($1, $2)" "assertEqual" nil
                        ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assertEqual"
                        nil nil)
                       ("ass" "assert $0" "assert" nil ("testing") nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/assert"
                        nil nil)
                       ("ak" "*args, **kwargs" "args_kwargs" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/python-base-mode/args_kwargs"
                        nil nil)))


;;; Do not edit! File generated at Mon Mar 17 09:53:42 2025
