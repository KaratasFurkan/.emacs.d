;;; Compiled snippets and support files for `web-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'web-mode
                     '(("multipart" "enctype=\"multipart/form-data\""
                        "multipart_form_data" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/multipart_form_data"
                        nil nil)
                       ("include"
                        "{% include \"$0.html\" ${1:with text=\"Text\" }%}"
                        "include" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/include"
                        nil nil)
                       ("form"
                        "<form method=\"post\"${1: enctype=\"multipart/form-data\"}>\n  {% csrf_token %}\n  {{ form.as_div }}\n  <button type=\"submit\">\n    $0Save\n  </button>\n</form>"
                        "form_as_div" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/form_as_div"
                        nil nil)
                       ("ec"
                        "{% extends \"base.html\" %}\n\n{% block content %}\n$0\n{% endblock content %}"
                        "extend_base_content" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/extends_base_content"
                        nil nil)
                       ("e" "{% extends \"base.html\" %}\n\n$0" "extends_base"
                        nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/extends_base"
                        nil nil)
                       ("url" "{% url \"$1\" %}" "django_url" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/django_url"
                        nil nil)
                       ("t" "{% $0 %}\n" "django_tag" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/django_tag"
                        nil nil)
                       ("if"
                        "{% if ${2:condition} %}\n  $0\n${1:\\{% else %\\}\n\n}{% endif %}"
                        "django_if" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/django_if"
                        nil nil)
                       ("for" "{% for ${1:item} in $1s %}\n  $0\n{% endfor %}"
                        "django_for" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/django_for"
                        nil nil)
                       ("com" "{# $0 #}" "django_comment" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/django_comment"
                        nil nil)
                       ("block"
                        "{% block ${1:block_name} %}\n  $0\n{% endblock $1 %}"
                        "django_block" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/django_block"
                        nil nil)
                       ("bh"
                        "{% load static %}\n<!DOCTYPE html>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"UTF-8\"/>\n    <title>{% block title %}Unmute{% endblock %}</title>\n    {% block head %}{% endblock %}\n  </head>\n  <body class=\"body\">\n    {% block body %}\n\n    {% block navbar %}\n    <nav>\n      <ul>\n        <li><a href=\"\">Home</a></li>\n        <li><a href=\"\">Signup</a></li>\n        <li><a href=\"\">Login</a></li>\n      </ul>\n    </nav>\n    {% endblock navbar %}\n\n    {% if messages %}\n    <ul class=\"message-list\">\n      {% for message in messages %}\n      <li class=\"message-list-item\">\n        {{ message }}\n      </li>\n      {% endfor %}\n    </ul>\n    {% endif %}\n\n    <div class=\"content\">\n      {% block content %}{% endblock %}\n    </div>\n\n    {% endblock body %}\n  </body>\n</html>\n"
                        "base_html" nil nil nil
                        "/home/furkan/.emacs.d/etc/yasnippet/snippets/web-mode/base_html"
                        nil nil)))


;;; Do not edit! File generated at Mon Mar 17 09:53:42 2025
