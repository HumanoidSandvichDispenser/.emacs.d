; define functions and interactive commands
(load-file
 (expand-file-name
  "functions.el"
  user-emacs-directory))

(load-file
 (expand-file-name
  "ivy-rich-fix.el"
  user-emacs-directory))

; load our configuration
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
