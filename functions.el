(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun find-config-file ()
  "Open the configuration file."
  (interactive)
  (find-file "~/.emacs.d/config.org"))

; https://stackoverflow.com/a/7250027
(defun beginning-of-text-or-line ()
  "Move point to the beginning of text on the current line; if that is
already the current position of point, then move it to the beginning
of the line."
  (interactive)
  (let ((pt (point)))
    (if olivetti-mode
        (progn
          (beginning-of-visual-line))
      (progn
        (beginning-of-line-text)))
    (when (eq pt (point))
      (beginning-of-line))))

(defun end-of-text-or-line ()
  "Move point to the end of the text"
  (interactive)
  (let ((pt (point)))
    (end-of-visual-line)
    (when (eq pt (point))
      (end-of-line))))

(defun org-append-heading ()
  "Append heading after the current heading and enter insert mode."
  (interactive)
  (org-insert-heading-respect-content)
  (evil-append 1))

(defun org-append-todo-heading ()
  "Append TODO heading after the current heading and enter insert mode."
  (interactive)
  (org-insert-todo-heading-respect-content)
  (evil-append 1))

(defun $org-ret ()
  "Perform special actions in specific org blocks. Otherwise perform evil-ret"
  (interactive)
  (cond ((org-in-src-block-p) (org-edit-special))
        ((looking-at "^\\*") (org-toggle-todo))
        (t (evil-ret 1))))

(defun $org-make-todo ()
  "Toggle the current line between a TODO heading and a non-heading."
  (interactive)
  (if (looking-at "^\\*")
      (progn
        (org-todo 'none)
        (org-toggle-heading))
    (progn
      (org-toggle-heading)
      (org-todo 'nextset))))

(defun $kill-emacs-prompt (&optional prompt)
  "Display with optional PROMPT to kill Emacs."
  (interactive)
  (y-or-n-p (format "%s?" (or prompt "Quit emacs"))))

(defun $ivy-posframe-get-size ()
  "Set the ivy-posframe size according to the current frame."
  (let ((height (or ivy-posframe-height (or ivy-height 20)))
        (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
    (list :height height :width width :min-height height :min-width width)))

(defun $org-agenda-project ()
  "Opens the Org Agenda in the Org Agenda project."
  (projectile-switch-project-by-name "~/Dropbox/Documents/org/"))

;; https://gist.github.com/brianloveswords/e23cedf3a80bab675fe5
(defun $newline-and-indent ()
  "Add two newlines and put the cursor at the right indentation
between them if a newline is attempted when the cursor is between
two curly braces, otherwise do a regular newline and indent"
  (interactive)
  (if (and (equal (char-before) 123) ; {
           (equal (char-after) 125)) ; }
      (progn
        (newline-and-indent)
        (split-line)
        (indent-for-tab-command))
    (if (derived-mode-p 'prog-mode)
        (newline-and-indent)
      (electric-newline-and-maybe-indent)
      (indent-relative-first-indent-point))))

(defun $prog-delete-trailing-whitespace ()
  "Deletes trailing whitespace only in 'prog-mode'."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

(defun $find-file-or-project ()
  "Find file in the project root or the current directory if not in a project."
  (interactive)
  (if (project-current)
      (progn
        (call-interactively 'project-find-file))
    (progn
      (call-interactively 'find-file))))

(defun $tabspaces-counsel-switch-buffer ()
  "A version of `tabspaces-switch-to-buffer' which respects tab-mode workspaces with `tabspaces-mode'."
  (interactive)
   (let ((blst (mapcar #'buffer-name (tabspaces--buffer-list))))
     (ivy-read "Switch to local buffer: " blst
               :matcher #'ivy--switch-buffer-matcher
               :require-match t
               :predicate (lambda (b)
                            (member (if (stringp b) b (car b)) blst))
               :preselect (buffer-name (other-buffer (current-buffer)))
               :action (lambda (choice)
                         (switch-to-buffer choice))
               :caller 'ivy-switch-buffer)))

(defun $switch-to-scratch-buffer ()
  "Switch to the scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun $tab-bar-tab-name-format (tab i)
  "Format a tab name given a TAB and I index `tab-bar-mode'."
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat "  "
             (if tab-bar-tab-hints (format "[%d] " i) "")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p 'non-selected 'selected)))
                      tab-bar-close-button)
                 "")
             "  ")
     'face (funcall tab-bar-tab-face-function tab))))

(defun $lsp-ui-doc-glance-or-focus ()
  "Glance over hover information popup.  If the information popup is already open, focus onto it."
  (interactive)
  (if (lsp-ui-doc--frame-visible-p)
      (progn
        (message "Visible")
        (lsp-ui-doc-hide)
        (lsp-ui-doc-show)
        (lsp-ui-doc-focus-frame))
    ;; lsp-ui-doc--visible-p is nil if we're in doc buffer
    (message "Not visible")
    (lsp-ui-doc-unfocus-frame)
    (lsp-ui-doc-glance)))

(defun $tabspaces-kill-stray-buffers-close-workspace ()
  "Kill the current tab as well as buffers that do not exist in any other workspace."
  (interactive)
  (let ((buffers (tabspaces--buffer-list))
        (tabs (tabspaces--list-tabspaces)))
    (dolist (buffer buffers)
      (let ((tab-exists nil)
            (idx 0))
        ;; check if buffer exists in other tabs
        (dolist (tab tabs)
          ;; filter out the current tab or break out if tab-exists is already `t'
          (if (and (not tab-exists) (/= idx (tab-bar--current-tab-index)))
              ;; if the buffer is a member of the tab's bufferlist
              (when (member buffer (tabspaces--buffer-list nil idx))
                (setq tab-exists t)))
          (cl-incf idx))
        ;; kill buffers that are not in any other tab
        (when (not tab-exists)
          (message "buffer %s NOT in other tabs" buffer)
          (kill-buffer buffer)))))
  ;; close the workspace
  (tabspaces-close-workspace))

(provide 'functions)
;;; functions.el ends here
