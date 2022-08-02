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
  "Opens the configuration file."
  (interactive)
  (find-file "~/.emacs.d/config.org"))

; https://stackoverflow.com/a/7250027
(defun beginning-of-text-or-line ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

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

(defun $persp-switch-to-1 ()
  "Switch to the first perspective."
  (interactive)
  (persp-switch-by-number 1))

(defun $persp-switch-to-2 ()
  "Switch to the first perspective."
  (interactive)
  (persp-switch-by-number 2))

(defun $persp-switch-to-3 ()
  "Switch to the first perspective."
  (interactive)
  (persp-switch-by-number 3))

(defun $persp-switch-to-4 ()
  "Switch to the first perspective."
  (interactive)
  (persp-switch-by-number 4))

(defun $persp-switch-to-5 ()
  "Switch to the first perspective."
  (interactive)
  (persp-switch-by-number 5))

(defun $projectile-mark-as-project ()
  "Mark a directory as a projectile project."
  (interactive))

(defun $org-ret ()
  "Perform special actions in specific org blocks. Otherwise perform evil-ret"
  (interactive)
  (cond ((org-in-src-block-p) (org-edit-special))
        ((looking-at "^\\*") (org-toggle-todo))
        (t (evil-ret 1))))

(defun $org-make-todo ()
  "Toggles the current line between a TODO heading and a non-heading"
  (interactive)
  (if (looking-at "^\\*")
      (progn
        (org-todo 'none)
        (org-toggle-heading))
    (progn
      (org-toggle-heading)
      (org-todo 'nextset))))

(defun $kill-emacs-prompt (&optional prompt)
  "Prompts the user whether to kill emacs or not."
  (interactive)
  (y-or-n-p (format "%s" (or prompt "Quit emacs? "))))
