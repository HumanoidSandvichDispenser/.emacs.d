;; https://github.com/Yevgnen/ivy-rich/issues/87#issuecomment-689581896
;; the following fixes slow ivy-rich buffer switching

(defvar ivy-rich--ivy-switch-buffer-cache
  (make-hash-table :test 'equal))

(define-advice ivy-rich--ivy-switch-buffer-transformer
    (:around (old-fn x) cache)
  (let ((ret (gethash x ivy-rich--ivy-switch-buffer-cache)))
    (unless ret
      (setq ret (funcall old-fn x))
      (puthash x ret ivy-rich--ivy-switch-buffer-cache))
    ret))

(define-advice +ivy/switch-buffer
    (:before (&rest _) ivy-rich-reset-cache)
  (clrhash ivy-rich--ivy-switch-buffer-cache))
