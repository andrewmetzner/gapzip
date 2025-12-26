(require 'model)

(defservlet delete "text/plain" (args)
  (let ((id (string-to-number (or (httpd-get-arg args "id") "0")))
        (pw (httpd-get-arg args "pw")))
    (if (string= pw board-admin-pass)
        (progn 
          (setq board-threads (cl-remove-if (lambda (p) (= (plist-get p :id) id)) board-threads))
          (board-save)
          (httpd-redirect t "/"))
      (insert "Access Denied"))))

(provide 'admin)
