;;; controller.el --- Post & Admin Logic
(require 'model)

(defun board-get-forwarded-ip (args)
  (let ((xff (cadr (assoc "X-Forwarded-For" args))))
    (when xff
      (car (split-string xff ",")))))

(defun board-is-admin-p (proc args)
  (let ((cookie (cadr (assoc "Cookie" args)))) 
    (and cookie (string-match (format "session=%s" (regexp-quote board-admin-password)) cookie))))

;; --- ADMIN ACTIONS ---

(defun board-admin-dashboard-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403) 
    (with-httpd-buffer proc "text/html" 
      (insert (render-header "Admin Dashboard" t)) 
      (insert-board-ascii) 
      (insert "<h2>Banned IPs</h2><ul>")
      (if board-banned-ips
          (dolist (ip board-banned-ips)
            (insert (format "<li>%s <a href='/admin/unban?ip=%s'>[Unban]</a></li>" ip ip)))
        (insert "<li>No bans found.</li>"))
      (insert "</ul><hr><a href='/home'>Back to Home</a></body></html>"))))

(defun board-admin-delete-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403) 
    (let* ((id-param (cadr (assoc "id" query)))
           (id (when id-param (string-to-number id-param))))
      (when id
        (let ((thread (cl-find-if (lambda (x) (= (plist-get (plist-get x :op) :id) id)) board-threads)))
          (if thread
              (setq board-threads (delete thread board-threads))
            (dolist (tt board-threads)
              (plist-put tt :replies 
                         (cl-remove-if (lambda (r) (= (plist-get r :id) id)) 
                                       (plist-get tt :replies))))))
        (board-save)
        (httpd-redirect proc "/home")))))

(defun board-admin-ban-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403) 
    (let ((ip (cadr (assoc "ip" query)))) 
      (when (and ip (not (member ip board-banned-ips)))
        (push ip board-banned-ips) 
        (board-save))
      (httpd-redirect proc "/home"))))

(defun board-admin-unban-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403) 
    (let ((ip (cadr (assoc "ip" query))))
      (setq board-banned-ips (delete ip board-banned-ips))
      (board-save)
      (httpd-redirect proc "/admin/dashboard"))))

(defun board-admin-logout-route (proc path query args)
  (httpd-send-header proc "text/html" 302 
                     :Location "/home" 
                     :Set-Cookie "session=; Path=/; Max-Age=0; Expires=Thu, 01 Jan 1970 00:00:00 GMT")
  (process-send-string proc ""))

;; --- EDIT LOGIC ---

(defun board-admin-edit-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403) 
    (let* ((id-param (cadr (assoc "id" query)))
           (id (when id-param (string-to-number id-param)))
           (post (or (let ((tt (cl-find-if (lambda (x) (= (plist-get (plist-get x :op) :id) id)) board-threads)))
                       (when tt (plist-get tt :op)))
                     (cl-some (lambda (tt) 
                                (cl-find-if (lambda (r) (= (plist-get r :id) id)) 
                                            (plist-get tt :replies))) 
                              board-threads))))
      (if (not post)
          (httpd-error proc 404)
        (with-httpd-buffer proc "text/html"
          (insert (render-header (format "Editing Post #%d" id) t))
          (insert (format "<h2>Edit Post #%d</h2>" id))
          (insert (format "
            <form method='POST' action='/admin/update'>
              <input type='hidden' name='id' value='%d'>
              <label>Author:</label><br>
              <input type='text' name='name' value='%s' style='width:100%%; margin-bottom:10px;'><br>
              <label>Subject:</label><br>
              <input type='text' name='subject' value='%s' style='width:100%%; margin-bottom:10px;'><br>
              <label>Comment:</label><br>
              <textarea name='comment' rows='12' cols='80' style='width:100%%; font-family:monospace;'>%s</textarea><br><br>
              <input type='submit' value='Save Changes'>
            </form>" 
            id 
            (board-escape-html (or (plist-get post :name) ""))
            (board-escape-html (or (plist-get post :subject) ""))
            (board-escape-html (or (plist-get post :body) ""))))
          (insert "<br><hr><a href='/home'>Cancel</a></body></html>"))))))

(defun board-admin-update-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403)
    (let* ((id (string-to-number (or (board-get-arg args "id") "0")))
           (new-name (board-get-arg args "name"))
           (new-subj (board-get-arg args "subject"))
           (new-body (board-get-arg args "comment")))
      (when (and (> id 0) new-body)
        (let ((found nil))
          (dolist (tt board-threads)
            (let ((op (plist-get tt :op)))
              (if (= (plist-get op :id) id)
                  (progn 
                    (plist-put op :name new-name)
                    (plist-put op :subject new-subj)
                    (plist-put op :body new-body)
                    (setq found t))
                (dolist (r (plist-get tt :replies))
                  (when (= (plist-get r :id) id)
                    (plist-put r :name new-name)
                    (plist-put r :subject new-subj)
                    (plist-put r :body new-body)
                    (setq found t))))))
          (when found (board-save))))
      (httpd-redirect proc "/home"))))

;; --- POST HANDLING ---

(require 'cl-lib) ; Crucial: ensures 'cl-find-if' and 'cl-remove' work

(defun board-handle-post (proc args)
  (let ((ip (board-get-ip proc args)))
    (cond
     ;; 1. Ban Check
     ((member ip board-banned-ips)      
      (with-httpd-buffer proc "text/html"
        (insert "<html><body style='background:black;color:red;text-align:center;padding-top:50px;font-family:sans-serif;'>
                 <h1>BANNED!</h1>
                 <img src='/hello.jpg' style='max-width:800px; border: 5px solid red;'><br>
                 <p style='font-size:1.5em;'>Your IP (" ip ") has been restricted.</p>
                 </body></html>")))

     ;; 2. Rate Limit Check
     ((not (board-check-rate-limit ip (1+ board-post-count)))
      (with-httpd-buffer proc "text/html"
        (insert (render-rate-limit-page ip 60))))

     ;; 3. Valid Post
     (t 
      (let* ((comment (board-get-arg args "comment")) 
             (subj (board-get-arg args "subject")) 
             (tags-raw (board-get-arg args "tags")) 
             (name-raw (or (board-get-arg args "name") "Anonymous"))
             (resto (board-get-arg args "resto")) 
             (is-reply (and resto (not (string-empty-p (string-trim resto))))))
        
        (when (and comment (not (string-empty-p (string-trim comment))))
          (setq board-post-count (1+ board-post-count))
          (let* ((nt (generate-tripcode name-raw))
                 (tags (if is-reply nil 
                         (if (or (null tags-raw) (string-empty-p (string-trim tags-raw))) 
                             '("shitpost") 
                           (mapcar (lambda (s) (downcase (string-trim s))) (split-string tags-raw "," t)))))
                 (new (list :id board-post-count
                            :name (car nt)
                            :trip (cadr nt)
                            :subject (or subj "")
                            :body comment
                            :timestamp (format-time-string "%Y-%m-%d %H:%M:%S")
                            :ip ip
                            :tags tags)))
            
            (if is-reply
                (let* ((tid (string-to-number resto))
                       (thread (cl-find-if (lambda (tt) (= (plist-get (plist-get tt :op) :id) tid)) board-threads)))
                  (when thread
                    (plist-put thread :replies (append (plist-get thread :replies) (list new)))
                    ;; Update list to bump thread
                    (setq board-threads (cons thread (cl-remove thread board-threads :test 'equal)))))
              (push (list :op new :replies nil) board-threads))
            
            (board-save)))
        
        ;; Execution of the redirect
        (let ((target (if is-reply (format "/thread?id=%s" resto) "/home")))
          (httpd-send-header proc "text/html" 302 
                             :Location target 
                             :Set-Cookie (format "preferred_name=%s; Path=/; Max-Age=31536000" 
                                                 (url-hexify-string name-raw)))
          (process-send-string proc "")))))))
;; (defun board-handle-post (proc args)
;;   (let ((ip (board-get-ip proc args)))
;;     (cond
;;      ((member ip board-banned-ips)      
;;       (with-httpd-buffer proc "text/html"
;;         (insert "<html><body style='background:black;color:red;text-align:center;padding-top:50px;font-family:sans-serif;'>")
;;         (insert "<h1>BANNED!</h1>")
;;         (insert "<img src='/hello.jpg' style='max-width:800px; border: 5px solid red;'><br>")
;;         (insert (format "<p style='font-size:1.5em;'>Your IP (%s) has been restricted.</p>" ip))
;;         (insert "</body></html>")))

;;      ((not (board-check-rate-limit ip (1+ board-post-count)))
;;       (with-httpd-buffer proc "text/html"
;;         (insert (render-rate-limit-page ip 60))))

;;      (t 
;;       (let* ((comment (board-get-arg args "comment")) 
;;              (subj (board-get-arg args "subject")) 
;;              (tags-raw (board-get-arg args "tags")) 
;;              (name-raw (or (board-get-arg args "name") "Anonymous"))
;;              (resto (board-get-arg args "resto")) 
;;              (is-reply (and resto (not (string-empty-p (string-trim resto))))))
        
;;         (when (and comment (not (string-empty-p (string-trim comment))))
;;           (setq board-post-count (1+ board-post-count))
;;           (let* ((nt (generate-tripcode name-raw))
;;                  (tags (if is-reply nil 
;;                          (if (or (null tags-raw) (string-empty-p (string-trim tags-raw))) 
;;                              '("shitpost") 
;;                            (mapcar (lambda (s) (downcase (string-trim s))) (split-string tags-raw "," t)))))
;;                  (new (list :id board-post-count
;;                             :name (car nt)
;;                             :trip (cadr nt)
;;                             :subject (or subj "")
;;                             :body comment
;;                             :timestamp (format-time-string "%Y-%m-%d %H:%M:%S")
;;                             :ip ip
;;                             :tags tags)))
            
;;             (if is-reply
;;                 (let* ((tid (string-to-number resto))
;;                        (thread (cl-find-if (lambda (tt) (= (plist-get (plist-get tt :op) :id) tid)) board-threads)))
;;                   (when thread
;;                     (plist-put thread :replies (append (plist-get thread :replies) (list new)))
;;                     (setq board-threads (cons thread (remove thread board-threads)))))
;;               (push (list :op new :replies nil) board-threads))
            
;;             (board-save)))
        
;;         (let ((target (if is-reply (format "/thread?id=%s" resto) "/home")))
;;           (httpd-send-header proc "text/html" 302 
;;                              :Location target 
;;                              :Set-Cookie (format "preferred_name=%s; Path=/; Max-Age=31536000" 
;;                                                  (url-hexify-string name-raw)))
;;           (process-send-string proc "")))))))

(defun board-admin-update-tags-route (proc path query args)
  (if (not (board-is-admin-p proc args)) 
      (httpd-error proc 403)
    (let* ((id (string-to-number (or (board-get-arg args "id") "0")))
           (tags-raw (board-get-arg args "tags"))
           (redir (board-get-arg args "redirect"))
           (thread (cl-find-if (lambda (x) (= (plist-get (plist-get x :op) :id) id)) board-threads)))
      (when thread
        (let* ((op (plist-get thread :op))
               (new-tags (mapcar (lambda (s) (downcase (string-trim s))) 
                                 (split-string tags-raw "," t))))
          (plist-put op :tags new-tags)
          (board-save)))
      (let ((target (if (string= redir "home") "/home" (format "/thread?id=%d" id))))
        (httpd-redirect proc target)))))

(defun board-admin-rename-tag-global-route (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (let* ((old-tag (downcase (string-trim (or (board-get-arg args "old") ""))))
           (new-tag (downcase (string-trim (or (board-get-arg args "new") "")))))
      (unless (or (string-empty-p old-tag) (string-empty-p new-tag))
        (dolist (tt board-threads)
          (let* ((op (plist-get tt :op))
                 (tags (plist-get op :tags)))
            (when (member old-tag tags)
              (setf (plist-get op :tags)
                    (mapcar (lambda (tag) (if (string= tag old-tag) new-tag tag)) tags)))))
        (board-save))
      (httpd-redirect proc (format "/tags?name=%s" (url-hexify-string new-tag))))))

;; MOVED LOGIC TO model.el for cleaner separation, 
;; but keeping this placeholder as a reminder.
(defun board-check-rate-limit (ip post-id)
  (model-check-rate-limit ip post-id))

(defun board-get-ip (proc &optional args)
  (let* ((contact (process-contact proc t))
         (remote (plist-get contact :remote))
         (direct-ip
          (cond
           ((vectorp remote) 
            (let ((addr (format-network-address remote t)))
              (if (string-match "^\\([^:]+\\):" addr) (match-string 1 addr) addr)))
           ((consp remote) (car remote))
           (t "127.0.0.1")))
         (forwarded-ip (and args (board-get-forwarded-ip args))))
    (if (and forwarded-ip (string= direct-ip "127.0.0.1"))
        forwarded-ip
      direct-ip)))

(provide 'controller)
