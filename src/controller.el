;;; controller.el --- Post & Admin Logic
(require 'model)
(require 'cl-lib)

;; --- UTILITY ---

(defun board-get-forwarded-ip (args)
  "Extracts the IP from X-Forwarded-For header if behind a proxy."
  (let ((xff (cadr (assoc "X-Forwarded-For" args))))
    (when xff
      (car (split-string xff ",")))))

(defun board-get-ip (proc &optional args)
  "Determines the user's IP address, accounting for local vs remote connections."
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

(defun board-is-admin-p (proc args)
  "Checks if the current session cookie matches the SHA-256 hash of the admin password."
  (let ((cookie (cadr (assoc "Cookie" args))))
    (and cookie (string-match (format "session=%s" (regexp-quote (secure-hash 'sha256 board-admin-password))) cookie))))

;; --- ADMIN ACTIONS ---

(defun board-admin-dashboard-route (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (with-httpd-buffer proc "text/html"
      (insert (render-header "Admin Dashboard" t))
      (insert-board-ascii)
      (insert "<div style='font-family:monospace; margin-bottom:15px;'>
                 <a href='/admin/stats'>[Stats]</a>
                 <a href='/admin/log' style='margin-left:10px;'>[Traffic Log]</a>
                 <a href='/home' style='margin-left:10px;'>[Back to Board]</a>
               </div>")
      (insert "<h2>Banned IPs</h2>")
      (insert "<table style='width:100%%; font-family:monospace; border-collapse:collapse;'>
                 <tr><th style='text-align:left; color:#888;'>IP</th>
                     <th style='text-align:left; color:#888;'>Actions</th></tr>")
      (if board-banned-ips
          (dolist (ip board-banned-ips)
            (insert (format "<tr><td style='padding:4px 8px;'>%s</td>
                                 <td><a href='/admin/unban?ip=%s'>[Unban]</a>
                                     <a href='/admin/deletebyip?ip=%s' style='margin-left:8px; color:#f44;'
                                        onclick='return confirm(\"Delete all posts from %s?\")'>
                                       [Delete All Posts]</a></td></tr>"
                            ip ip ip ip)))
        (insert "<tr><td colspan='2' style='padding:4px 8px; color:#666;'>No bans.</td></tr>"))
      (insert "</table>")
      (insert (render-footer)))))

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

;; --- EDIT & UPDATE ---

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
           (new-body (board-get-arg args "comment"))
           (tags-raw (board-get-arg args "tags"))
           (new-tags (when (and tags-raw (not (string-empty-p (string-trim tags-raw))))
                       (mapcar (lambda (s) (downcase (string-trim s)))
                               (split-string tags-raw "," t)))))
      (when (and (> id 0) new-body)
        (let ((found nil))
          (dolist (tt board-threads)
            (let ((op (plist-get tt :op)))
              (if (= (plist-get op :id) id)
                  (progn
                    (plist-put op :name new-name)
                    (plist-put op :subject new-subj)
                    (plist-put op :body new-body)
                    (when new-tags (plist-put op :tags new-tags))
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

(defun board-handle-post (proc args)
  (let ((ip (board-get-ip proc args)))
    (cond
     ((member ip board-banned-ips)
      (with-httpd-buffer proc "text/html"
        (insert "<html><body style='background:black;color:red;text-align:center;padding-top:50px;font-family:sans-serif;'>
                 <h1>BANNED!</h1>
                 <p style='font-size:1.5em;'>Your IP (" ip ") has been restricted.</p>
                 </body></html>")))
     (t
      (let* ((comment (board-get-arg args "comment"))
             (subj (board-get-arg args "subject"))
             (tags-raw (board-get-arg args "tags"))
             (name-raw (or (board-get-arg args "name") "Anonymous"))
             (resto (board-get-arg args "resto"))
             (is-reply (and resto (not (string-empty-p (string-trim resto)))))
             (is-sage (string= (or (board-get-arg args "sage") "") "1")))

        ;; Refuse replies to locked threads
        (when is-reply
          (let* ((tid (string-to-number (string-trim resto)))
                 (thread (cl-find-if (lambda (tt) (= (plist-get (plist-get tt :op) :id) tid)) board-threads)))
            (when (and thread (plist-get thread :locked))
              (httpd-send-header proc "text/html" 302
                                 :Location (format "/threads/%s?error=locked" (string-trim resto)))
              (process-send-string proc "")
              (cl-return-from board-handle-post nil))))

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
                (let* ((tid (string-to-number (string-trim resto)))
                       (thread (cl-find-if (lambda (tt) (= (plist-get (plist-get tt :op) :id) tid)) board-threads)))
                  (when thread
                    (plist-put thread :replies (append (plist-get thread :replies) (list new)))
                    (unless is-sage
                      (setq board-threads (cons thread (cl-remove thread board-threads :test 'equal))))))
              (push (list :op new :replies nil) board-threads))

            (board-save)))

        (let ((target (if is-reply (format "/threads/%s" (string-trim resto)) "/home")))
          (httpd-send-header proc "text/html" 302
                             :Location target
                             :Set-Cookie (format "preferred_name=%s; Path=/; Max-Age=31536000"
                                                 (url-hexify-string name-raw)))
          (process-send-string proc "")))))))

(defun board-tags-route (proc path query args)
  (let* ((path-parts (cl-remove-if #'string-empty-p (split-string path "/")))
         ;; Extract tag from URL path: /tags/NAME → NAME
         (tag-encoded (nth 1 path-parts))
         (tag-name (if tag-encoded
                       (decode-coding-string (url-unhex-string tag-encoded) 'utf-8)
                     ""))
         (is-admin (board-is-admin-p proc args))
         (threads-per-page 10)
         (page-param (cadr (assoc "page" query)))
         (page (if page-param (string-to-number page-param) 1)))
    (with-httpd-buffer proc "text/html"
      (insert (render-header (if (string-empty-p tag-name) "all tags" (format "Tag: %s" tag-name))
                             is-admin
                             (when (not (string-empty-p tag-name))
                               (format "/tags/rss?name=%s" (url-hexify-string tag-name)))))
      (insert-board-ascii)

      (if (string-empty-p tag-name)
          (let ((all-tags (get-all-board-tags board-threads)))
            (insert "<div><a href='/home' class='nav-link'>[Back]</a></div><hr>")
            (insert (render-tag-index-page all-tags is-admin)))

        (let* ((filtered (cl-remove-if-not
                          (lambda (tt) (member (downcase tag-name) (plist-get (plist-get tt :op) :tags)))
                          board-threads))
               (total-threads (length filtered))
               (total-pages (ceiling (/ (float (max 1 total-threads)) threads-per-page)))
               (max-pages (min total-pages 10)))
          (setq page (max 1 (min page max-pages)))
          (let* ((start (* (1- page) threads-per-page))
                 (end (min total-threads (+ start threads-per-page)))
                 (page-threads (when filtered (cl-subseq filtered start end))))
            (insert "<div><a href='/home' class='nav-link'>[Back]</a></div><hr>")
            (insert (format "<h2>tag: %s</h2>" (board-escape-html tag-name)))

            (when is-admin
              (insert (format "
              <div class='admin-rename-box' style='background:#1a1a1a; padding:10px; border:1px solid #444; margin-bottom:20px;'>
                <form method='POST' action='/admin/rename-tag-global'>
                  <input type='hidden' name='old' value='%s'>
                  <b>Admin:</b> Rename this tag globally:
                  <input name='new' placeholder='new name' style='background:#000; color:#fff; border:1px solid #333;'>
                  <input type='submit' value='Apply'>
                </form>
              </div>" (board-escape-html tag-name))))

            (if page-threads
                (dolist (tt page-threads) (insert (render-thread-html tt nil is-admin)))
              (insert "<p class='greentext'>No posts found with this tag.</p>"))

            (when (> max-pages 1)
              (insert "<div class='pagination'>Pages: ")
              (dotimes (i max-pages)
                (let ((p (1+ i)))
                  (insert (if (= p page)
                              (format "<b>[%d]</b> " p)
                            (format "<a href='/tags/%s?page=%d'>[%d]</a> "
                                    (url-hexify-string tag-name) p p)))))
              (insert "</div>")))))

      (insert (render-footer)))))

;; --- THREAD LOCK / STICKY ---

(defun board-admin-lock-route (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (let* ((id (string-to-number (or (cadr (assoc "id" query)) "0")))
           (thread (cl-find-if (lambda (tt) (= (plist-get (plist-get tt :op) :id) id)) board-threads))
           (back (or (cadr (assoc "Referer" args)) (format "/threads/%d" id))))
      (when thread
        (plist-put thread :locked (not (plist-get thread :locked)))
        (board-save))
      (httpd-redirect proc back))))

(defun board-admin-sticky-route (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (let* ((id (string-to-number (or (cadr (assoc "id" query)) "0")))
           (thread (cl-find-if (lambda (tt) (= (plist-get (plist-get tt :op) :id) id)) board-threads))
           (back (or (cadr (assoc "Referer" args)) "/home")))
      (when thread
        (plist-put thread :sticky (not (plist-get thread :sticky)))
        (board-save))
      (httpd-redirect proc back))))

;; --- BULK DELETE BY IP ---

(defun board-admin-deletebyip-route (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (let ((ip (cadr (assoc "ip" query))))
      (when ip
        ;; Remove threads where OP is from this IP
        (setq board-threads
              (cl-remove-if (lambda (tt) (string= (plist-get (plist-get tt :op) :ip) ip))
                            board-threads))
        ;; Remove replies from this IP in remaining threads
        (dolist (tt board-threads)
          (plist-put tt :replies
                     (cl-remove-if (lambda (r) (string= (plist-get r :ip) ip))
                                   (plist-get tt :replies))))
        (board-save))
      (httpd-redirect proc "/admin/dashboard"))))

;; --- STATS PAGE ---

(defun board-admin-stats-route (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (let* ((total-threads (length board-threads))
           (total-replies (apply #'+ (mapcar (lambda (tt) (length (plist-get tt :replies))) board-threads)))
           (total-posts (+ total-threads total-replies))
           (now (float-time))
           (one-hour-ago (- now 3600))
           (posts-last-hour (length (cl-remove-if (lambda (e) (< (nth 1 e) one-hour-ago)) board-post-log)))
           (all-tags (get-all-board-tags board-threads))
           (tag-counts (mapcar (lambda (tag)
                                 (cons tag (length (cl-remove-if-not
                                                    (lambda (tt) (member tag (plist-get (plist-get tt :op) :tags)))
                                                    board-threads))))
                               all-tags))
           (sorted-tags (sort tag-counts (lambda (a b) (> (cdr a) (cdr b))))))
      (with-httpd-buffer proc "text/html"
        (insert (render-header "Board Stats" t))
        (insert-board-ascii)
        (insert "<h2>Board Statistics</h2>")
        (insert (format "<div class='thread-container' style='padding:15px;'>
          <table style='width:100%%; font-family:monospace; border-collapse:collapse;'>
            <tr><td style='padding:4px 10px; color:#888;'>Total threads</td><td><b>%d</b></td></tr>
            <tr><td style='padding:4px 10px; color:#888;'>Total replies</td><td><b>%d</b></td></tr>
            <tr><td style='padding:4px 10px; color:#888;'>Total posts</td><td><b>%d</b></td></tr>
            <tr><td style='padding:4px 10px; color:#888;'>Posts (last hour)</td><td><b>%d</b></td></tr>
            <tr><td style='padding:4px 10px; color:#888;'>Unique tags</td><td><b>%d</b></td></tr>
            <tr><td style='padding:4px 10px; color:#888;'>Banned IPs</td><td><b>%d</b></td></tr>
          </table></div>"
                        total-threads total-replies total-posts posts-last-hour
                        (length all-tags) (length board-banned-ips)))
        (insert "<h3>Top Tags</h3><div class='thread-container' style='padding:15px;'>")
        (dolist (tc (seq-take sorted-tags 20))
          (insert (format "<a href='/tags/%s' class='tag'>%s</a> <span style='color:#666;'>(%d)</span>  "
                          (url-hexify-string (car tc)) (car tc) (cdr tc))))
        (insert "</div>")
        (insert (render-footer))))))

(provide 'controller)
