;;; main.el --- Server Entry Point
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'simple-httpd) (package-refresh-contents) (package-install 'simple-httpd))

(setq board-root (file-name-directory (or load-file-name buffer-file-name default-directory)))
(add-to-list 'load-path (expand-file-name "src" board-root))

(require 'simple-httpd) (require 'model) (require 'view) (require 'controller)

(defvar board-admin-password 
  (let ((pwd-file (expand-file-name "data/password" board-root)))
    (if (file-exists-p pwd-file)
        (with-temp-buffer
          (insert-file-contents pwd-file)

          (goto-char (point-min))
          (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      "no_file")))

(setq httpd-port 8080)

(defun board-paginate (items page per-page)
  "Return sublist of ITEMS for PAGE (1-based)."
  (let* ((page (max 1 page))
         (start (* per-page (1- page)))
         (end (min (length items) (+ start per-page))))
    (cl-subseq items start end)))

;; --- ASCII ART HELPER ---
(defun insert-board-ascii ()
  (insert (format "<pre class='board-ascii'>%s</pre>" board-ascii-art)))

(defvar board-ascii-art "
* g o a t s e x * g o a t s e x * g o a t s e x * 
g                                               g  
o /     \\             \\            /    \\       o  
a|       |             \\          |      |      a  
t|       `.             |         |       :     t  
s`        |             |        \\|       |     s  
e \\       | /       /  \\\\\\   --__ \\\\       :    e  
x  \\      \\/   _--~~          ~--__| \\     |    x  
*   \\      \\_-~                    ~-_\\    |    *  
g    \\_     \\        _.--------.______\\|   |    g  
o      \\     \\______// _ ___ _ (_(__>  \\   |    o  
a       \\   .  C ___)  ______ (_(____>  |  /    a  
t       /\\ |   C ____)/      \\ (_____>  |_/     t  
s      / /\\|   C_____)       |  (___>   /  \\    s  
e     |   (   _C_____)\\______/  // _/ /     \\   e  
x     |    \\  |__   \\\\_________// (__/       |  x  
*    | \\    \\____)   `----   --'             |  *  
g    |  \\_          ___\\       /_          _/ | g  
o   |              /    |     |  \\            | o  
a   |             |    /       \\  \\           | a  
t   |          / /    |         |  \\           |t  
s   |         / /      \\__/\\___/    |          |s  
e  |         / /        |    |       |         |e  
x  |          |         |    |       |         |x  
* g o a t s e x * g o a t s e x * g o a t s e x * 
")

;; --- HELPERS ---
;; (defun board-get-arg (args key)
;;   (let* ((cb (cadr (assoc "Content" args))) (raw (if (stringp cb) cb "")) (re (concat "\\(?:^\\|&\\)" (if (symbolp key) (symbol-name key) key) "=\\([^&]*\\)")) (val (when (string-match re raw) (match-string 1 raw))))
;;     (if val (url-unhex-string (replace-regexp-in-string "\\+" " " val)) nil)))
(defun board-get-arg (args key)
  (let* ((post-data (plist-get args :post-data))
         (content-assoc (cadr (assoc "Content" args)))
         (raw (or post-data content-assoc ""))
         (re (concat "\\(?:^\\|&\\)" (regexp-quote (if (symbolp key) (symbol-name key) key)) "=\\([^&]*\\)"))
         (val (when (string-match re raw) (match-string 1 raw))))
    (if (not val)
        nil
      (let ((result val))

        (setq result (replace-regexp-in-string "\\+" " " result))

        (setq result (replace-regexp-in-string "%0D%0A" "\n" result))
        (setq result (replace-regexp-in-string "%0A" "\n" result))
        (setq result (replace-regexp-in-string "%0D" "\n" result))
	
        (setq result (url-unhex-string result))
        result))))

(defun board-get-cookie-name (args)
  (let* ((cookie-header (cadr (assoc "Cookie" args)))
         (val (when (and cookie-header (string-match "preferred_name=\\([^; ]+\\)" cookie-header))
                (match-string 1 cookie-header))))
    (if val (url-unhex-string val) "")))

;; --- STATIC SERVING ---
(defun httpd/public (proc path query args)
  (let* ((relative-path (substring path 1))
         (full-path (expand-file-name relative-path board-root))
         (extension (file-name-extension full-path))
         (content-type (cond ((string= extension "css") "text/css")
                             ((string= extension "js") "application/javascript")
                             ((member extension '("jpg" "jpeg")) "image/jpeg")
                             ((string= extension "png") "image/png")
                             (t "text/plain"))))
    (if (and (file-exists-p full-path) (not (file-directory-p full-path)))
        (with-httpd-buffer proc content-type (insert-file-contents full-path))
      (httpd-error proc 404))))

;; --- ROUTES ---
(defun httpd/home (proc path query args)
  (let* ((admin (board-is-admin-p proc args))
         (remembered-name (board-get-cookie-name args))

         (error-msg (cadr (assoc "error" query)))
         (threads-per-page 10)
         (total-threads (length board-threads))
 
         (recent-tags (let ((all-tags nil))
                        (dolist (tt board-threads)
                          (setq all-tags (append (plist-get (plist-get tt :op) :tags) all-tags)))
                        (seq-take (delete-dups (reverse all-tags)) 10)))
         (total-pages (ceiling (/ (float total-threads) threads-per-page)))
         (max-pages (min total-pages 10))
         (page-param (cadr (assoc "page" query)))
         (page (if page-param (string-to-number page-param) 1)))
    (setq page (max 1 (min page max-pages)))
    (let* ((start (* (- page 1) threads-per-page))
           (end (min total-threads (+ start threads-per-page)))
           (page-threads (if (<= page max-pages) (cl-subseq board-threads start end) nil)))
      (with-httpd-buffer proc "text/html"
        (insert (render-header "goatse.world" admin "/rss"))
        (insert-board-ascii)
        (insert (render-tag-overview recent-tags))

        (when (string= error-msg "ratelimit")
          (insert "<div style='color: #ff4444; border: 1px solid #ff4444; padding: 10px; 
                         margin: 10px auto; width: 345px; text-align: center; 
                         font-family: monospace; background: #1a0000;'>
                    [!] RATE LIMIT REACHED<br>
                    <span style='font-size:0.8em;'>Max 5 posts per hour. Please wait.</span>
                  </div>"))
        
        (insert (format "<h3></h3><form method='POST' action='/post-entry'>
                          <input name='name' placeholder='Name#Trip' value='%s'>
                          <input name='subject' placeholder='Subject'><br>
                          <input name='tags' placeholder='Tags' style='width:345px;margin-top:5px;'><br>
                          <textarea name='comment' rows='4' style='width:345px;margin-top:5px;'></textarea><br>
                          <input type='submit' value='Create New Thread'>
                        </form><hr>" (board-escape-html remembered-name)))

        (if page-threads
            (dolist (tt page-threads) (insert (render-thread-html tt nil admin)))
          (insert "<div><a href='/home'>[Back]</a></div><hr>Not found"))

        (when (> max-pages 1)
          (insert "<div class='pagination'>Pages: ")
          (dotimes (i max-pages)
            (let ((p (1+ i)))
              (insert (if (= p page)
                          (format "<b>[%d]</b> " p)
                        (format "<a href='/home?page=%d'>[%d]</a> " p p)))))
          (insert "</div>"))
        (insert (render-footer))))))

;; (defun httpd/home (proc path query args)
;;   (let* ((admin (board-is-admin-p proc args))
;;          (remembered-name (board-get-cookie-name args))
;;          (threads-per-page 10)
;;          (total-threads (length board-threads))
;;          (recent-tags (let ((all-tags nil))
;;                         (dolist (tt board-threads)
;;                           (setq all-tags (append (plist-get (plist-get tt :op) :tags) all-tags)))
;;                         (seq-take (delete-dups (reverse all-tags)) 25)))
;;          (total-pages (ceiling (/ (float total-threads) threads-per-page)))
;;          (max-pages (min total-pages 10))
;;          (page-param (cadr (assoc "page" query)))
;;          (page (if page-param (string-to-number page-param) 1)))
;;     (setq page (max 1 (min page max-pages)))
;;     (let* ((start (* (- page 1) threads-per-page))
;;            (end (min total-threads (+ start threads-per-page)))
;;            (page-threads (if (<= page max-pages) (cl-subseq board-threads start end) nil)))
;;       (with-httpd-buffer proc "text/html"
;;         (insert (render-header "goatse.world" admin "/rss"))
;;         (insert-board-ascii)
;;         (insert (render-tag-overview recent-tags))
        
;;         (insert (format "<h3></h3><form method='POST' action='/post-entry'>
;;                           <input name='name' placeholder='Name#Trip' value='%s'>
;;                           <input name='subject' placeholder='Subject'><br>
;;                           <input name='tags' placeholder='Tags' style='width:345px;margin-top:5px;'><br>
;;                           <textarea name='comment' rows='4' style='width:345px;margin-top:5px;'></textarea><br>
;;                           <input type='submit' value='Create New Thread'>
;;                         </form><hr>" (board-escape-html remembered-name)))
;;         (if page-threads
;;             (dolist (tt page-threads) (insert (render-thread-html tt nil admin)))
;;           (insert "<div><a href='/home'>[Back]</a></div><hr>Not found"))
;;         (when (> max-pages 1)
;;           (insert "<div class='pagination'>Pages: ")
;;           (dotimes (i max-pages)
;;             (let ((p (1+ i)))
;;               (insert (if (= p page)
;;                           (format "<b>[%d]</b> " p)
;;                         (format "<a href='/home?page=%d'>[%d]</a> " p p)))))
;;           (insert "</div>"))
;;         (insert (render-footer))))))

(defun httpd/thread (proc path query args)
  (let* ((id-param (cadr (assoc "id" query)))
         (id (if id-param (string-to-number id-param) 0))
         (admin (board-is-admin-p proc args)) 
         (remembered-name (board-get-cookie-name args))
         (tt (cl-find-if (lambda (x) (= (plist-get (plist-get x :op) :id) id)) board-threads))
         (op-subject (when tt (plist-get (plist-get tt :op) :subject)))
         (page-title (if (and op-subject (not (string-empty-p (string-trim op-subject))))
                         op-subject
                       "no subject")))
    (with-httpd-buffer proc "text/html" 
      (insert (render-header page-title admin (format "/thread/rss?id=%d" id)))
      (insert-board-ascii)
      (insert (format "<div><a href='/home'>[Back]</a></div><hr><form method='POST' action='/post-entry'><input type='hidden' name='resto' value='%d'><input name='name' placeholder='Name#Trip' value='%s' style='margin-bottom:5px;'><br><textarea name='comment' rows='4' style='width:345px;'></textarea><br><input type='submit' value='Reply'></form><hr>" id (board-escape-html remembered-name)))
      (if tt (insert (render-thread-html tt t admin)) (insert "Not found")) 
      (insert (render-footer)))))

;; --- TAGS & TAG RSS ---

(defun httpd/tags (proc path query args)
  "Render threads filtered by tag with pagination, max 10 pages."
  (let* ((tag-raw (cadr (assoc "name" query)))
         (tag-name (if tag-raw (url-unhex-string tag-raw) ""))
         (admin (board-is-admin-p proc args))
         (threads-per-page 10)
         (filtered (cl-remove-if-not
                    (lambda (tt) (member (downcase tag-name) (plist-get (plist-get tt :op) :tags)))
                    board-threads))
         (total-threads (length filtered))
         (total-pages (ceiling (/ (float total-threads) threads-per-page)))
         (max-pages (min total-pages 10))
         (page-param (cadr (assoc "page" query)))
         (page (if page-param (string-to-number page-param) 1)))
    (setq page (max 1 (min page max-pages)))
    (let* ((start (* (- page 1) threads-per-page))
           (end (min total-threads (+ start threads-per-page)))
           (page-threads (if (<= page max-pages) (cl-subseq filtered start end) nil)))
      (with-httpd-buffer proc "text/html"
        (insert (render-header (format "Tag: %s" tag-name) admin (format "/tags/rss?name=%s" (url-hexify-string tag-name))))
        (insert-board-ascii)
        (insert "<a href='/home'>[Back]</a><hr>")
        (insert (format "<h2>Tag: %s</h2>" tag-name))
        (when admin
          (insert (format "
            <div class='admin-rename-box' style='background:#1a1a1a; padding:10px; border:1px solid red; margin-bottom:20px;'>
              <form method='POST' action='/admin/rename-tag-global'>
                <input type='hidden' name='old' value='%s'>
                Admin: Rename this tag globally to: 
                <input name='new' placeholder='new tag name' style='background:#000; color:#fff; border:1px solid #444;'>
                <input type='submit' value='Apply to All Threads'>
              </form>
            </div>" tag-name)))
        (if page-threads
            (dolist (tt page-threads) (insert (render-thread-html tt nil admin)))
          (insert "<hr>Not found"))
        (when (> max-pages 1)
          (insert "<div class='pagination'>Pages: ")
          (dotimes (i max-pages)
            (let ((p (1+ i)))
              (insert (if (= p page)
                          (format "<b>[%d]</b> " p)
                        (format "<a href='/tags?name=%s&page=%d'>[%d]</a> "
                                (url-hexify-string tag-name) p p)))))
          (insert "</div>"))
        (insert (render-footer))))))




;; (defun httpd/tags (proc path query args)
;;   (let* ((tag-raw (cadr (assoc "name" query)))
;;          (tag-name (if tag-raw (url-unhex-string tag-raw) ""))
;;          (admin (board-is-admin-p proc args))
;;          (tag-rss-url (format "/tags/rss?name=%s" (url-hexify-string tag-name)))
;;          (filtered (cl-remove-if-not 
;;                     (lambda (tt) (member (downcase tag-name) (plist-get (plist-get tt :op) :tags))) 
;;                     board-threads)))
;;     (with-httpd-buffer proc "text/html"
;;       (insert (render-header (format "Tag: %s" tag-name) admin tag-rss-url))
;;       (insert-board-ascii)
;;       (insert (format "<h2>Tagged: %s</h2>" tag-name))
      
;;       ;; --- ADMIN RENAME FORM ---
;;       (when admin
;;         (insert (format "
;;           <div class='admin-rename-box' style='background:#1a1a1a; padding:10px; border:1px solid red; margin-bottom:20px;'>
;;             <form method='POST' action='/admin/rename-tag-global'>
;;               <input type='hidden' name='old' value='%s'>
;;               Admin: Rename this tag globally to: 
;;               <input name='new' placeholder='new tag name' style='background:#000; color:#fff; border:1px solid #444;'>
;;               <input type='submit' value='Apply to All Threads'>
;;             </form>
;;           </div>" tag-name)))

;;       (insert "<a href='/home'>[Back]</a><hr>")
;;       (if filtered
;;           (dolist (tt filtered) (insert (render-thread-html tt nil admin)))
;;         (insert "<p>No threads found with this tag.</p>"))
;;       (insert "</body></html>"))))

;; (defun httpd/tags (proc path query args)
;;   (let* ((tag-raw (cadr (assoc "name" query)))
;;          (tag-name (if tag-raw (url-unhex-string tag-raw) ""))
;;          (admin (board-is-admin-p proc args))
;;          (tag-rss-url (format "/tags/rss?name=%s" (url-hexify-string tag-name)))
;;          (filtered (cl-remove-if-not 
;;                     (lambda (tt) (member (downcase tag-name) (plist-get (plist-get tt :op) :tags))) 
;;                     board-threads)))
;;     (with-httpd-buffer proc "text/html"
;;       (insert (render-header (format "Tag: %s" tag-name) admin tag-rss-url))
;;       (insert-board-ascii)
;;       (insert (format "<h2>Tag: %s</h2><a href='/home'>[Back]</a><hr>" tag-name))
;;       (if filtered
;;           (dolist (tt filtered) (insert (render-thread-html tt nil admin)))
;;         (insert "<p>No threads found with this tag.</p>"))
;;       (insert "</body></html>"))))

(defun httpd/tags/rss (proc path query args)
  (let* ((tag-raw (cadr (assoc "name" query)))
         (tag (if tag-raw (downcase (url-unhex-string tag-raw)) ""))
         (filtered (cl-remove-if-not 
                    (lambda (tt) (member tag (plist-get (plist-get tt :op) :tags))) 
                    board-threads)))
    (with-httpd-buffer proc "application/rss+xml"
      (insert (render-rss-feed filtered (format "goatse.world - Tag: %s" tag))))))

(defun httpd/rss (proc path query args)
  "Global RSS feed."
  (with-httpd-buffer proc "application/rss+xml"
    (insert (render-rss-feed board-threads "goatse.world main feed"))))

(defun httpd/thread/rss (proc path query args)
  "Thread-specific RSS feed."
  (let* ((id (string-to-number (or (cadr (assoc "id" query)) "0")))
         (tt (cl-find-if (lambda (x) (= (plist-get (plist-get x :op) :id) id)) board-threads)))
    (if tt
        (with-httpd-buffer proc "application/rss+xml"
          (insert (render-thread-rss tt)))
      (httpd-error proc 404))))

(defun httpd/admin/log (proc path query args)
  (if (not (board-is-admin-p proc args))
      (httpd-error proc 403)
    (with-httpd-buffer proc "text/html"
      (insert (render-header "Traffic Log" t))
      (insert "<h2>Recent Activity (1hr)</h2>")
      (insert "<table style='width:100%; font-family:monospace; background:#111; color:#eee;'>
               <tr><th>IP</th><th>Post ID</th><th>Action</th></tr>")
      (dolist (entry board-post-log)
        (insert (format "<tr><td>%s</td><td>#%s</td><td><a href='/admin/ban?ip=%s'>[BAN]</a></td></tr>" 
                        (car entry) (nth 2 entry) (car entry))))
      (insert "</table><br><a href='/admin/clear-log'>[Emergency Clear All Limits]</a>"))))

(defun httpd/admin/clear-log (proc path query args)
  (when (board-is-admin-p proc args)
    (setq board-post-log nil)
    (board-save)
    (httpd-redirect proc "/admin/log")))

(httpd-def-route "/admin/log" 'httpd/admin/log)
(httpd-def-route "/admin/clear-log" 'httpd/admin/clear-log)

;; --- ADMIN ROUTES ---

(defun httpd/admin/dashboard (proc path query args) (board-admin-dashboard-route proc path query args))
(defun httpd/admin/delete (proc path query args) (board-admin-delete-route proc path query args))
(defun httpd/admin/ban (proc path query args) (board-admin-ban-route proc path query args))
(defun httpd/admin/unban (proc path query args) (board-admin-unban-route proc path query args))
(defun httpd/admin/logout (proc path query args) (board-admin-logout-route proc path query args))
(defun httpd/admin/update-tags (proc path query args) (board-admin-update-tags-route proc path query args))
(defun httpd/admin/rename-tag-global (proc path query args) 
  (board-admin-rename-tag-global-route proc path query args))
(defun httpd/admin/edit (proc path query args) 
  (board-admin-edit-route proc path query args))
(defun httpd/admin/update (proc path query args) 
  (board-admin-update-route proc path query args))

(defun httpd/login (proc path query args) (with-httpd-buffer proc "text/html" (insert-board-ascii) (insert "<h2>Admin</h2><form method='POST' action='/admin/auth'><input type='password' name='pwd'><input type='submit'></form>")))
(defun httpd/admin/auth (proc path query args) (let ((pwd (board-get-arg args "pwd"))) (if (string= pwd board-admin-password) (progn (httpd-send-header proc "text/html" 302 :Location "/home" :Set-Cookie (format "session=%s; Path=/; HttpOnly" board-admin-password)) (process-send-string proc "")) (httpd-error proc 403))))

(defvar board-post-log nil "List of (ip . timestamp) for rate limiting.")
(defun httpd/post-entry (proc path query args)
  (let* ((ip (car (process-contact proc)))
         (admin (board-is-admin-p proc args)))
    (if (or admin (board-check-rate-limit ip))
        (board-handle-post proc args)
      ;; RATE LIMIT HIT: Redirect back to home with an error flag
      (httpd-send-header proc "text/html" 302 :Location "/home?error=ratelimit")
      (process-send-string proc ""))))

;; (defun httpd/post-entry (proc path query args) (board-handle-post proc args))
(defun httpd/ (proc path query args) (httpd-redirect proc "/home"))



(board-load) (httpd-start)
(princ (format "\nBoard active at: http://localhost:%d/home\n" httpd-port))
(while t (accept-process-output nil 1))
