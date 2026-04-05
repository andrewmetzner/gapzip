;;; main.el --- Server Entry Point
(require 'package)
(require 'cl-lib) ; Added to fix cl- function errors

(setq package-archives '(("melpa" . "https://melpa.org/packages/") ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'simple-httpd) (package-refresh-contents) (package-install 'simple-httpd))

(setq board-root (file-name-directory (or load-file-name buffer-file-name default-directory)))
(add-to-list 'load-path (expand-file-name "src" board-root))

(require 'simple-httpd) 
(require 'model) 
(require 'view) 
(require 'controller)

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

(defun insert-board-ascii ()
  (insert (format "<pre class='board-ascii'>%s</pre>" board-ascii-art)))

;; --- HELPERS ---
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
        (setq result (decode-coding-string (url-unhex-string result) 'utf-8))
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
    (let* ((sorted-threads (append (cl-remove-if-not (lambda (tt) (plist-get tt :sticky)) board-threads)
                                   (cl-remove-if (lambda (tt) (plist-get tt :sticky)) board-threads)))
           (start (* (- page 1) threads-per-page))
           (end (min total-threads (+ start threads-per-page)))
           (page-threads (if (<= page max-pages) (cl-subseq sorted-threads start end) nil)))
      (with-httpd-buffer proc "text/html"
        (insert (render-header "goatse.world" admin "/rss"))
        (insert-board-ascii)
        
        (when (string= error-msg "ratelimit")
          (insert (render-rate-limit-box (board-get-ip proc args))))

        (insert (render-tag-overview recent-tags))

        (insert (format "<h3>New Thread</h3>
                        <form method='POST' action='/post-entry'>
                          <input name='name' placeholder='Name#Trip' value='%s'>
                          <input name='subject' placeholder='Subject'><br>
                          <input name='tags' placeholder='Tags (comma separated)' style='width:345px;margin-top:5px;'><br>
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

;; /thread?id=X kept for backward compat - redirects to /threads/X
(defun httpd/thread (proc path query args)
  (let ((id-param (cadr (assoc "id" query))))
    (httpd-redirect proc (if id-param (format "/threads/%s" id-param) "/home"))))

(defun httpd/threads (proc path query args)
  (let* ((path-parts (cl-remove-if #'string-empty-p (split-string path "/")))
         (id-str (nth 1 path-parts))
         (id (string-to-number (or id-str "0")))
         (error-msg (cadr (assoc "error" query)))
         (admin (board-is-admin-p proc args))
         (remembered-name (board-get-cookie-name args))
         (tt (cl-find-if (lambda (x) (= (plist-get (plist-get x :op) :id) id)) board-threads))
         (is-locked (and tt (plist-get tt :locked)))
         (op-subject (when tt (plist-get (plist-get tt :op) :subject)))
         (page-title (if (and op-subject (not (string-empty-p (string-trim op-subject))))
                         op-subject
                       (format "Thread #%d" id))))
    (with-httpd-buffer proc "text/html"
      (insert (render-header page-title admin (format "/threads/rss?id=%d" id)))
      (insert-board-ascii)

      (when (string= error-msg "ratelimit")
        (insert (render-rate-limit-box (board-get-ip proc args))))

      (when (string= error-msg "locked")
        (insert "<div style='color:#f44; border:1px solid #f44; padding:8px; margin:8px auto;
                              width:345px; text-align:center; font-family:monospace; background:#1a0000;'>
                   <b>[!] THREAD IS LOCKED</b>
                 </div>"))

      (insert (format "<div><a href='/home'>[Back]</a></div><hr>" ))

      (if is-locked
          (insert "<div style='color:#888; font-family:monospace; margin:8px 0;'>[Thread locked — no new replies]</div><hr>")
        (insert (format "<form method='POST' action='/post-entry'>
                           <input type='hidden' name='resto' value='%d'>
                           <input name='name' placeholder='Name#Trip' value='%s' style='margin-bottom:5px;'><br>
                           <textarea name='comment' rows='4' style='width:345px;'></textarea><br>
                           <label style='font-size:0.85em; color:#888;'>
                             <input type='checkbox' name='sage' value='1'> sage (no bump)
                           </label><br>
                           <input type='submit' value='Reply'>
                         </form><hr>"
                        id (board-escape-html remembered-name))))

      (if tt
          (insert (render-thread-html tt t admin))
        (insert "<p class='greentext'>Thread not found.</p>"))
      (insert (render-footer)))))

;; --- TAGS & TAG RSS ---
(defun httpd/tags/rss (proc path query args)
  (let* ((tag-raw (cadr (assoc "name" query)))
         (tag (if tag-raw (downcase (decode-coding-string (url-unhex-string tag-raw) 'utf-8)) ""))
         (filtered (cl-remove-if-not
                    (lambda (tt) (member tag (plist-get (plist-get tt :op) :tags)))
                    board-threads)))
    (with-httpd-buffer proc "application/rss+xml"
      (insert (render-rss-feed filtered (format "goatse.world - Tag: %s" tag))))))

(defun httpd/rss (proc path query args)
  (with-httpd-buffer proc "application/rss+xml"
    (insert (render-rss-feed board-threads "goatse.world main feed"))))

;; /thread/rss kept for backward compat
(defun httpd/thread/rss (proc path query args)
  (let ((id (cadr (assoc "id" query))))
    (httpd-redirect proc (if id (format "/threads/rss?id=%s" id) "/rss"))))

(defun httpd/threads/rss (proc path query args)
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

;; --- ROUTE REGISTRATION ---


(defun httpd/admin/dashboard (proc path query args) (board-admin-dashboard-route proc path query args))
(defun httpd/admin/delete (proc path query args) (board-admin-delete-route proc path query args))
(defun httpd/admin/ban (proc path query args) (board-admin-ban-route proc path query args))
(defun httpd/admin/unban (proc path query args) (board-admin-unban-route proc path query args))
(defun httpd/admin/logout (proc path query args) (board-admin-logout-route proc path query args))
(defun httpd/admin/update-tags (proc path query args) (board-admin-update-tags-route proc path query args))
(defun httpd/admin/rename-tag-global (proc path query args) (board-admin-rename-tag-global-route proc path query args))
(defun httpd/admin/edit (proc path query args) (board-admin-edit-route proc path query args))
(defun httpd/admin/update (proc path query args) (board-admin-update-route proc path query args))
(defun httpd/admin/lock (proc path query args) (board-admin-lock-route proc path query args))
(defun httpd/admin/sticky (proc path query args) (board-admin-sticky-route proc path query args))
(defun httpd/admin/deletebyip (proc path query args) (board-admin-deletebyip-route proc path query args))
(defun httpd/admin/stats (proc path query args) (board-admin-stats-route proc path query args))

(defun httpd/tags (proc path query args)
  (board-tags-route proc path query args))

(defun httpd/login (proc path query args)
  (let ((failed (cadr (assoc "error" query))))
    (with-httpd-buffer proc "text/html"
      (insert (render-header "Admin Login" nil))
      (insert-board-ascii)
      (insert (format "<div style='text-align:center; margin-top:30px;'>
                         <h2>Admin</h2>
                         %s
                         <form method='POST' action='/admin/auth'>
                           <input type='password' name='pwd' placeholder='password'
                                  style='display:block; margin:8px auto; width:180px;'><br>
                           <input type='submit' value='Login'>
                         </form>
                       </div>"
                      (if failed "<p style='color:#f44;'>Wrong password.</p>" "")))
      (insert (render-footer)))))

(defun httpd/admin/auth (proc path query args)
  (let ((pwd (board-get-arg args "pwd")))
    (if (string= pwd board-admin-password)
        (progn
          (httpd-send-header proc "text/html" 302
                             :Location "/home"
                             :Set-Cookie (format "session=%s; Path=/; HttpOnly"
                                                 (secure-hash 'sha256 board-admin-password)))
          (process-send-string proc ""))
      (httpd-redirect proc "/login?error=1"))))

;; --- POSTING LOGIC ---
(defun httpd/post-entry (proc path query args)
  (let* ((ip (board-get-ip proc args))
         (admin (board-is-admin-p proc args))
         (resto (board-get-arg args "resto"))
         (fail-url (if (and resto (not (string-empty-p resto)))
                       (format "/threads/%s?error=ratelimit" resto)
                     "/home?error=ratelimit")))
    (if (or admin (board-check-rate-limit ip (1+ board-post-count)))
        (board-handle-post proc args)
      (progn
        (httpd-send-header proc "text/html" 302 :Location fail-url)
        (process-send-string proc "")))))

(defun httpd/ (proc path query args) (httpd-redirect proc "/home"))

;; --- FINAL BOOT ---
(board-load) 
(httpd-start)
(princ (format "\nBoard active at: http://localhost:%d/home\n" httpd-port))

;; Infinite loop for batch mode
(while t (accept-process-output nil 1))
