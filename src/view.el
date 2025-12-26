;;; view.el --- Modular HTML Rendering Engine

(defvar site-root "http://localhost:8080")

(defun board-escape-html (text)
  (let ((table '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;"))))
    (dolist (pair table text) (setq text (replace-regexp-in-string (car pair) (cdr pair) text))) text))

(defun board-format-rss-date (iso-date)
  (condition-case nil (format-time-string "%a, %d %b %Y %H:%M:%S %z" (date-to-time iso-date)) (error iso-date)))

(defun board-get-replies-to (post-id thread)
  (let* ((all-posts (cons (plist-get thread :op) (plist-get thread :replies)))
         (re (format "&gt;&gt;%s\\b" post-id))
         (matches nil))
    (dolist (p all-posts)
      (let ((body (board-escape-html (or (plist-get p :body) ""))))
        (when (string-match re body)
          (push (plist-get p :id) matches))))
    (nreverse matches)))

(defun render-header (title &optional is-admin feed-url)
  (format "<!DOCTYPE html><html><head>
           <meta charset='UTF-8'>
           <meta name='viewport' content='width=device-width, initial-scale=0.7'>
           <title>%s</title>
           %s
           <link rel='stylesheet' href='/public/style.css'>
           <script>
             function quote(id) { 
               const box = document.querySelector('textarea[name=\"comment\"]'); 
               if(box) { box.value += '>>' + id + '\\n'; box.focus(); } 
             }
           </script></head>
           <body><div class='container'>
           <div class='top-nav'>
             <span class='board-title'>goatse.world</span> 
             %s | 
             %s
           </div>" 
          title
          (if feed-url (format "<link rel='alternate' type='application/rss+xml' title='%s' href='%s'>" title feed-url) "")
          (if feed-url (format " <a href='%s' class='rss-link'>[RSS get!]</a>" feed-url) "")
          (if is-admin 
              "<b>Admin:</b> <a href='/admin/dashboard'>Bans</a> | <a href='/admin/logout'>Logout</a>" 
              "<a href='/login'>Admin Login</a>")))

(defun board-render-body (text)
  (let* ((escaped (board-escape-html text))
         (with-reflinks (replace-regexp-in-string "&gt;&gt;\\([0-9]+\\)" 
                                                  "<a href='#p\\1' class='reflink'>>>\\1</a>" escaped))
         (lines (split-string with-reflinks "\n")))
    (mapconcat (lambda (line) 
                 (if (string-prefix-p "&gt;" (string-trim-left line)) 
                     (format "<span class='greentext'>%s</span>" line) 
                   line)) 
               lines "\n")))

(defun render-post-html (post &optional is-op in-thread-view thread is-admin)
  "Render an individual post with backlinks and Admin IP view."
  (let* ((id (plist-get post :id)) 
         (ip (plist-get post :ip)) 
         (tags (plist-get post :tags))
         (backlinks (when (and thread in-thread-view) (board-get-replies-to id thread))))
    (format "<div class='post %s' id='p%s'>
               <div class='post-meta'>
                 <span class='subject'>%s</span> 
                 <span class='name'>%s%s</span> 
                 <span class='date'>%s</span> 
                 <span class='num'><a class='post-id-link' href='#p%s'>No.%s</a></span>
                 %s %s %s
               </div>
               <div class='content'>%s</div>
               %s
               %s
             </div>"
            (if is-op "op" "reply") 
            id 
            (board-escape-html (or (plist-get post :subject) ""))
            (board-escape-html (or (plist-get post :name) "Anonymous")) 
            (if (plist-get post :trip) (format "<span class='tripcode'>!%s</span>" (plist-get post :trip)) "")
            (plist-get post :timestamp) 
            id id 
            (if (and is-op (not in-thread-view)) 
                (format " [<a href='/thread?id=%s' class='nav-link'>View Thread</a>]" id) "")
            (if in-thread-view 
                (format " [<span class='quote-btn' onclick='quote(%s)'>Reply</span>]" id) "")
            (if is-admin 
                (format "<span class='admin-tools'>[<a href='/admin/delete?id=%s' class='admin-del'>D</a>] [<a href='/admin/ban?ip=%s' class='admin-ban'>B</a>] <small class='ip-tag'>(IP: %s)</small></span>" id ip ip) "")
            (board-render-body (or (plist-get post :body) ""))
            (if backlinks 
                (format "<div class='backlinks'>Replies: %s</div>" 
                        (mapconcat (lambda (bid) (format "<a href='#p%s' class='reflink'>>>%s</a>" bid bid)) backlinks " "))
              "")
            (if (and is-op (or tags is-admin))
                (format "<div class='tags-container' style='display:flex; align-items:center; gap:10px; margin-top:5px;'>
                          <div><span class='tag-label'>Tags:</span> %s</div>
                          %s
                        </div>" 
                        (if tags (mapconcat (lambda (tag) (format "<a href='/tags?name=%s' class='tag'>%s</a>" (url-hexify-string tag) tag)) tags " ") "none")
                        (if is-admin 
                            (format "<form method='POST' action='/admin/update-tags' style='display:inline;'>
                                      <input type='hidden' name='id' value='%s'>
                                      <input type='hidden' name='redirect' value='%s'>
                                      <input name='tags' value='%s' style='width:120px; font-size:10px; background:#111; color:#ccc; border:1px solid #333; height:18px;'>
                                      <input type='submit' value='set' style='font-size:10px; height:20px; padding:0 5px;'>
                                    </form>" 
                                    id (if in-thread-view "thread" "home") (mapconcat 'identity tags ","))
                          ""))
              ""))))

(defun render-thread-html (thread &optional full-view is-admin)
  "Render a whole thread container with reply count."
  (let* ((op (plist-get thread :op)) 
         (replies (plist-get thread :replies)) 
         (count (length replies))
         (shown (if full-view replies (last replies 3))))
    (concat (if full-view 
                (format "<div class='thread-info'>Showing Thread No.%s (total replies: %d)</div>" (plist-get op :id) count) 
              "")
            "<div class='thread-container'>" 
            (render-post-html op t full-view thread is-admin)
            (mapconcat (lambda (r) (render-post-html r nil full-view thread is-admin)) shown "") 
            "</div>")))

;; --- RSS RENDERING ---

(defun render-rss-item (post thread-id)
  (format "<item>
             <title>No.%s</title>
             <link>%s/thread?id=%s#p%s</link>
             <description>%s</description>
             <pubDate>%s</pubDate>
           </item>" 
          (plist-get post :id) 
          site-root 
          thread-id 
          (plist-get post :id) 
          (board-escape-html (or (plist-get post :body) "")) 
          (board-format-rss-date (plist-get post :timestamp))))

(defun render-rss-feed (threads title)
  (concat "<?xml version='1.0' encoding='UTF-8' ?>
           <rss version='2.0'><channel>
           <title>" title "</title>
           <link>" site-root "/home</link>" 
          (mapconcat (lambda (tt) (render-rss-item (plist-get tt :op) (plist-get (plist-get tt :op) :id))) threads "") 
          "</channel></rss>"))

(defun render-thread-rss (thread)
  (let ((id (plist-get (plist-get thread :op) :id)))
    (concat "<?xml version='1.0' encoding='UTF-8' ?>
             <rss version='2.0'><channel>
             <title>Thread No." (number-to-string id) "</title>
             <link>" site-root "/thread?id=" (number-to-string id) "</link>"
            (render-rss-item (plist-get thread :op) id) 
            (mapconcat (lambda (r) (render-rss-item r id)) (plist-get thread :replies) "") 
            "</channel></rss>")))

(provide 'view)
