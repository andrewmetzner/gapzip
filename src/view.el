;;; view.el --- Modular HTML Rendering Engine meowww

(defun board-get-config (filename default-val)
  (let ((file-path (expand-file-name (concat "data/" filename) 
                                     (or (bound-and-true-p board-root) default-directory))))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (if (string-empty-p (string-trim line)) default-val (string-trim line))))
      default-val)))

(defvar site-root (board-get-config "hostname" "http://localhost:8080"))

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

(defun render-footer ()
  "<div class='footer' style='text-align:center; margin-top:20px; font-size:0.8em; color:#888;'>
      Powered by <a href='https://github.com/andrewmetzner/gapzip' target='_blank'>Emacs Lisp</a>
    </div>
    </div></body></html>")

(defun render-header (title &optional is-admin feed-url)
  (format "<!DOCTYPE html><html><head>
            <meta charset='UTF-8'>
            <meta name='viewport' content='width=device-width, initial-scale=0.7'>
            <link rel=\"icon\" type=\"image/png\" href=\"/public/gwf.png\">
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
              %s
              %s
            </div>" 
          title
          (if feed-url (format "<link rel='alternate' type='application/rss+xml' title='%s' href='%s'>" title feed-url) "")
          (if feed-url (format " <a href='%s' class='rss-link'>[RSS get!]</a>" feed-url) "")
          (if is-admin 
              "<b>Admin:</b> <a href='/admin/dashboard'>Bans</a> | <a href='/admin/log'>Log</a> | <a href='/admin/logout'>Logout</a>" 
              "<a href='/login'>∈)☼(∋</a>")))

(defun board-render-body (text &optional thread-id)
  (let* ((escaped (board-escape-html text))
         (reflink-target (if thread-id 
                              (format "/thread?id=%s#p\\1" thread-id) 
                            "#p\\1"))
         (with-reflinks (replace-regexp-in-string "&gt;&gt;\\([0-9]+\\)" 
                                                  (format "<a href='%s' class='reflink'>>>\\1</a>" reflink-target) 
                                                  escaped))
         (lines (split-string with-reflinks "\n")))
    (mapconcat (lambda (line) 
                  (if (string-prefix-p "&gt;" (string-trim-left line)) 
                      (format "<span class='greentext'>%s</span>" line) 
                    line)) 
                lines "<br>")))

(defun render-post-html (post &optional is-op in-thread-view thread is-admin)
  (let* ((id (plist-get post :id)) 
         (ip (plist-get post :ip)) 
         (tags (plist-get post :tags))
         (thread-id (if is-op id (plist-get (plist-get thread :op) :id)))
         (post-link (if in-thread-view 
                        (format "#p%s" id) 
                      (format "/thread?id=%s#p%s" thread-id id)))
         (backlinks (when (and thread in-thread-view) (board-get-replies-to id thread))))
    (format "<div class='post %s' id='p%s'>
                 <div class='post-meta'>
                   <span class='subject'>%s</span> 
                   <span class='name'>%s%s</span> 
                   <span class='date'>%s</span> 
                   <span class='num'><a class='post-id-link' href='%s'>No.%s</a></span>
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
            post-link id
            (if (and is-op (not in-thread-view)) 
                (format " [<a href='/thread?id=%s' class='nav-link'>View Thread</a>]" id) "")
            (if in-thread-view 
                (format " [<span class='quote-btn' onclick='quote(%s)'>Reply</span>]" id) "")
            
            (if is-admin 
                (format "<span class='admin-tools'>
                           [<a href='/admin/edit?id=%s' class='admin-edit'>E</a>] 
                           [<a href='/admin/delete?id=%s' class='admin-del'>D</a>] 
                           [<a href='/admin/ban?ip=%s' class='admin-ban'>B</a>] 
                           <small class='ip-tag'>(IP: %s)</small>
                         </span>" id id ip ip) "")

            (board-render-body (or (plist-get post :body) "") thread-id)

            (if backlinks 
                (format "<div class='backlinks'>Replies: %s</div>" 
                        (mapconcat (lambda (bid) (format "<a href='/thread?id=%s#p%s' class='reflink'>>>%s</a>" thread-id bid bid)) backlinks " "))
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
          (board-render-rss-body (or (plist-get post :body) "")) 
          (board-format-rss-date (plist-get post :timestamp))))

(defun board-render-rss-body (text)
  (let* ((escaped (board-escape-html text))
         (with-reflinks (replace-regexp-in-string 
                         "&gt;&gt;\\([0-9]+\\)" 
                         "<span style='color:#5555ff; font-weight:bold;'>>>\\1</span>" 
                         escaped))
         (lines (split-string with-reflinks "\n")))
    (format "<![CDATA[%s]]>"
            (mapconcat (lambda (line)
                         (if (string-prefix-p "&gt;" (string-trim-left line))
                             (format "<span style='color:#789922;'>%s</span>" line)
                           line))
                       lines "<br>"))))

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

(defun render-tag-overview (tags)
  (if (not tags)
      ""
    (format "<div class='tag-overview' style='text-align:center; margin:10px 0; font-family:monospace;'>
                <span style='color:#888;'>recent tags:</span> 
                %s
                <br><div style='display:inline-block; margin-left:15px;'>
                  <form action='/tags' method='GET' style='display:inline;'>
                    <input type='text' name='name' placeholder='search tags...' 
                           style='background:#000; color:#ccc; border:1px solid #444; font-size:0.8em; padding:2px 5px; width:100px;'>
                  </form>
                </div>
              </div><hr>"
            (mapconcat (lambda (tag) 
                         (format "<a href='/tags?name=%s' class='tag'>%s</a>" 
                                 (url-hexify-string tag) tag)) 
                       tags " "))))

(defun render-tag-search-page (all-tags &optional is-admin)
  (concat
   (render-header "Tag Index" is-admin)
   "<div class='container'>
      <a href='/home'>[Back]</a>
      <h2>Board Tag Index</h2>
      <div class='tag-search-box' style='margin-bottom:20px;'>
        <form action='/tags' method='GET'>
            <input type='text' name='name' placeholder='Search tag...' style='background:#111; color:#fff; border:1px solid #333;'>
            <input type='submit' value='Filter'>
        </form>
      </div>
      <div class='tag-cloud-full'>"
   (if all-tags
       (mapconcat (lambda (tag) 
                    (format "<a href='/tags?name=%s' class='tag' style='display:inline-block; margin:5px;'>%s</a>" 
                            (url-hexify-string tag) tag)) 
                  all-tags " ")
     "No tags found.")
   "</div></div>"
   (render-footer)))

(defun render-rate-limit-page (ip wait-time)
  (concat
   (render-header "Rate Limited")
   (format "<div class='container' style='display:flex; justify-content:center; align-items:center; height:50vh;'>
             <div class='rate-limit-error' style='color: #ff4444; border: 1px solid #ff4444; 
                          padding: 20px; width: 400px; text-align: center; 
                          font-family: monospace; background: #1a0000; box-shadow: 0 0 15px rgba(255,0,0,0.5);'>
               <b style='font-size: 1.5em;'>[!] RATE LIMIT REACHED</b><br><br>
               <span style='font-size: 1em; color: #ccc;'>
                 IP: %s<br><br>
                 Max 5 posts per hour.<br>
                 Please wait about %d seconds.
               </span><br><br>
               <a href='/home' style='color:#888; text-decoration:none;'>[ Return to Board ]</a>
             </div>
           </div>" ip wait-time)
   (render-footer)))



(provide 'view)
