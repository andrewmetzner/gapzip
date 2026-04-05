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
              "<b>Admin:</b> <a href='/admin/dashboard'>Bans</a> | <a href='/admin/log'>Log</a> | <a href='/admin/stats'>Stats</a> | <a href='/admin/logout'>Logout</a>"
              "<a href='/login'>∈)☼(∋</a>")))

(defun board-render-body (text &optional thread-id)
  (let* ((escaped (board-escape-html text))
         (reflink-target (if thread-id
                              (format "/threads/%s#p\\1" thread-id)
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
                      (format "/threads/%s#p%s" thread-id id)))
         (backlinks (when (and thread in-thread-view) (board-get-replies-to id thread)))

         (raw-subject (or (plist-get post :subject) ""))
         (subject-text (if (and is-op (string-empty-p (string-trim raw-subject)))
                           (format "Thread #%s" id)
                         (board-escape-html raw-subject))))
    (format "<div class='post %s' id='p%s'>
                  <div class='post-meta'>
                    <span class='subject'>%s</span><br>
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
            subject-text
            (board-escape-html (or (plist-get post :name) "Anonymous")) 
            (if (plist-get post :trip) (format "<span class='tripcode'>!%s</span>" (plist-get post :trip)) "")
            (plist-get post :timestamp) 
            post-link id
            (if (and is-op (not in-thread-view))
                (format " [<a href='/threads/%s' class='nav-link'>View Thread</a>]" id) "")
            (if in-thread-view 
                (format " [<span class='quote-btn' onclick='quote(%s)'>Reply</span>]" id) "")
            
            (if is-admin
                (format "<span class='admin-tools'>
                            [<span class='admin-edit' style='cursor:pointer;'
                                  onclick='var e=document.getElementById(\"edit-%s\");e.style.display=e.style.display===\"none\"?\"block\":\"none\"'>edit</span>]
                            [<a href='/admin/delete?id=%s' class='admin-del'>D</a>]
                            [<a href='/admin/ban?ip=%s' class='admin-ban'>B</a>]
                            <small class='ip-tag'>(IP: %s)</small>
                          </span>
                          <div id='edit-%s' style='display:none; margin-top:6px;'>
                            <form method='POST' action='/admin/update'
                                  style='background:#111; border:1px solid #333; padding:8px; font-family:monospace;'>
                              <input type='hidden' name='id' value='%s'>
                              <input name='name' value='%s' placeholder='Name'
                                     style='width:100%%; background:#000; color:#ccc; border:1px solid #333; margin-bottom:4px;'><br>
                              %s
                              <textarea name='comment' rows='5'
                                        style='width:100%%; background:#000; color:#ccc; border:1px solid #333; font-family:monospace; margin-bottom:4px;'>%s</textarea><br>
                              <input type='submit' value='save'
                                     style='font-size:11px; padding:2px 8px;'>
                              <span onclick='document.getElementById(\"edit-%s\").style.display=\"none\"'
                                    style='cursor:pointer; margin-left:8px; color:#888;'>[cancel]</span>
                            </form>
                          </div>"
                        id id ip ip
                        id id
                        (board-escape-html (or (plist-get post :name) ""))
                        (if is-op
                            (format "<input name='subject' value='%s' placeholder='Subject'
                                            style='width:100%%; background:#000; color:#ccc; border:1px solid #333; margin-bottom:4px;'><br>
                                     <input name='tags' value='%s' placeholder='Tags (comma separated)'
                                            style='width:100%%; background:#000; color:#ccc; border:1px solid #333; margin-bottom:4px;'><br>"
                                    (board-escape-html (or (plist-get post :subject) ""))
                                    (mapconcat 'identity (or tags '()) ","))
                          "<input type='hidden' name='subject' value=''>")
                        (board-escape-html (or (plist-get post :body) ""))
                        id)
              "")

            (board-render-body (or (plist-get post :body) "") thread-id)

            (if backlinks 
                (format "<div class='backlinks'>Replies: %s</div>" 
                        (mapconcat (lambda (bid) (format "<a href='/threads/%s#p%s' class='reflink'>>>%s</a>" thread-id bid bid)) backlinks " "))
              "")
            (if (and is-op tags)
                (format "<div class='tags-container' style='margin-top:5px;'>
                            <span class='tag-label'>Tags:</span> %s
                          </div>"
                        (mapconcat (lambda (tag) (format "<a href='/tags/%s' class='tag'>%s</a>" (url-hexify-string tag) tag)) tags " "))
              ""))))

(defun render-thread-html (thread &optional full-view is-admin)
  (let* ((op (plist-get thread :op))
         (replies (plist-get thread :replies))
         (count (length replies))
         (shown (if full-view replies (last replies 3)))
         (is-locked (plist-get thread :locked))
         (is-sticky (plist-get thread :sticky))
         (op-id (plist-get op :id)))
    (concat
     (if full-view
         (format "<div class='thread-info'>Thread No.%s &mdash; %d repl%s%s%s%s</div>"
                 op-id count (if (= count 1) "y" "ies")
                 (if is-sticky " <span style='color:#ff0; font-weight:bold;'>[STICKY]</span>" "")
                 (if is-locked " <span style='color:#f44; font-weight:bold;'>[LOCKED]</span>" "")
                 (if is-admin
                     (format " &mdash; <a href='/admin/sticky?id=%d' class='admin-edit'>[%s sticky]</a> <a href='/admin/lock?id=%d' class='admin-edit'>[%s lock]</a>"
                             op-id (if is-sticky "un" "") op-id (if is-locked "un" ""))
                   ""))
       (concat
        (when (or is-sticky is-locked)
          (format "<div style='font-size:0.8em; margin-bottom:2px;'>%s%s</div>"
                  (if is-sticky "<span style='color:#ff0; font-weight:bold;'>[STICKY] </span>" "")
                  (if is-locked "<span style='color:#f44; font-weight:bold;'>[LOCKED]</span>" "")))))
     "<div class='thread-container'>"
     (render-post-html op t full-view thread is-admin)
     (mapconcat (lambda (r) (render-post-html r nil full-view thread is-admin)) shown "")
     (if (not full-view)
         (format "<div style='text-align:right; font-size:0.8em; color:#666; padding:4px 8px; border-top:1px solid #222;'>total replies: %d</div>" count)
       "")
     "</div>")))

(defun render-rss-item (post thread-id)
  (format "<item>
             <title>No.%s</title>
             <link>%s/threads/%s#p%s</link>
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
             <link>" site-root "/threads/" (number-to-string id) "</link>"
            (render-rss-item (plist-get thread :op) id) 
            (mapconcat (lambda (r) (render-rss-item r id)) (plist-get thread :replies) "") 
            "</channel></rss>")))


(defun render-tag-overview (tags)
  "Show a few recent tags and a link to the full directory."
  (if (not tags)
      ""
    (format "<div class='tag-overview' style='text-align:center; margin:10px 0; font-family:monospace;'>
                <span style='color:#888;'>recent:</span> %s
                <br><a href='/tags' class='nav-link' style='font-size:0.85em;'>[view all tags]</a>
              </div><hr>"
            (mapconcat (lambda (tag) 
                         (format "<a href='/tags/%s' class='tag'>%s</a>" 
                                 (url-hexify-string tag) tag)) 
                       tags " "))))

(defun render-tag-index-page (all-tags &optional is-admin)
  "Renders the grid of tags for the /tags page."
  (concat
   "<div class='thread-container' style='padding:20px;'>
      <h2 style='margin-top:0;'>all tags</h2>
      <div class='tag-cloud-full' style='display: flex; flex-wrap: wrap; gap: 10px;'>"
   (if all-tags
       (mapconcat (lambda (tag) 
                    (format "<a href='/tags/%s' class='tag' style='font-size: 1.1em;'>%s</a>" 
                            (url-hexify-string tag) tag)) 
                  (sort all-tags 'string-lessp) "")
     "<p class='greentext'>no tags.</p>")
   "</div></div>"))


(defun render-tag-search-page-content (all-tags &optional is-admin)
  (concat
   "<div class='container'>
      <a href='/home' class='nav-link'>[Back]</a>
    </div>
    <div class='thread-container' style='padding: 20px; border: 1px solid #333; background: rgba(0,0,0,0.2);'>
      <h2 style='margin-top:0;'>all tags</h2>
      <div class='tag-cloud-full' style='display: flex; flex-wrap: wrap; gap: 10px;'> "
   (if all-tags
       (mapconcat (lambda (tag) 
                    (format "<a href='/tags/%s' class='tag' style='font-size: 1.1em;'>%s</a>" 
                            (url-hexify-string tag) tag)) 
                  all-tags "")
     "<p class='greentext'>WHAT?? NO TAGS!.</p>")
   "</div></div>"))


(defun render-rate-limit-box (&optional ip wait-time)
  "Returns a standalone HTML error box for injection into pages."
  (format 
   "<div class='rate-limit-error' style='color: #ff4444; border: 1px solid #ff4444; 
                padding: 10px; margin: 10px auto; width: 345px; text-align: center; 
                font-family: monospace; background: #1a0000; box-shadow: 0 0 10px rgba(255,0,0,0.5);'>
     <b style='font-size: 1.2em;'>[!] RATE LIMIT REACHED</b><br>
     <span style='font-size: 0.85em; color: #ccc;'>
       IP: %s<br>
       YOU ARE LIMITED!
     </span>
   </div>" 
   (or ip "Unknown") 
   (or wait-time "60")))



(provide 'view)
