;;; view.el --- Modular HTML Rendering Engine
(require 'url-util)

;; --- HELPERS ---

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

;; --- RENDERING CORE ---

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
         (reflink-target (if thread-id (format "/thread?id=%s#p\\1" thread-id) "#p\\1"))
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
         (post-link (if in-thread-view (format "#p%s" id) (format "/thread?id=%s#p%s" thread-id id))))
    (format "<div class='post %s' id='p%s'>
                  <div class='post-meta'>
                    <span class='subject'>%s</span> 
                    <span class='name'>%s%s</span> 
                    <span class='date'>%s</span> 
                    <span class='num'><a class='post-id-link' href='%s'>No.%s</a></span>
                    %s %s %s
                  </div>
                  <div class='content'>%s</div>
               </div>"
            (if is-op "op" "reply") 
            id 
            (board-escape-html (or (plist-get post :subject) ""))
            (board-escape-html (or (plist-get post :name) "Anonymous")) 
            (if (plist-get post :trip) (format "<span class='tripcode'>!%s</span>" (plist-get post :trip)) "")
            (plist-get post :timestamp) 
            post-link id
            (if (and is-op (not in-thread-view)) (format " [<a href='/thread?id=%s'>View</a>]" id) "")
            (if in-thread-view (format " [<span class='quote-btn' onclick='quote(%s)'>Reply</span>]" id) "")
            (if is-admin (format "<small class='ip-tag'>(IP: %s)</small>" ip) "")
            (board-render-body (or (plist-get post :body) "") thread-id))))

(defun render-footer ()
  "<div class='footer'>Powered by Emacs Lisp</div></div></body></html>")

(defun render-tag-overview (tags)
  "Renders the horizontal list of recent tags shown on the homepage."
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
  "Renders the full /tags index page."
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

(provide 'view)
