;;; model.el --- Logic and Data Management
(require 'subr-x)
(require 'cl-lib)

(defvar board-threads nil)
(defvar board-post-count 0)
(defvar board-root nil)
(defvar board-banned-ips nil)

;; (defun board-save ()
;;   (let ((save-path (expand-file-name "data/board-data.el" board-root)))
;;     (unless (file-exists-p (file-name-directory save-path))
;;       (make-directory (file-name-directory save-path) t))
;;     (with-temp-file save-path
;;       (let ((print-length nil) (print-level nil) (print-quoted t))
;;         (prin1 (list :count board-post-count :threads board-threads :bans board-banned-ips) (current-buffer))))))

;; (defun board-load ()
;;   (let ((load-path (expand-file-name "data/board-data.el" board-root)))
;;     (if (file-exists-p load-path)
;;         (with-temp-buffer
;;           (insert-file-contents load-path)
;;           (condition-case nil
;;               (let ((data (read (current-buffer))))
;;                 (setq board-post-count (or (plist-get data :count) 0))
;;                 (setq board-threads (plist-get data :threads))
;;                 (setq board-banned-ips (plist-get data :bans)))
;;             (error (setq board-threads nil board-post-count 0))))
;;       (setq board-threads nil board-post-count 0))))

(defvar board-post-log nil "List of (ip timestamp post-id)")

(defun board-save ()
  (let ((save-path (expand-file-name "data/board-data.el" board-root)))
    (with-temp-file save-path
      (let ((print-length nil) (print-level nil) (print-quoted t))
        (prin1 (list :count board-post-count 
                     :threads board-threads 
                     :bans board-banned-ips
                     :log board-post-log) (current-buffer))))))

(defun board-load ()
  (let ((load-path (expand-file-name "data/board-data.el" board-root)))
    (if (file-exists-p load-path)
        (with-temp-buffer
          (insert-file-contents load-path)
          (let ((data (read (current-buffer))))
            (setq board-post-count (or (plist-get data :count) 0))
            (setq board-threads (plist-get data :threads))
            (setq board-banned-ips (plist-get data :bans))
            (setq board-post-log (plist-get data :log)))))))

(defun board-check-rate-limit (ip &optional post-id)
  "Check limit and log the attempt. post-id is optional to prevent argument errors."
  (let* ((now (float-time))
         (one-hour-ago (- now 3600)))
    ;; Cleanup old entries
    (setq board-post-log (cl-remove-if (lambda (e) (< (nth 1 e) one-hour-ago)) board-post-log))
    ;; Count matches for this IP
    (let ((count (cl-count-if (lambda (e) (string= (car e) ip)) board-post-log)))
      (if (< count 5)
          (progn (push (list ip now post-id) board-post-log) t)
        nil))))

(defun board-classic-tripcode (pass)
  (let* ((perl-code "use Encode qw(encode); my $pw = substr($ARGV[0], 0, 8); my $sjv = encode('shiftjis', $pw); my $salt = substr($sjv . 'H.', 1, 2); $salt =~ tr/:;<=>?@[\\\\\\\]^_`/ABCDEFGabcdef/; $salt =~ s/[^.-z]/./g; print substr(crypt($sjv, $salt), -10);")
         (result (with-temp-buffer
                   (call-process "perl" nil t nil "-MEncode" "-e" perl-code "--" pass)
                   (string-trim (buffer-string)))))
    result))

(defun generate-tripcode (input)
  (let ((clean-input (replace-regexp-in-string "◆" "&#9671;" input)))
    (cond
     ((string-match "^\\([^#]*\\)#\\(.+\\)$" clean-input)
      (let* ((name (match-string 1 clean-input))
             (pass (match-string 2 clean-input))
             (trip (board-classic-tripcode pass)))
        (list (if (string-empty-p name) "Anonymous" name) trip)))
     (t (list (if (string-empty-p clean-input) "Anonymous" clean-input) nil)))))

(defun get-latest-unique-tags (threads count)
  "Extract the last N unique tags from all thread OPs."
  (let ((all-tags nil))
    (dolist (thread threads)
      (setq all-tags (append (plist-get (plist-get thread :op) :tags) all-tags)))
    ;; reverse to get recent ones first, delete-dups, then take the count
    (seq-take (delete-dups (reverse all-tags)) count)))

;; upd ate

(defun get-all-board-tags (threads)
  (let ((all-tags nil))
    (dolist (thread threads)
      (setq all-tags (append (plist-get (plist-get thread :op) :tags) all-tags)))
    (sort (delete-dups all-tags) 'string<)))

(provide 'model)
