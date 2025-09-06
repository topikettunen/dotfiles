;; -*- lexical-binding: t; -*-

(defcustom hugo-project-root nil
  "Hugo project root"
  :type 'string)

(defcustom hugo-site-path nil
  "Path to hugo site (under the hugo-project-root)"
  :type 'string)

(defconst hugo-server-buffer-name "*Hugo*")

(defun hugo-new-post (path)
  (interactive (list (read-string "New Hugo post: " "")))
  (let* ((site-path (concat hugo-project-root hugo-site-path))
        (default-directory site-path))
    (shell-command (concat "hugo new blog/" path))
    (find-file (concat site-path "/content/blog/" path))))

(defun hugo-get-server-process ()
  (let ((proc (get-buffer-process hugo-server-buffer-name)))
    (if (and proc (process-live-p proc))
        proc
        nil)))

(defun hugo-server-start (&optional arg)
  (interactive "P")
  (if (hugo-get-server-process)
      (message "Hugo server is already running")
    (let ((default-directory (concat hugo-project-root hugo-site-path)))
      (start-process "hugo" hugo-server-buffer-name "hugo" "serve" "-D")
      (message "Started Hugo server")))
  (unless arg (browse-url "http://localhost:1313/")))

(defun hugo-server-stop ()
  (interactive)
  (let ((proc (hugo-get-server-process)))
    (if proc
        (progn
          (interrupt-process proc)
          (message "Stopped Hugo server"))
      (message "Hugo server is not running")))
  (kill-buffer hugo-server-buffer-name))

(defun hugo-deploy ()
  (interactive)
  (let ((default-directory hugo-project-root))
    (let ((message-log-max nil))
      (compile (concat "./scripts/deploy-sites.sh")))))

(defun hugo-insert-current-time ()
  "Get the current timestamp for Hugo."
  (interactive)
  (let ((tz (format-time-string "%z")))
    (insert (format-time-string "%Y-%m-%dT%T")
            (substring tz 0 3) ":" (substring tz 3 5))))

(provide 'tok-hugo)

;;; tok-hugo.el ends here
