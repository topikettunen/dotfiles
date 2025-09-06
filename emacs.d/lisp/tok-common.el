;; -*- lexical-binding: t; -*-

(defun cfg ()
  (interactive)
  (find-file (tok/emacs-path "init.el")))

(defun sudo (&optional path)
  "Reopen `path' (or current file) with root privileges."
  (interactive)
  (find-alternate-file
   (concat "/sudo:root@localhost:" (or path buffer-file-name))))

(defun remote-sudo (host path)
  "Reopen `path' (or current file) with root privileges in the
remote host machine."
  (interactive "sHostname: \nsRemote find file (sudo): ")
  (find-alternate-file
   (format "/ssh:tok@%s|sudo::%s" host path)))

(defun duplicate-line (arg)
  "Duplicate the line containing point."
  (interactive "p")
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (if arg
          (dotimes (i arg)
            (unless (= i 0)
              (insert ?\n))
            (insert line-text))
        (insert line-text)))))

(defun logical-cores ()
  "Print the number of processing units available."
  (string-trim-right
   (shell-command-to-string
    (if (or (string-equal system-type "darwin")
            (string-equal system-type "berkley-unix"))
        "sysctl -n hw.logicalcpu"
      "nproc"))))

(defun rename-current-buffer-file ()
  "Renames the current buffer and the file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Delete the current buffer and the file connected with it"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (y-or-n-p "Are you sure this file should be removed? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil t)))

(defun jump-to-mark-and-recenter (&optional arg)
  (interactive "*p")
  (goto-char (mark))
  (recenter))

(provide 'tok-common)
