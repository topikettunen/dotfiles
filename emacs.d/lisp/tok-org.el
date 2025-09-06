;; -*- lexical-binding: t; -*-

(defun tok/org-path (path)
  (expand-file-name path (if (string-equal system-type "darwin")
                             "~/Documents/org/"
                           "~/org/")))

(use-package org
  :bind (("M-m" . org-capture))
  :custom
  (org-capture-templates
   `(("t" "Add task" entry
      (file+headline ,(tok/org-path "todo.org") "Inbox")
      "* TODO %?" :prepend t)
     ("h" "Add habit" entry
      (file+headline ,(tok/org-path "habits.org") "Habits")
      "* TODO %?
:PROPERTIES:
:STYLE: habit
:END:" :prepend t)
     ("p" "Add paper to reading list" entry
      (file+headline ,(tok/org-path "reading-list.org") "Reading List")
      "* TODO [[%^{URL}][%^{Name}]]
:PROPERTIES:
:READING_TYPE: paper
:END:" :prepend t)
     ("b" "Add book to reading list" entry
      (file+headline ,(tok/org-path "reading-list.org") "Reading List")
      "* TODO %^{Author} - %^{Name}
:PROPERTIES:
:READING_TYPE: book
:END:" :prepend t)))
  (org-fast-tag-selection-single-key 'expert)
  (org-log-into-drawer t)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  :preface
  (defun todo ()
    (interactive)
    (find-file (tok/org-path "todo.org")))

  (defun habits ()
    (interactive)
    (find-file (tok/org-path "habits.org")))

  (defun reading-list ()
    (interactive)
    (find-file (tok/org-path "reading-list.org")))

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property
                                    :begin (org-element-at-point))))
     "/DONE" 'file)
    (org-save-all-org-buffers))
  :config
  (push 'org-habit org-modules)
  :init
  (dolist (f (list (tok/org-path "todo.org")
                   (tok/org-path "habits.org")
                   (tok/org-path "reading-list.org")))
    (unless (file-exists-p f)
      (make-empty-file f))))

(use-package org-agenda
  :bind (("M-C" . jump-to-org-agenda-dashboard)
         ("C-c a" . org-agenda))
  :custom
  (org-agenda-custom-commands
   `(("d" "Dashboard"
      ((agenda "")
       (todo ""
             ((org-agenda-overriding-header "Unscheduled TODO")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))
       (agenda ""
               ((org-agenda-span 'day)
                (org-agenda-overriding-header "Habit tracker")
                (org-agenda-files
                 '(,(tok/org-path "habits.org")))))))
     ("r" "Reading list"
      ((tags "READING_TYPE=\"paper\""
             ((org-agenda-overriding-header "Paper reading list")
              (org-agenda-prefix-format " %?-12t% s")
              (org-agenda-files
               '(,(tok/org-path "reading-list.org")))))
       (tags "READING_TYPE=\"book\""
             ((org-agenda-overriding-header "Book reading list")
              (org-agenda-prefix-format " %?-12t% s")
              (org-agenda-files
               '(,(tok/org-path "reading-list.org")))))))))
  (org-agenda-files `(,(tok/org-path "todo.org")
                      ,(tok/emacs-path "lunar.org")))
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-window-setup 'only-window)
  :preface
  (defun jump-to-org-agenda-dashboard ()
    (interactive)
    (org-agenda nil "d"))

  (defun org-lunar-phases ()
    "Show lunar phase in Agenda buffer."
    (require 'lunar)
    (let* ((phase-list (lunar-phase-list (nth 0 date) (nth 2 date)))
           (phase (cl-find-if (lambda (phase) (equal (car phase) date))
                              phase-list))
           (lunar-phase-names '("● New Moon"
                                "☽ First Quarter Moon"
                                "○ Full Moon"
                                "☾ Last Quarter Moon")))
      (when phase
        ;; Return the phase to the agenda file.
        (setq ret (concat (lunar-phase-name (nth 2 phase)))))))
  :init
  (add-hook 'emacs-startup-hook #'jump-to-org-agenda-dashboard t))

(provide 'tok-org)
