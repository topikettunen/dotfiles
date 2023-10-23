;; -*- lexical-binding: t; -*-

;;; Startup

;; Remove global eldoc-mode.
(remove-hook 'after-change-major-mode-hook
             'global-eldoc-mode-enable-in-buffers)
(global-eldoc-mode -1)

;; Garbage collect after startup is done.
(add-hook 'after-init-hook #'garbage-collect t)

;;; Functions

(eval-and-compile
  (defun tok/emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (defun tok/org-path (path)
    (expand-file-name path (if (string= system-type "darwin")
                               "~/Documents/org/"
                             "~/org/")))

  (defun lookup-password (host user port)
    (require 'auth-source)
    (require 'auth-source-pass)
    (let ((auth (auth-source-search :host host :user user :port port)))
      (if auth
          (let ((secretf (plist-get (car auth) :secret)))
            (if secretf
                (funcall secretf)
              (error "Auth entry for %s@%s:%s has no secret!"
                     user host port)))
        (error "No auth entry found for %s@%s:%s" user host port)))))

;;; Packages

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa"
                     (concat proto "://melpa.org/packages/"))
               t))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      use-package-compute-statistics nil
      use-package-always-defer (not init-file-debug)
      debug-on-error init-file-debug)

(eval-and-compile
  (require 'use-package))

;;; Diminish minor modes before everything else.

(use-package diminish
  :ensure t
  :demand t)

;;; Built-in

(use-package abbrev
  :diminish)

(use-package asm-mode
  :hook (asm-mode . (lambda () (setq-local indent-tabs-mode t))))

(use-package autorevert
  :custom
  (auto-revert-use-notify nil)
  :init
  (global-auto-revert-mode t))

(use-package calendar
  :custom
  ;; Week starts on Monday
  (calendar-week-start-day 1))

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :preface
  (defun tok/cc-mode-hook ()
    ;; The depth of -10 places this before eglot's
    ;; willSave notification, so that that
    ;; notification reports the actual contents
    ;; that will be saved.
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (setq-local compile-command "ninja"))

  (defun llvm-lineup-statement (langelem)
    (let ((in-assign (c-lineup-assignments langelem)))
      (if (not in-assign)
          '++
        (aset in-assign 0
              (+ (aref in-assign 0)
                 (* 2 c-basic-offset)))
        in-assign)))
  :hook ((c-mode c++-mode) . tok/cc-mode-hook)
  :bind (:map c-mode-base-map
              ("<tab>" . indent-for-tab-command))
  :init
  ;; Add a cc-mode style for editing LLVM C and C++ code
  (c-add-style "llvm.org"
               '("gnu"
	         (fill-column . 80)
	         (c++-indent-level . 2)
	         (c-basic-offset . 2)
	         (indent-tabs-mode . nil)
	         (c-offsets-alist . ((arglist-intro . ++)
				     (innamespace . 0)
				     (member-init-intro
                                      . llvm-lineup-statement)))))

  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "llvm.org"))))

(use-package compile
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compilation-skip-threshold 2)
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (progn
            (display-buffer it)
            (pop-to-buffer it))
        (call-interactively 'compile)))))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package dired
  :bind (:map dired-mode-map
              ;; Don't create new buffers when moving up or down in
              ;; directories.
              ("RET" . dired-find-alternate-file)
              ("^" . (lambda () (interactive) (find-alternate-file "..")))))

(use-package eglot
  :commands eglot
  :hook ((c-mode c++-mode go-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider
                                       :hoverProvider
                                       :inlayHintProvider))
  (eglot-workspace-configuration '((:gopls . ((staticcheck . t)))))
  :config
  (setq read-process-output-max (* 1024 1024))

  (add-hook 'eglot-managed-mode-hook
            #'(lambda ()
                ;; Show flymake diagnostics first.
                (setq eldoc-documentation-functions
                      (cons #'flymake-eldoc-function
                            (remove #'flymake-eldoc-function
                                    eldoc-documentation-functions))))))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package eldoc
  :diminish
  :hook ((c-mode-common go-mode) . eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-display-truncation-message nil))

(use-package emacs
  :bind (("C-z"))
  :custom
  (auto-hscroll-mode 'current-line)
  (column-number-mode t)
  (custom-file "~/.emacs.d/custom.el")
  (create-lockfiles nil)
  (fill-column 78)
  (history-delete-duplicates t)
  (history-length t)
  (indent-tabs-mode nil)
  (load-prefer-newer t)
  (ns-alternate-modifier 'meta)
  (ns-command-modifier 'meta)
  (ns-right-alternate-modifier 'none)
  (process-adaptive-read-buffering nil)
  (ring-bell-function 'ignore)
  (size-indication-mode t)
  (tab-always-indent 'complete)
  (user-full-name "Topi Kettunen")
  (user-mail-address "topi@topikettunen.com")
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)

  (when (string-equal system-type "darwin")
    (setq default-directory "/Users/tok/")))

(use-package erc
  :bind (:map erc-mode-map
              ("C-c r" . reset-erc-track-mode))
  :custom
  (erc-autojoin-channels-alist '(("libera.chat"
                                  "#c"
                                  "#C++"
                                  "#commonlisp"
                                  "#freebsd"
                                  "#lisp"
                                  "#lispcafe"
                                  "#networking"
                                  "#sbcl"
                                  "#security")))
  (erc-timestamp-only-if-changed-flag nil)
  (erc-timestamp-format "[%H:%M] ")
  (erc-fill-prefix "          ")
  (erc-fill-column 70)
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-autojoin-timing 'ident)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :preface
  (defun setup-irc-env ()
    (setq-local scroll-conservatively 101
                line-spacing 4))

  (defun irc (&optional arg)
    (interactive "P")
    (let ((server "irc.libera.chat")
          (nick "tok"))
      (erc-tls :server server :port 6697 :nick nick
               :password (lookup-password server nick 6697))))

  (defun reset-erc-track-mode ()
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update)
    (erc-modified-channels-display)
    (force-mode-line-update))
  :hook (erc-mode . setup-irc-env)
  :config
  (erc-track-minor-mode 1)
  (erc-track-mode 1))

(use-package flymake
  :custom
  (flymake-mode-line-format
   '(" " flymake-mode-line-exception flymake-mode-line-counters))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flyspell
  :custom
  (ispell-list-command "--list")
  (flyspell-default-directory "en_GB")
  :hook (text-mode . flyspell-mode))

(use-package files
  :custom
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:.*" ,temporary-file-directory t)))
  (auto-save-list-file-name nil t)
  (backup-directory-alist
   `((".*" . "~/.cache/emacs/backups")
     ("\\(recentf\\|archive/sent)" ,temporary-file-directory)))
  (confirm-kill-emacs 'y-or-n-p)
  (delete-old-versions t)
  (require-final-newline t)
  (version-control t)
  :init
  (defadvice find-file (around find-file-line-number
                               (filename &optional wildcards)
                               activate)
    "Turn files like file.cpp:14 into file.cpp and going to the
 14th line."
    (save-match-data
      (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
             (line-number (and matched
                               (match-string 2 filename)
                               (string-to-number (match-string 2 filename))))
             (filename (if matched (match-string 1 filename) filename)))
        ad-do-it
        (when line-number
          ;; `goto-line' is for interactive use.
          (goto-char (point-min))
          (forward-line (1- line-number)))))))

(use-package help
  :bind (("<f1>" . help)
         (:map help-map
	       ("F" . describe-face)))
  :init
  (temp-buffer-resize-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido-mode
  :custom
  (ido-enable-flex-matching t)
  (ido-max-prospects 6)
  (ido-use-filename-at-point nil)
  (ido-use-virtual-buffers t)
  (ido-use-virtual-buffers-automatically t)
  :init
  (ido-mode 1)
  (ido-everywhere))

(use-package js
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'auto-mode-alist
               `("\\.json\\'" . ,(if (>= emacs-major-version 29)
                                     js-json-mode
                                   js-mode))))

(use-package kmacro
  :bind ("C-x C-k C-k"))

(use-package lisp-mode
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda () (add-hook 'after-save-hook #'check-parens nil t))))

(use-package newcomment
  :custom
  (comment-empty-lines t))

(use-package novice
  :custom
  (disabled-command-function nil t))

(use-package org
  :bind (("M-m" . org-capture))
  :custom
  (org-capture-templates
   `(("t" "Add task" entry
      (file+headline ,(tok/org-path "todo.org") "Inbox")
      "* TODO %?")
     ("h" "Add habit" entry
      (file+headline ,(tok/org-path "habits.org") "Habits")
      "* TODO %?
:PROPERTIES:
:STYLE: habit
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

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file)
    (org-save-all-org-buffers))
  :config
  (add-to-list 'org-modules 'org-habit t))

(use-package org-agenda
  :bind (("M-C" . jump-to-org-agenda-dashboard)
         ("C-c a" . org-agenda))
  :custom
  (org-agenda-custom-commands
   `(("d" "Dashboard"
      ((agenda "")
       (todo ""
             ((org-agenda-overriding-header "Unscheduled TODO")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
       (agenda ""
             ((org-agenda-span 'day)
              (org-agenda-overriding-header "Habit tracker")
              (org-agenda-files
               '(,(tok/org-path "habits.org")))))))))
  (org-agenda-files `(,(tok/org-path "todo.org")
                      ,(tok/emacs-path "lunar.org")))
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  (org-agenda-inhibit-startup t)
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

(use-package paragraphs
  :custom
  (sentence-end-double-space nil))

(use-package replace
  :bind (("C-x C-r" . query-replace)
         ("C-x C-l" . query-replace-regexp)))

(use-package sh-script
  :custom
  (sh-basic-offset 2))

(use-package simple
  :custom
  (kill-whole-line t)
  :diminish auto-fill-function
  :bind (("C-h" . delete-backward-char)
         ("M-h" . backward-kill-word))
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package startup
  :custom
  (initial-scratch-message "")
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode))

(use-package time
  :custom
  (display-time-24hr-format t)
  :init
  (display-time-mode))

(use-package windmove
  :bind (("M-h" . windmove-left))
         ("M-l" . windmove-right)
         ("M-j" . windmove-down)
         ("M-k" . windmove-up))

(use-package window
  :bind (("M-o" . window-swap-states)
         ("M-H" . (lambda () (interactive) (shrink-window-horizontally 5)))
         ("M-L" . (lambda () (interactive) (enlarge-window-horizontally 5)))
         ("M-J" . (lambda () (interactive) (shrink-window 5)))
         ("M-K" . (lambda () (interactive) (enlarge-window 5)))))

;;; Local

(use-package llvm-mode
  :demand t
  :load-path "lisp/")

(use-package tok
  :demand t
  :load-path "lisp/"
  :bind (("C-c C-d" . duplicate-line)
         ("<f3>" . sudo)
         ("C-x C-x" . jump-to-mark-and-recenter)
         ("<C-M-return>" . toggle-frame-fullscreen)
         ("C-x K" . delete-current-buffer-file)
         ("C-x R" . rename-current-buffer-file)
         ("C-c M-q" . unfill-paragraph)))

(use-package tok-hugo
  :demand t
  :load-path "lisp/"
  :custom
  (hugo-project-root "~/infra/")
  (hugo-site-path "topikettunen.com"))

(use-package tok-theme
  :demand t
  :load-path "themes/tok-theme/"
  :preface
  (defun reload-theme ()
    (interactive)
    (disable-theme 'tok)
    (load-theme 'tok t))

  (defun toggle-theme ()
    (interactive)
    (if tok-theme-dark
        (setenv "TOK_DARK_THEME")
      (setenv "TOK_DARK_THEME" "1"))
    (setq tok-theme-dark (if tok-theme-dark nil t))
    (load-theme 'tok t))

  (defun tok/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (progn
                (setenv "TOK_DARK_THEME")
                (setq tok-theme-dark nil)))
      ('dark (progn
               (setenv "TOK_DARK_THEME" "1")
               (setq tok-theme-dark t))))
    (load-theme 'tok t))
  :bind (("C-<f12>" . reload-theme)
         ("<f12>" . toggle-theme))
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tok-theme")
  (add-hook 'ns-system-appearance-change-functions #'tok/apply-theme)
  (load-theme 'tok t))

;;; Third-party

(use-package cmake-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (string-equal system-type "darwin")
  :init
  (exec-path-from-shell-initialize))

(use-package go-mode
  :ensure t
  :preface
  (defun tok/gofmt-before-save ()
    (interactive)
    (gofmt-before-save)
    ;; Run `eglot-code-actions' only in buffers where `eglot' is active.
    (when (functionp 'eglot-code-actions)
      (eglot-code-actions nil nil "source.organizeImports" t)))

  (defun tok/go-mode-hook ()
    (setq-local compile-command "go build")
    ;; Using depth -10 will put this before eglot's
    ;; willSave notification so that the notification
    ;; reports the actual contents that will be
    ;; saved.
    (add-hook 'before-save-hook 'tok/gofmt-before-save -10 t))
  :hook (go-mode . tok/go-mode-hook))

(use-package jenkinsfile-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :defer 1
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package paredit
  :diminish
  :ensure t
  :bind (:map paredit-mode-map
              ("M-J"))
  :hook ((emacs-lisp-mode lisp-mode sly-mrepl-mode)
         . paredit-mode)
  :config
  (advice-add 'paredit-RET
              :after
              (lambda ()
                (when (string-prefix-p "*sly-mrepl for"
                                       (buffer-name (current-buffer)))
                  (sly-mrepl-return)))))

(use-package sly
  :ensure t
  :bind (:map sly-mode-map
              ("C-c C-q" . sly-mrepl-sync))
  :custom
  (sly-kill-without-query-p t)
  (sly-symbol-completion-mode nil)
  :init
  (setq inferior-lisp-program "ros -Q run"))

(use-package terraform-mode
  :ensure t
  :preface
  (defun generate-terraform-docs (path)
    "Run terraform-docs to generate documentation for Terraform
module."
    (interactive (list (read-directory-name "Module directory: "
                                            default-directory)))
    (shell-command
     (concat "terraform-docs markdown table "
             "--output-file README.md "
             "--output-mode inject "
             path)))
  :hook ((terraform-mode . terraform-format-on-save-mode)))

(use-package yaml-mode
  :ensure t)

(use-package vterm
  :ensure t
  :demand t
  :preface
  (defun tok/vterm (term-no)
    (let ((buf-name (format "*vterminal<%d>*" term-no)))
      (unless (get-buffer buf-name)
        (vterm buf-name))
      (switch-to-buffer buf-name)))
  :custom
  (vterm-timer-delay 0.1)
  :hook (vterm-mode . (lambda ()
                        (setq-local show-trailing-whitespace nil)))
  ;; I rarely use more than three terminal, add more if necessary.
  :bind (("C-c 1" . (lambda () (interactive) (tok/vterm 1)))
         ("C-c 2" . (lambda () (interactive) (tok/vterm 2)))
         ("C-c 3" . (lambda () (interactive) (tok/vterm 3)))
         (:map vterm-mode-map
               ("C-h" . vterm-send-backspace))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook ((c-mode-common go-mode) . yas-minor-mode))

;;; Finalization

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            ;; Restore desired GC values and revert `file-name-handler-alist'.
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.5
                  file-name-handler-alist file-name-handler-alist-old)

            ;; Delete no longer necessary startup variable.
            (makunbound 'file-name-handler-alist-old)))
