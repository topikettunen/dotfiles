;; -*- lexical-binding: t; -*-

;;; Startup

;; Remove global eldoc-mode.
(remove-hook 'after-change-major-mode-hook
             'global-eldoc-mode-enable-in-buffers)
(global-eldoc-mode -1)

;; Garbage collect after startup is done.
(add-hook 'after-init-hook #'garbage-collect t)

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
  (defsubst tok/emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (defsubst tok/user-data (path)
    (let ((data-dir (tok/emacs-path "data")))
      (unless (file-directory-p data-dir)
        (make-directory data-dir))
      (expand-file-name path (tok/emacs-path "data")))))

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
  (defun llvm-lineup-statement (langelem)
    (let ((in-assign (c-lineup-assignments langelem)))
      (if (not in-assign)
          '++
        (aset in-assign 0
              (+ (aref in-assign 0)
                 (* 2 c-basic-offset)))
        in-assign)))
  :init
  (c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))

  (setq c-default-style '((c-mode . "llvm.org")
                          (c++-mode . "llvm.org")
                          (java-mode . "java")
                          (awk-mode . "awk")
                          (other . "gnu"))))

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

(use-package dired
  :bind (:map dired-mode-map
              ;; Don't create new buffers when moving up or down in
              ;; directories.
              ("RET" . dired-find-alternate-file)
              ("^" . (lambda () (interactive) (find-alternate-file "..")))))

(use-package emacs
  :bind (("C-z"))
  :custom
  (auto-hscroll-mode 'current-line)
  (column-number-mode t)
  (custom-file "~/.emacs.d/custom.el")
  (create-lockfiles nil)
  (fill-column 80)
  (history-delete-duplicates t)
  (history-length t)
  (indent-tabs-mode nil)
  (load-prefer-newer t)
  (menu-bar-mode (display-graphic-p))
  (tool-bar-mode nil)
  (ns-alternate-modifier 'alt)
  (ns-command-modifier 'meta)
  (ns-function-modifier 'hyper)
  (ns-right-alternate-modifier 'alt)
  (frame-title-format
   '(:eval (if buffer-file-name default-directory "%b")))
  (process-adaptive-read-buffering nil)
  (ring-bell-function 'ignore)
  (scroll-conservatively 101)
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
                                  "#commonlisp"
                                  "#emacs"
                                  "#freebsd"
                                  "#lisp"
                                  "#lispcafe"
                                  "#sbcl")))
  (erc-timestamp-only-if-changed-flag nil)
  (erc-timestamp-format "[%H:%M] ")
  (erc-track-position-in-mode-line 't)  ; at the end of global-mode-string
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
        (error "No auth entry found for %s@%s:%s" user host port))))

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

(use-package flyspell
  :custom
  (ispell-list-command "--list")
  (flyspell-default-directory "en_GB")
  :hook (text-mode . flyspell-mode))

(use-package files
  :custom
  (auto-save-list-file-prefix (tok/user-data "auto-save-list/.saves-"))
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:.*" ,temporary-file-directory t)))
  (auto-save-list-file-name nil t)
  (backup-by-copying t)
  (backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
  (confirm-kill-emacs nil)
  (delete-old-versions t)
  (require-final-newline t)
  (version-control t)
  :init
  (defadvice find-file (around find-file-line-number
                               (filename &optional wildcards)
                               activate)
    "Turn files like file.cpp:14 into opening file.cpp and going to the
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

(use-package grep
  :bind ("M-s g" . grep)
  :custom
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil))

(use-package help
  :bind ((:map help-map
	       ("F" . describe-face))))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-display-maybe-show-predicates t)
  (ibuffer-expert t)
  (ibuffer-maybe-show-regexps nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)
        (mode . vterm-mode)))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Dired"
       (mode . dired-mode))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")))
      ("ERC"
       (mode . erc-mode))
      ("Go"
       (mode . go-mode))
      ("Lisp"
       (or
        (mode . emacs-lisp-mode)
        (mode . lisp-mode)))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "\\*magit")
        (name . "magit-")
        (name . "git-monitor")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (mode . org-mode)))
      ("Terraform"
       (mode . terraform-mode)))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-shrink-to-minimum-size t t)
  (ibuffer-use-other-window t)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-max-prospects 6)
  (ido-use-filename-at-point nil)
  (ido-use-virtual-buffers t)
  (ido-use-virtual-buffers-automatically t)
  :init
  (ido-mode 1)
  (ido-everywhere))

(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

(use-package lisp
  :custom
  (delete-pair-blink-delay 0))

(use-package lisp-mode
  :hook ((emacs-lisp-mode lisp-mode)
         . (lambda ()
             (add-hook 'after-save-hook #'check-parens nil t))))

(use-package net-utils
  :custom
  (ping-program-options (list "-c" "4")))

(use-package newcomment
  :custom
  (comment-empty-lines t))

(use-package novice
  :custom
  (disabled-command-function nil t))

(use-package nsm
  :custom
  (nsm-settings-file (tok/user-data "network-security.data")))

(use-package paragraphs
  :custom
  (sentence-end-double-space nil))

(use-package project
  :custom
  (project-list-file (tok/user-data "projects")))

(use-package recentf
  :bind (("C-x C-r" . ido-recentf-open))
  :custom
  (recentf-auto-cleanup 60)
  (recentf-max-saved-items 100)
  (recentf-save-file (tok/user-data "recentf"))
  :preface
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  :init
  (recentf-mode 1))

(use-package replace
  :bind (("C-x C-l" . query-replace-regexp)))

(use-package savehist
  :unless noninteractive
  :custom
  (savehist-file (tok/user-data "history"))
  :init
  (savehist-mode 1))

(use-package sh-mode
  :custom
  (sh-basic-offset 2))

(use-package simple
  :custom
  (kill-whole-line t)
  :diminish auto-fill-function
  :bind (("C-t")
         ("C-h" . delete-backward-char)
         ("M-h" . backward-kill-word)
         ("M-z" . zap-up-to-char))
  :hook (text-mode . auto-fill-mode)
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package startup
  :custom
  (initial-scratch-message "")
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.cache/emacs/backups")
  :config
  ;; Without this change, tramp ends up sending hundreds of shell commands to
  ;; the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))

  ;; Setting this with `:custom' does not take effect.
  (setq tramp-persistency-file-name (tok/user-data "tramp")))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package url
  :custom
  (url-configuration-directory (tok/user-data "url/")))

(use-package url-cache
  :custom
  (url-cache-directory (tok/user-data "url/cache")))

(use-package vc
  :custom
  (vc-follow-symlinks t))

(use-package windmove
  :custom
  (windmove-wrap-around t)
  :init
  (windmove-default-keybindings 'meta))

(use-package window
  :preface
  (defun tok/shrink-window-hor ()
    (interactive)
    (shrink-window-horizontally 5))

  (defun tok/enlarge-window-hor ()
    (interactive)
    (enlarge-window-horizontally 5))

  (defun tok/shrink-window-ver ()
    (interactive)
    (shrink-window 5))

  (defun tok/enlarge-window-ver ()
    (interactive)
    (enlarge-window 5))
  :bind (("M-o" . window-swap-states)
         ("M-S-<left>" . tok/shrink-window-hor)
         ("M-S-<right>" . tok/enlarge-window-hor)
         ("M-S-<down>" . tok/shrink-window-ver)
         ("M-S-<up>" . tok/enlarge-window-ver)))

;;; Local

(use-package llvm-mode
  :demand t
  :load-path "lisp/")

(use-package tok-colors
  :no-require t
  :demand t
  :init
  (deftheme tok-colors)

  (custom-theme-set-faces
   'tok-colors
   '(default ((t (:background "black" :foreground "grey85")))))
  :config
  (enable-theme 'tok-colors))

(use-package tok-common
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

(use-package tok-org
  :demand t
  :load-path "lisp/")

;;; Third-party

(defmacro tok/install-pkg (pkgs)
  "Some of the third-party packages are just installed without
giving configurations in them at all. This macro is just a helper
for installing those."
  `(progn
     ,@(cl-loop for pkg in pkgs
                collect `(use-package ,pkg :ensure t))))

(tok/install-pkg (cmake-mode
                  dockerfile-mode
                  jenkinsfile-mode
                  magit
                  markdown-mode
                  nginx-mode
                  yaml-mode))

(use-package exec-path-from-shell
  :ensure t
  :if (string-equal system-type "darwin")
  :init
  (exec-path-from-shell-initialize))

(use-package go-mode
  :ensure t
  :preface
  (defun tok/go-mode-hook ()
    (setq-local compile-command "go build"
                tab-width 2))
  :hook (go-mode . gofmt-before-save))

(use-package multiple-cursors
  :ensure t
  :custom
  (mc/list-file (tok/user-data "mc-lists.el"))
  :defer 1
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package paredit
  :diminish
  :ensure t
  :bind (:map paredit-mode-map
              ("M-r")
              ("M-s")
              ("M-I" . paredit-splice-sexp)
              ("M-J"))
  :hook ((emacs-lisp-mode lisp-mode slime-repl-mode) . paredit-mode))

(use-package slime
  :ensure t
  :diminish slime-autodoc-mode
  :bind (:map slime-mode-map
              ("C-c M-m" . slime-macroexpand-1))
  :custom
  (slime-repl-history-file (tok/user-data "slime-history.eld"))
  (slime-kill-without-query-p t)
  (slime-startup-animation nil)
  :preface
  (defun tok/slime-completion-at-point ()
    "Make SLIME behave nicely with corfu."
    (let ((slime-current-thread :repl-thread)
          (package (slime-current-package)))
      (when-let ((symbol (thing-at-point 'symbol)))
        (pcase-let ((`(,beg . ,end)
                     (bounds-of-thing-at-point 'symbol)))
          (list beg end
                (car (slime-eval
                      ;; Or swank:simple-completions
                      `(swank:fuzzy-completions
                        ,(substring-no-properties symbol) ',package))))))))
  :init
  (setq inferior-lisp-program "ros -Q run")

  (advice-add #'slime--completion-at-point
              :override #'tok/slime-completion-at-point))

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
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package transient
  :ensure t
  :custom
  (transient-history-file (tok/user-data "transient/history.el"))
  (transient-values-file (tok/user-data "transient/values.el")))

(use-package vterm
  :if (not (string= system-type "windows-nt"))
  :ensure t
  :demand t
  :preface
  (defun tok/vterm (&optional arg)
    (interactive "P")
    (let ((buf-name (format "*vterminal<%d>*" (if arg arg 1))))
      (unless (get-buffer buf-name)
        (vterm buf-name))
      (switch-to-buffer buf-name)))
  :custom
  (vterm-timer-delay nil)
  :hook (vterm-mode . (lambda ()
                        (setq-local show-trailing-whitespace nil)
                        (display-line-numbers-mode -1)))
  :bind (("C-c t" . tok/vterm)
         (:map vterm-mode-map
               ("C-h" . vterm-send-backspace))))

;;; Finalization

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            ;; Restore desired GC values and revert `file-name-handler-alist'.
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.5
                  file-name-handler-alist file-name-handler-alist-old)
            ;; Delete no longer necessary startup variable.
            (makunbound 'file-name-handler-alist-old)))
