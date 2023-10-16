;; -*- lexical-binding: t; -*-

;; Increase garbage collection threshold temporarily and reset it later.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file'
;; is loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(setq initial-frame-alist
      '((fullscreen . maximized)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-line . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

;; No title bar
(when (>= emacs-major-version 29)
  (push '(undecorated-round . t) default-frame-alist))

;; Don't blind me, if I'm in dark mode when opening Emacs.
(when (eq ns-system-appearance 'dark)
  (push '(background-color . "black") default-frame-alist))

;; `file-name-handler-alist' is consulted on each call to `require', `load',
;;  or various file/io functions (like `expand-file-name' or `file-remote-p').
;;  You get a noteable boost to startup time by unsetting or simplifying its
;;  value.
(setq file-name-handler-alist-old file-name-handler-alist
      file-name-handler-alist nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(set-face-attribute 'default nil :height 160)

;; Small margin to left so text don't need to start right next to screen's
;; edge.
(setq-default left-margin-width 1)
