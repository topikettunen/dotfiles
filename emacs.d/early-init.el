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
      '((top . 1) (left . 1) (width . 100) (height . 58)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq default-frame-alist
      '((background-color . "black")
        (tool-bar-lines . 0)
        (menu-bar-line . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

;; `file-name-handler-alist' is consulted on each call to `require',
;;  `load', or various file/io functions (like `expand-file-name' or
;;  `file-remote-p'). You get a noteable boost to startup time by unsetting
;;  or simplifying its value.
(setq file-name-handler-alist-old file-name-handler-alist
      file-name-handler-alist nil)

(set-face-attribute 'default nil :height 180)
