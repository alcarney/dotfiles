;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Disable GUI things
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; And enable others
(show-paren-mode t)
(menu-bar-mode t)

(setq frame-inhibit-implied-resize t
      inhibit-x-resources t
      inhibit-startup-message t)
