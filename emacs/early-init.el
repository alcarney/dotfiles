;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Disable gui things
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)

(setq frame-inhibit-implied-resize t
      inhibit-x-resources t
      inhibit-startup-message t)
