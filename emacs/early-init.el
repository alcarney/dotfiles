;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Disable GUI things
(blink-cursor-mode -1)
(menu-bar-mode -1)  ;; Use the menu-bar via the tab-bar instead
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; And enable others
(show-paren-mode t)

(setq frame-inhibit-implied-resize t
      inhibit-x-resources t
      inhibit-startup-message t)
