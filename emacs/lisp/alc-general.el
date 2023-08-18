;;; alc-general.el --- Catch all for settings that don't belong anywhere else. -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "29.1"))

;; Always use absolute numbers, even when narrowed
(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

;; Enable some commands
(put 'scroll-left 'disabled nil)

;; Remember recently visited files
(recentf-mode t)
;; - also track frequently visited buffers
;; (add-hook ’buffer-list-update-hook ’recentf-track-opened-file) ; TODO: Why is this symbol void on first load?

;; Remember last visted location in files.
(save-place-mode t)

;; Context menus!
(context-menu-mode t)

;; Highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-context-when-offscreen 'overlay)

;; Put tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-resize-echo-area t)

;; Smooth scrolling
(pixel-scroll-precision-mode t)

(use-package emacs
  :bind (("C-x C-b" . ibuffer)))

(use-package bookmark
  :config
  ;; Save the bookmarks file immediately.
  (setq bookmark-save-flag 1))

(use-package spacious-padding
  :hook (after-init . spacious-padding-mode))

(use-package ef-themes
  :bind ("<f5>" . ef-themes-toggle)
  :hook ((after-init . (lambda () (ef-themes-select 'ef-elea-light)))
         (server-after-make-frame . (lambda () (ef-themes-select 'ef-elea-light))))
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-to-toggle '(ef-elea-light ef-elea-dark)))

(provide 'alc-general)
