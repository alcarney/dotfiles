;;; alc-general.el --- Catch all for settings that don't belong anywhere else.

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

(use-package ef-themes :ensure t)

(use-package modus-themes
  :bind ("<f5>" . modus-themes-toggle)
  :init
  ;; (setq modus-themes-diffs 'desaturated
  ;;       modus-themes-headings           '((t . rainbow-section-no-bold))
  ;;       modus-themes-intense-hl-line    t
  ;;       modus-themes-lang-checkers      'straight-underline
  ;;       modus-themes-links              'faint-neutral-underline
  ;;       modus-themes-mode-line          '(accented borderless)
  ;;       modus-themes-org-blocks         'grayscale
  ;;       modus-themes-paren-match        'intense-bold
  ;;       modus-themes-region             'bg-only-no-extend
  ;;       modus-themes-scale-headings     t
  ;;       modus-themes-slanted-constructs t
  ;;       modus-themes-variable-pitch-ui  t)

  (load-theme 'modus-operandi t))

(provide 'alc-general)
