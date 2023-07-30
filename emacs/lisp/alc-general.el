;;; alc-general.el --- Catch all for settings that don't belong anywhere else.

;; Always use absolute numbers, even when narrowed
(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

;; Enable some commands
(put 'scroll-left 'disabled nil)

;; Remember recently visited files - also track frequently visited buffers
(recentf-mode t)
;; (add-hook ’buffer-list-update-hook ’recentf-track-opened-file)

;; Remember last visted location in files.
(save-place-mode t)

;; Context menus!
(context-menu-mode t)

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
