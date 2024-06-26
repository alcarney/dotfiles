;;; alc-general.el --- Catch all for settings that don't belong anywhere else. -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "29.1"))

;; Line numbers
(setq-default display-line-numbers-widen t) ; Always use absolute numbers, even when narrowed
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

;; Enable some commands
(dolist (command '(dired-find-alternate-file
                   narrow-to-region
                   scroll-left))
  (put command 'disabled nil))

;; Don't try to be clever, TAB should be used for indentation.
(setq tab-always-indent t)

;; Remember recently visited files
(recentf-mode t)
;; - also track frequently visited buffers
;; (add-hook ’buffer-list-update-hook ’recentf-track-opened-file) ; TODO: Why is this symbol void on first load?

;; Remember last visted location in files.
(save-place-mode t)

;; Repeat things
(repeat-mode)

;; Context menus!
(context-menu-mode t)

;; Improve performance of files with long lines
(use-package so-long
  :init
  (global-so-long-mode 1)
  :config
  (add-to-list 'so-long-variable-overrides '(truncate-lines . t) t))

;; Highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-context-when-offscreen 'overlay)

;; Put tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-resize-echo-area t)

;; Use the treesitter equivalent for the following modes.
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

;; Smooth scrolling
(use-package pixel-scroll
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolation-total-time 0.2)
  :init
  (pixel-scroll-precision-mode t))

;; Backup files
(defun alc-general-make-backup (fpath)
  "Transform the given filepath into the path at which to save its backup"
  (let* ((backup-root (locate-user-emacs-file "backup"))
         (backup-fpath (concat backup-root fpath "~")))
    (make-directory (file-name-directory backup-fpath) t)
    backup-fpath))

(setq make-backup-file-name-function #'alc-general-make-backup)

(use-package emacs
  :bind (("C-x C-b" . ibuffer)))

(use-package bookmark
  :config
  ;; Save the bookmarks file immediately.
  (setq bookmark-save-flag 1))

(provide 'alc-general)
