;;; alc-general.el --- Catch all for settings that don't belong anywhere else. -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "29.1"))

;; Line numbers
(setq-default display-line-numbers-widen t) ; Always use absolute numbers, even when narrowed
(setq-default display-line-numbers-width 4)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

;; Enable some commands
(put 'scroll-left 'disabled nil)

;; Don't try to be clever, TAB should be used for indentation.
(setq tab-always-indent t)

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
