;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Appearance
(load-theme 'modus-operandi t)
(global-set-key (kbd "<f5>") #'modus-themes-toggle)
(set-face-attribute 'default nil :font "VictorMono NF" :height 113 :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(setq inhibit-startup-message t)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)

(global-display-line-numbers-mode 1)
(dolist (hook '(doc-view-mode-hook
                eshell-mode-hook
                gfm-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;; General
(save-place-mode t)
(savehist-mode t)

;; Bootstrap use-package
(package-initialize)

(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Whitespace
(use-package whitespace
  :init
  (setq whitespace-style '(face empty trailing))
  (setq-default indent-tabs-mode nil)
  (global-whitespace-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup))

(use-package all-the-icons :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (column-number-mode 1)
  (size-indication-mode 1)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon nil
        doom-modeline-minor-modes nil))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
