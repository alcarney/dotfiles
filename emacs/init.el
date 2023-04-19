;;; init.el -- Emacs configuration -*- lexical-binding: t -*-
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)

(setq inhibit-startup-message t)

;; Always use absolute numbers, even when narrowed
(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

(save-place-mode t) ; Remember last visted location in files.

(package-initialize)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'alc-completion)
(require 'alc-editing)
(require 'alc-minibuffer)
(require 'alc-modeline)
(require 'alc-notes)

(require 'alc-lang)
(require 'alc-lang-rst)
(require 'alc-lang-nix)
(require 'alc-lang-python)

(use-package solaire-mode
  :ensure t
  :init (solaire-global-mode +1))

(use-package modus-themes
  :bind ("<f5>" . modus-themes-toggle)
  :init
  (setq modus-themes-diffs 'desaturated
        modus-themes-headings           '((t . rainbow-section-no-bold))
        modus-themes-intense-hl-line    t
        modus-themes-lang-checkers      'straight-underline
        modus-themes-links              'faint-neutral-underline
        modus-themes-mode-line          '(accented borderless)
        modus-themes-org-blocks         'grayscale
        modus-themes-paren-match        'intense-bold
        modus-themes-region             'bg-only-no-extend
        modus-themes-scale-headings     t
        modus-themes-slanted-constructs t
        modus-themes-variable-pitch-ui  t)

  (load-theme 'modus-operandi t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(define-minor-mode recompile-on-save-mode
  "When enabled, run `recompile' after the current buffer is saved"
  :lighter "recompile"
  (if recompile-on-save-mode
      (add-hook 'after-save-hook 'recompile nil t)
    (remove-hook 'after-save-hook 'recompile t)))

(use-package whitespace
  :init
  (setq whitespace-style '(face empty tab-mark trailing missing-newline-at-eof))
  (setq-default indent-tabs-mode nil)
  (global-whitespace-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup))

(put 'narrow-to-region 'disabled nil)
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
