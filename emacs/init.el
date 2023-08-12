;;; init.el -- Emacs configuration -*- lexical-binding: t -*-
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(tool-bar-mode -1)

(setq inhibit-startup-message t)
(setq custom-file (make-temp-file "emacs-custom-"))

(package-initialize)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'alc-completion)
(require 'alc-dired)
(require 'alc-editing)
(require 'alc-general)
(require 'alc-minibuffer)
(require 'alc-modeline)
(require 'alc-notes)

(require 'alc-lang)
(require 'alc-lang-rst)
(require 'alc-lang-rust)
(require 'alc-lang-nix)
(require 'alc-lang-python)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
