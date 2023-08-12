;;; init.el -- Emacs configuration -*- lexical-binding: t -*-
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil :family "UbuntuMono NF" :height 120)
(set-face-attribute 'fixed-pitch nil :family "UbuntuMono NF" :height 120)
(set-face-attribute 'variable-pitch nil :family "Ubuntu NF" :weight 'light :height 115)

(setq custom-file (make-temp-file "emacs-custom-"))

(package-initialize)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'alc-completion)
(require 'alc-editing)
(require 'alc-general)
(require 'alc-minibuffer)
(require 'alc-modeline)
(require 'alc-window)

(require 'alc-dired)
(require 'alc-eww)
(require 'alc-notes)

(require 'alc-lang)
(require 'alc-lang-nix)
(require 'alc-lang-python)
(require 'alc-lang-rst)
(require 'alc-lang-rust)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
