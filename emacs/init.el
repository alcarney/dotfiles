;;; init.el -- Emacs configuration -*- lexical-binding: t -*-
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(set-face-attribute 'default nil :family "UbuntuMono NF" :height 120)
(set-face-attribute 'fixed-pitch nil :family "UbuntuMono NF" :height 120)
(set-face-attribute 'variable-pitch nil :family "Ubuntu NF" :weight 'light :height 115)

(setq custom-file (make-temp-file "emacs-custom-"))

(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'alc-editing)
(require 'alc-general)
(require 'alc-isearch)
(require 'alc-minibuffer)
(require 'alc-modeline)
(require 'alc-tab-bar)
(require 'alc-terminals)
(require 'alc-theme)
(require 'alc-window)

(require 'alc-dired)
(require 'alc-eww)
(require 'alc-git)
(require 'alc-notes)

(require 'alc-lang)
(require 'alc-lang-nix)
(require 'alc-lang-python)
(require 'alc-lang-rst)
(require 'alc-lang-rust)
(require 'alc-lang-typescript)
