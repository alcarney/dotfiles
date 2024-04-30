;;; alc-terminals.el --- (e)Shell, term, eat, oh my! -*- lexical-binding: t -*-

(use-package eat
  :hook ((eshell-load . eat-eshell-mode))
  :init
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "t" #'eat-project)))

(provide 'alc-terminals)
