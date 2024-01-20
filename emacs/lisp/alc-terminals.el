;;; alc-terminals.el --- (e)Shell, term, eat, oh my! -*- lexical-binding: t -*-

(use-package eat
  :config
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "t" #'eat-project)))

(provide 'alc-terminals)
