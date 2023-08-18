;;; alc-lang-rust.el --- Settings for rust files -*- lexical-binding: t -*-

(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))

(provide 'alc-lang-rust)
