;;; alc-lang-rust.el --- Settings for rust files -*- lexical-binding: t -*-

(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot
        rustic-analyzer-command '("nix" "develop" "-c" "rust-analyzer")))

(provide 'alc-lang-rust)
