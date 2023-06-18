;;; alc-lang-rust.el --- Settings for rust files

(use-package rustic
  :ensure t
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-lsp-client 'eglot))

(provide 'alc-lang-rust)
