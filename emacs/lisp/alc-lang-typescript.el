;;; alc-lang-typescript.el --- Typescript configuration


(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook ((typescript-ts-mode . eglot-ensure)))

(provide 'alc-lang-typescript)
