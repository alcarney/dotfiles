;;; alc-lang-rst.el --- Settings for reStructuredText files

(use-package rst
  :hook ((rst-mode . eglot-ensure)
         ;; TODO: Figure out how to prevent flyspell's default keybindings from
         ;;       conflicting with the `completion-at-point' binding we want.
         ;; (rst-mode . flyspell-mode)
         (rst-mode . visual-line-mode))
  :bind (:map rst-mode-map
              ("C-M-i" . completion-at-point)))



(provide 'alc-lang-rst)
