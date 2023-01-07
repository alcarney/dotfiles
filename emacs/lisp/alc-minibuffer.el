;;; alc-minibuffer --- Minibuffer configuration

;; Filter commands according to major mode
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package savehist
  ;; Vertico uses this to sort by most recently used.
  :init (savehist-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))


(provide 'alc-minibuffer)
