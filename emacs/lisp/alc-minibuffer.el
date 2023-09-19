;;; alc-minibuffer --- Minibuffer configuration -*- lexical-binding: t -*-

(use-package orderless)

;; Built-in completion framework settings
(setq completion-auto-help 'visible
      completion-auto-select nil
      completion-cycle-threshold 3
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion)))
      completion-show-help nil
      completion-styles '(orderless basic))

;; Built-in *Completions* buffer settings
(setq completions-detailed t
      completions-format 'one-column
      completions-group nil
      completions-header-format nil
      completions-max-height 15)

;; Keybindings
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)

;; Filter commands according to major mode
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; (use-package vertico
;;   :init
;;   (vertico-mode 1)

;;   ;; When calling `find-file' automatically clear the filepath prompt
;;   ;; when typing something that "resets" it e.g. '/' or '~/'
;;   (file-name-shadow-mode 1)
;;   (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package savehist
  ;; Vertico uses this to sort by most recently used.
  :init (savehist-mode))

(use-package marginalia)
  ;;(marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         :map project-prefix-map
         ("b" . consult-project-buffer)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))


(use-package embark-consult)

(use-package corfu
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'alc-minibuffer)
