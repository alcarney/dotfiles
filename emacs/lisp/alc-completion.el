;;; alc-completion.el --- Completion configuration

(setq tab-always-indent t)

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

(provide 'alc-completion)
