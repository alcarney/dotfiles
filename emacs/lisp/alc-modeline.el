;;; alc-modeline.el --- Modeline

(column-number-mode)
(size-indication-mode)
(display-time-mode)
(setq display-time-format "%H:%M"
      display-time-load-average nil
      mode-line-position-column-line-format '(" %l:%c"))

(use-package minions
  :config (minions-mode 1))

(provide 'alc-modeline)
