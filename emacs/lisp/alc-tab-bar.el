;;; alc-tab-bar.el --- Tab bar configuration -*- lexical-binding: t -*-

(use-package tab-bar
  :config
  (setq tab-bar-show 0  ; Always show the tab-bar
        ;; Don't show the tabs themselves, use tab-bar like a panel.
        tab-bar-format '(tab-bar-format-align-right tab-bar-format-global))
  (tab-bar-mode))

(use-package battery
  :config
  (display-battery-mode))

(use-package time
  :config
  (setq display-time-format "%H:%M %d/%m/%y"
        display-time-day-and-date nil
        display-time-default-load-average nil)
  (display-time-mode))

(provide 'alc-tab-bar)
