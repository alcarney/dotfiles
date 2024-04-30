;;; alc-theme.el --- Themes and related settings -*- lexical-binding: t -*-

(use-package ef-themes
  :bind ("<f5>" . ef-themes-toggle)
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-to-toggle '(ef-dream ef-reverie))
  (ef-themes-select 'ef-reverie))

(provide 'alc-theme)
