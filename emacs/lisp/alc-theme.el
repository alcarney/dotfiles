;;; alc-theme.el --- Themes and related settings -*- lexical-binding: t -*-

(use-package ef-themes
  :bind ("<f5>" . ef-themes-toggle)
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-to-toggle '(ef-elea-light ef-elea-dark))
  (ef-themes-select 'ef-elea-light))

(provide 'alc-theme)
