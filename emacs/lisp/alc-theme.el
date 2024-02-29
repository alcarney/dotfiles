;;; alc-theme.el --- Themes and related settings -*- lexical-binding: t -*-

(use-package ef-themes
  :bind ("<f5>" . ef-themes-toggle)
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t
        ef-themes-to-toggle '(ef-elea-light ef-elea-dark))
  (ef-themes-select 'ef-elea-light))

(use-package spacious-padding
  :config
  (setq spacious-padding-subtle-mode-line t
        spacious-padding-widths '(:internal-border-width 1
                                  :header-line-width 4
                                  :mode-line-width 6
                                  :tab-width 1
                                  :right-divider-width 30
                                  :scroll-bar-width 8))
  (spacious-padding-mode 1))

(provide 'alc-theme)
