;;; alc-dired.el --- Settings for dired

(use-package dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq dired-dwim-target t
        ;; -A, all files - except '.' & '..'
        ;; -F, add symbols denoting object type (file, directory, etc.)
        ;; -G, omit owning group
        ;; -h, human readable file sizes
        ;; -l, long listing, required for dired.
        ;; -v, natural sort of (version) numbers within text
        dired-listing-switches "-AFGhlvX --group-directories-first --time-style=long-iso"))

(provide 'alc-dired)
