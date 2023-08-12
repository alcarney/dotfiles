;;; alc-eww.el --- Configuration for eww

;; Package-Requires: ((emacs "29.1"))

(use-package eww
  :config
  (setq eww-auto-rename-buffer 'title))

(provide 'alc-eww)
