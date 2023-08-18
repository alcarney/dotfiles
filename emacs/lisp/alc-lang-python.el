;;; alc-lang-python.el --- Python configuration

(defun my/python-mode-hook ()
  "Tweaks and config to run when starting `python-mode'"
  (setq-local fill-column 88)
  (eglot-ensure))

(use-package python
  :hook (python-mode . my/python-mode-hook)
  :config
  (setq python-shell-dedicated 'project))

(provide 'alc-lang-python)
