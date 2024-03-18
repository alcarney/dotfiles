;;; alc-lang-python.el --- Python configuration

(defun alc-python-mode-hook ()
  "Tweaks and config to run when starting `python-mode'"
  (setq-local fill-column 88)

  ;; Files in site-packages/ should make read only by default.
  ;; TODO: Include system lib/ and typeshed-fallback/ in this
  (if (string-match-p "site-packages/" (buffer-file-name))
      (read-only-mode)
    (eglot-ensure)))

(use-package python
  :hook ((python-mode . alc-python-mode-hook)
         (python-ts-mode . alc-python-mode-hook))
  :config
  (setq python-shell-dedicated 'project))

(provide 'alc-lang-python)
