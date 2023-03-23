;;; alc-lang.el --- General programming language config

(setq project-vc-extra-root-markers '("package.json"
                                      "pyproject.toml"))

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'isort apheleia-formatters)
      '("isort" "--profile=black" "--force-single-line" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
      '(isort black))
  (apheleia-global-mode))

(defun me/eglot-python-workspace-config (server)
  "Configure pyright to use the correct virtual environment."
  (message "Finding venv path...")
  (if (project-current)
      (let ((venv-path (project-root (project-current))))
        (list (cons :python
              (list :venvPath (expand-file-name venv-path)))))
    '()))

(use-package eglot
  :ensure t
  :config
  (setq-default eglot-workspace-configuration #'me/eglot-python-workspace-config))

(use-package yaml-mode :ensure t)

(provide 'alc-lang)
