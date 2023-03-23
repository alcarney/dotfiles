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

(defun me/pyright-find-venv-path (dirname venv)
  "Search upwards from DIRNAME looking for VENV."
  (if (file-exists-p (file-name-concat dirname venv))
      dirname
    (let ((parent (file-name-directory (directory-file-name dirname))))
      (if (not (string= "/" parent))
          (me/pyright-find-venv-path parent venv)))))

(defun me/eglot-python-workspace-config (server)
  "Configure pyright to use the correct virtual environment."
  (if (project-current)
      (let* ((dirname   (expand-file-name (project-root (project-current))))
             (venv-path (me/pyright-find-venv-path dirname ".env")))
        (if venv-path
            (list (cons :python
                        (list :venvPath venv-path)))
                  (cons :python.analysis
                        (list :logLevel "trace"))))
    '()))

(use-package eglot
  :ensure t
  :config
  (setq-default eglot-workspace-configuration #'me/eglot-python-workspace-config))

(use-package yaml-mode :ensure t)

(provide 'alc-lang)
