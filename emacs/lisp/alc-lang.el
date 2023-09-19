;;; alc-lang.el --- General programming language config

(setq project-vc-extra-root-markers '("Cargo.toml"
                                      "package.json"
                                      "pyproject.toml"))

(defun me/project-try-vc-subproject (orig-fun &rest args)
  "Advice for `project-try-vc'.

When using `project-vc-extra-root-markers' to teach project.el
about subprojects within a monorepo, `project-try-vc'
successfully finds the subject's root but fails to detect the
backend. But by calling `vc-responsible-backend' on the found
root, we can fill in the blanks.

As a result, commands like `project-find-file' now respect the
parent repo's .gitignore file even when being run from within a
subproject."
  (let* ((res (apply orig-fun args))
         (dir (nth 2 res))
         (backend (or (nth 1 res)
                      (ignore-errors (vc-responsible-backend dir)))))
    (if dir
        `(vc ,backend ,dir))))

(advice-add 'project-try-vc :around #'me/project-try-vc-subproject)

;; Compilation
(setq compilation-scroll-output t)

(define-minor-mode recompile-on-save-mode
  "When enabled, run `recompile' after the current buffer is saved"
  :lighter "recompile"
  (if recompile-on-save-mode
      (add-hook 'after-save-hook 'recompile nil t)
    (remove-hook 'after-save-hook 'recompile t)))

(use-package apheleia
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
  :config
  (setq-default eglot-workspace-configuration #'me/eglot-python-workspace-config)

  (defclass eglot-esbonio (eglot-lsp-server) ()
    :documentation "Esbonio Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-esbonio))
    "Passes the initializationOptions required to run the server."
    `(:sphinx (:confDir "${workspaceRoot}/docs"
               :srcDir "${confDir}" )
      :server (:logLevel "debug")))

  (add-to-list 'eglot-server-programs
               `(rst-mode . (eglot-esbonio
                             "/var/home/alex/Projects/esbonio/.env/bin/python"
                             "-m" "esbonio")))
  )

(use-package yaml-mode)

(provide 'alc-lang)
