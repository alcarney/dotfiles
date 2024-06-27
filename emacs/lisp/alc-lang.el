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
  ;; TODO: Ensure isort and black use the current project's config...
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--profile=black" "--force-single-line" "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(isort black))
  (apheleia-global-mode))

(defun me/search-upwards-for-path (dirname path)
  "Search upwards from DIRNAME looking for PATH."
  (if (file-exists-p (file-name-concat dirname path))
      dirname
    (let ((parent (file-name-directory (directory-file-name dirname))))
      (if (not (string= "/" parent))
          (me/search-upwards-for-path parent path)))))


(defun me/eglot-pyright-workspace-config (server)
  "Configure pyright to use the correct virtual environment for the given SERVER."
  (if (project-current)
      (let* ((dirname   (expand-file-name (project-root (project-current))))
             (venv-path (me/search-upwards-for-path dirname ".env")))
        (if venv-path
            `(:python
              (:venvPath ,venv-path))
          '(:python.analysis
            (:logLevel "trace"))))
    '()))

(defun me/eglot-esbonio-workspace-config (server)
  "Configure esbonio to use the correct python command for the given SERVER"
  (if (project-current)
      (let* ((dirname        (expand-file-name (project-root (project-current))))
             (venv-path      (me/search-upwards-for-path dirname ".env"))
             (python-command (file-name-concat venv-path ".env/bin/python")))
        (if venv-path
            `(:esbonio
              (:sphinx
               (:pythonCommand [,python-command]))):
               '()))
    '()))

(defun me/eglot-workspace-config (server)
  "Configure eglot on a per-project basis."
  (let ((mode (car (eglot--major-modes server))))
    (cond
     ;; Esbonio
     ((eq mode 'rst-mode)
      (me/eglot-esbonio-workspace-config server))
     ;; Pyright
     ((or (eq mode 'python-ts-mode) (eq mode 'python-mode))
      (me/eglot-pyright-workspace-config server))
     ;; Fallback
     (t '()))))

(defun me/eglot-fix-workspace-configuration-scope (orig-fun server &optional path)
  "Fix the scopeUri of a workspace/configuration request.

When eglot handles a workspace/configuration request with an
associated scopeUri it uses the `file-name-directory' function to
determine the directory from which to resolve the configuration
values.

This causes an issue for servers like esbonio and pyright which
set the scopeUri to a directory. For `file-name-directory' to
treat the path as a directory it must include a trailing slash, a
convention which these servers do not follow.

Therefore `file-name-directory' treats the path as a file and
removes the final component, causing eglot to resolve the
configuration relative to the wrong directory.

This function fixes the issue by advising the
`eglot--workspace-configuration-plist' function, ensuring that
paths referencing directories include the trailing slash."
  (if (and path
           (file-directory-p path)
           (not (s-ends-with? "/" path)))
      (funcall orig-fun server (concat path "/"))
    (funcall orig-fun server path)))

(defun alc-lang-show-lsp-logs (server)
  "Display the eglot events buffer for the current server.

This function also ensures that `so-long-mode' is enabled for the
buffer to help improve performace."
  (interactive (list (eglot-current-server)))
  (if server
      (with-current-buffer (jsonrpc-events-buffer server)
        (so-long-mode)
        (display-buffer (current-buffer)))))

(use-package eglot
  :config
  (setq-default eglot-workspace-configuration #'me/eglot-workspace-config)
  (advice-add 'eglot--workspace-configuration-plist
              :around #'me/eglot-fix-workspace-configuration-scope)
  (add-to-list 'auto-mode-alist '("\\*EGLOT .* events\\*" . so-long-mode)))

(use-package dape
  ;; To use window configuration like gud (gdb-mi)
  ;; :init
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  )

(use-package yaml-mode)

(provide 'alc-lang)
