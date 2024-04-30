;;; alc-lang-python.el --- Python configuration

(defun alc-python-library-file-p ()
  "Determine if the current buffer is a library file"
  (if-let ((file-name (buffer-file-name)))
      (or
       (string-match-p "site-packages/" file-name)
       (string-match-p "typeshed-fallback/" file-name)
       (string-match-p "/usr/lib\\(64\\)?/" file-name))
    ;; For now, consider buffers that do not visit a file a "library" as well
    t))

(defun alc-python-get-interpreter-path ()
  "Locate the path to the python interpreter for the current file"
  (when-let ((root (locate-dominating-file default-directory ".env"))
             (python (file-name-concat root ".env/bin/python"))
             (_ (file-exists-p python)))
    python))

(defun alc-python-mode-hook ()
  "Tweaks and config to run when starting `python-mode'"
  (setq-local fill-column 88)

  ;; Files in site-packages/ etc. should be read only by default.
  ;; Also do not start eglot in these locations to cut down on the
  ;; number of server instances.
  (if (alc-python-library-file-p)
      (read-only-mode)
    (eglot-ensure)))

(use-package python
  :hook ((python-mode . alc-python-mode-hook)
         (python-ts-mode . alc-python-mode-hook))
  :config
  (setq python-shell-dedicated 'project))

(provide 'alc-lang-python)
