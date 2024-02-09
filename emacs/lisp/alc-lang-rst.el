;;; alc-lang-rst.el --- Settings for reStructuredText files -*- lexical-binding: t -*-

(use-package rst
  :hook ((rst-mode . eglot-ensure)
         ;; TODO: Figure out how to prevent flyspell's default keybindings from
         ;;       conflicting with the `completion-at-point' binding we want.
         ;; (rst-mode . flyspell-mode)
         (rst-mode . visual-line-mode))
  :bind (:map rst-mode-map
              ("C-M-i" . completion-at-point))
  :config
  (add-to-list 'eglot-server-programs '(rst-mode . ("esbonio"))))

(defun esbonio-preview-file ()
  "Preview the current file."
  (interactive)
  (let ((server (eglot-current-server))
        (uri  (eglot--path-to-uri buffer-file-name)))
    (if server
        (let* ((result (eglot-execute-command server "esbonio.server.previewFile"
                                              (vector `(:uri ,uri :show :json-false))))
               (uri (plist-get result :uri)))
          (eww uri t)))))

(provide 'alc-lang-rst)
