;;; alc-git.el --- Configuration for git and related features -*- lexical-binding: t -*-

(use-package ediff
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

;; Thanks to this discussion: https://github.com/magit/forge/discussions/544
;; The following code is able to delegate to the gh cli in order to obtain the api token!
(cl-defun auth-source-ghcli-search (&rest spec
                                          &key backend require
                                          type max host user port
                                          &allow-other-keys)
  "Given a property list SPEC, return search matches from the `:backend'.
See `auth-source-search' for details on SPEC."
  ;; just in case, check that the type is correct (null or same as the backend)
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid GH CLI search: %s %s")

  (when-let* ((hostname (string-remove-prefix "api." host))
              ;; split ghub--ident again
              (ghub_ident (split-string (or user "") "\\^"))
              (username (car ghub_ident))
              (package (cadr ghub_ident))
              (cmd (format "gh auth token --hostname '%s'" hostname))
              (token (when (string= package "forge") (string-trim-right (shell-command-to-string cmd))))
              (retval (list
                       :host hostname
                       :user username
                       :secret token)))
    (auth-source-do-debug  "auth-source-ghcli: return %s as final result (plus hidden password)"
                           (seq-subseq retval 0 -2)) ;; remove password
    (list retval)))

(defvar auth-source-ghcli-backend
  (auth-source-backend
   :source "." ;; not used
   :type 'gh-cli
   :search-function #'auth-source-ghcli-search)
  "Auth-source backend for GH CLI.")

(defun auth-source-ghcli-backend-parse (entry)
  "Create a GH CLI auth-source backend from ENTRY."
  (when (eq entry 'gh-cli)
    (auth-source-backend-parse-parameters entry auth-source-ghcli-backend)))

(if (boundp 'auth-source-backend-parser-functions)
    (add-hook 'auth-source-backend-parser-functions #'auth-source-ghcli-backend-parse)
  (advice-add 'auth-source-backend-parse :before-until #'auth-source-ghcli-backend-parse))

(setq auth-sources '(gh-cli))

(provide 'alc-git)
