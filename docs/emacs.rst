Emacs
-----

This is where my emacs config goes.

.. contents::
   :depth: 2
   :local:

Early Init
^^^^^^^^^^

If Emacs sees an ``early-init.el`` file, it will execute that first before proceeding to the standard ``init.el`` file.
I stil don't really know what should go here... but I do see some people use it for GUI related options

:filename: emacs/early-init.el

.. code:: elisp

   ;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

   ;; Disable GUI things
   (blink-cursor-mode -1)
   (scroll-bar-mode -1)
   (tool-bar-mode -1)

   ;; And enable others
   (show-paren-mode t)
   (menu-bar-mode t)

   (setq frame-inhibit-implied-resize t
         inhibit-x-resources t
         inhibit-startup-message t)

Languages
^^^^^^^^^

reStructuredText
""""""""""""""""

:filename: emacs/lisp/alc-lang-rst.el

.. code:: elisp

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


The following command implements the ability to preview the current file via ``esbonio``

.. code:: elisp

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

The required ``(provide FEATURE)`` footer.

.. code:: elisp

   (provide 'alc-lang-rst)


Packages
^^^^^^^^

Configuration for any remaining packages that don't fit into the above categories.

Eat
"""

:filename: emacs/lisp/alc-terminals.el

.. code:: elisp

   ;;; alc-terminals.el --- (e)Shell, term, eat, oh my! -*- lexical-binding: t -*-

   (use-package eat
     :config
     (with-eval-after-load 'project
       (keymap-set project-prefix-map "t" #'eat-project)))

   (provide 'alc-terminals)
