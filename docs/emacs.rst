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


Packages
^^^^^^^^

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
