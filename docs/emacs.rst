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


Appearance
^^^^^^^^^^


Modeline
""""""""

.. admonition:: References

   - Protesilaos' tutorial on `writing custom mode lines <https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial/>`__

:filename: emacs/lisp/alc-modeline.el

.. code:: elisp

   ;;; alc-modeline.el --- Modeline configuration -*- lexical-binding: t -*-
   ;;; Code:

   (defgroup alc-modeline nil
     "My custom modeline"
     :group 'mode-line)

   (defgroup alc-modeline-faces nil
     "Faces for my custom modeline"
     :group 'alc-modeline)


**Project Indentification**

If the current buffer is associated with a project, show the name of the project.

.. code:: elisp

   (defface alc-modeline-project-id-face
     '((default :inherit (bold)))
     "Face for styling the project indicator"
     :group 'alc-modeline-faces)

   (defvar-local alc-modeline-project-identification
    '(:eval
      (if-let ((pr (project-current))
               (file (buffer-file-name)))
          (propertize (format "🖿 %s " (project-name pr))
                      'face 'alc-modeline-project-id-face))))
   (put 'alc-modeline-project-identification 'risky-local-variable t)

**Remote Indication**

Replaces the default ``mode-line-remote`` and indicates if the current buffer is visiting a remote file

.. code:: elisp

   (defvar-local alc-modeline-remote-indication
       '(:eval
          (when (file-remote-p default-directory)
            (propertize " ☁ "
                        'face '(bold)))))
   (put 'alc-modeline-remote-indication 'risky-local-variable t)

**Buffer Identification**

Intended to replace the default ``mode-line-buffer-identification`` and ``mode-line-modified`` components this displays the name of the buffer and a face depending on if the buffer is unsaved, read only etc.

.. code:: elisp

   (defun alc-modeline-buffer-identification-face ()
     "Return the face(s) to apply to the buffer name in the modeline."
     (cond ((and (buffer-file-name)
                 (buffer-modified-p))
            'error)
           (buffer-read-only '(italic mode-line-buffer-id))
           (t 'mode-line-buffer-id)))

   (defvar-local alc-modeline-buffer-identification
       '(:eval
          (propertize "%b"
                      'face (alc-modeline-buffer-identification-face))))
   (put 'alc-modeline-buffer-identification 'risky-local-variable t)

**Dedidcated Windows**

Indicates if the current window is dedicated.

.. code:: elisp

   (defface alc-modeline-window-dedicated-face
     '((default :inherit (bold)))
     "Face for styling the dedicated window indicator"
     :group 'alc-modeline-faces)

   (defvar-local alc-modeline-window-dedicated
       '(:eval
         (when (window-dedicated-p)
           (propertize "🖈 "
                       'face 'alc-modeline-window-dedicated-face))))
   (put 'alc-modeline-window-dedicated 'risky-local-variable t)

For reference, here are the components that were in the default modeline

- ``mode-line-mule-info``
- ``mode-line-client``
- ``mode-line-frame-identification``
- ``mode-line-position``
- ``mode-line-misc-info``
- ``mode-line-end-spaces``

**Default Modeline**

Finally, here is my default modeline definition

.. code:: elisp

   (setq-default mode-line-format
                 '("%e"
                   mode-line-front-space
                   alc-modeline-window-dedicated
                   alc-modeline-project-identification
                   "  "
                   alc-modeline-remote-indication
                   alc-modeline-buffer-identification
                   ))

**Modeline Styles**

The following snippet applies styles to the modeline that are derived from colors provided by the ``ef-themes``

.. code:: elisp

   (with-eval-after-load 'ef-themes
     (defun alc-modeline-apply-ef-colors ()
       "Style the modeline using colors provided by the `ef-themes'"
       (if (ef-themes--list-enabled-themes) ; Only if an ef-theme is active.
           (ef-themes-with-colors
             (set-face-attribute 'alc-modeline-project-id-face nil :background bg-main :foreground modeline-info))))

     (alc-modeline-apply-ef-colors)
     (add-hook 'ef-themes-post-load-hook #'alc-modeline-apply-ef-colors))


.. code:: elisp

   (provide 'alc-modeline)

Tab Bar
"""""""

:filename: emacs/lisp/alc-tab-bar.el

.. code:: elisp

   ;;; alc-tab-bar.el --- Tab bar configuration -*- lexical-binding: t -*-

   (use-package tab-bar
     :config
     (setq tab-bar-show 0  ; Always show the tab-bar
           ;; Don't show the tabs themselves, use tab-bar like a panel.
           tab-bar-format '(tab-bar-format-align-right tab-bar-format-global))
     (tab-bar-mode))


**Time and Date**

Thanks to adding ``tab-bar-format-global`` to ``tab-bar-format``, ``display-time-mode`` will show the time and date in the tab-bar.

**Battery Level**

.. code:: elisp

   (use-package battery
     :config
     (display-battery-mode))

.. code:: elisp

   (use-package time
     :config
     (setq display-time-format "%H:%M %d/%m/%y"
           display-time-day-and-date nil
           display-time-default-load-average nil)
     (display-time-mode))

.. code:: elisp

   (provide 'alc-tab-bar)


Theme
"""""

:filename: emacs/lisp/alc-theme.el

.. code:: elisp

   ;;; alc-theme.el --- Themes and related settings -*- lexical-binding: t -*-


**ef-themes**

.. code:: elisp

   (use-package ef-themes
     :bind ("<f5>" . ef-themes-toggle)
     :init
     (setq ef-themes-mixed-fonts t
           ef-themes-variable-pitch-ui t
           ef-themes-to-toggle '(ef-elea-light ef-elea-dark))
     (ef-themes-select 'ef-elea-light))

**Spacious Padding**

Another great package from Protesilaos, gives just a little bit more breathing room to UI elements.

.. code:: elisp

   (use-package spacious-padding
     :config
     (setq spacious-padding-subtle-mode-line t
           spacious-padding-widths '(:internal-border-width 1
                                     :header-line-width 4
                                     :mode-line-width 6
                                     :tab-width 1
                                     :right-divider-width 30
                                     :scroll-bar-width 8))
     (spacious-padding-mode 1))


.. code:: elisp

   (provide 'alc-theme)

Applications
^^^^^^^^^^^^

Configuration for packages that provide an entire application's worth of functionality.

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


Builtins
^^^^^^^^

Configuration for builtin functionality

isearch
"""""""

:filename: emacs/lisp/alc-isearch.el

.. code:: elisp

   ;;; alc-isearch.el -- isearch configuration -*- lexical-binding: t -*-


Thanks to Charles Choi for the `idea <http://yummymelon.com/devnull/improving-emacs-isearch-usability-with-transient.html>`__ of exposing isearch utility functions via a transient menu.

.. code:: elisp

   (require 'transient)

   (transient-define-prefix alc-transient-isearch ()
     "isearch"
     [["Search Term"
       ("w" "Expand to next word"      isearch-yank-word-or-char)
       ("s" "Expand to next symbol"    isearch-yank-symbol-or-char)
       ("l" "Expand to end of line"    isearch-yank-line)
       ("y" "Use kill ring"            isearch-yank-kill)
       ("." "Expand to thing-at-point" isearch-forward-thing-at-point)]

      ["Replace"
       :if-nil buffer-read-only
       ("r" "Replace"          isearch-query-replace)
       ("R" "Replace (regexp)" isearch-query-replace-regexp)]

      ["Ignore"
       ("i c" "ignore case"       isearch-toggle-case-fold)
       ("i w" "ignore whitespace" isearch-toggle-lax-whitespace)]])


.. code:: elisp

   (use-package isearch
     :bind (:map isearch-mode-map
            ("C-S-s" . alc-transient-isearch))
     :config
     (setq isearch-lazy-count t))

   (provide 'alc-isearch)

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
