;;; alc-window.el --- Settings for windows

(use-package tab-bar
  :config
  ;; Never show the tab-bar
  (setq tab-bar-show nil))

(setq display-buffer-alist
      `(
        ;; Put flymake buffers at the bottom - like VSCode's panel.
        ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                (derived-mode . flymake-project-diagnostics-mode)
                (major-mode . shell-mode)))  ;; TODO: Why does this only work sometimes?
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))
        ))

(provide 'alc-window)
