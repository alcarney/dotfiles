;;; alc-window.el --- Settings for windows -*- lexical-binding: t -*-

;; Move between windows using M-{left,right,up,down}
(windmove-default-keybindings 'meta)

;; Left/right side windows should occupy the full height of the frame
(setq window-sides-vertical t)

;; Ensure that buffers like shell/eshell etc open at the bottom.
;; Needed since the implementation of `shell' calls
;; '(pop-to-buffer buffer display-comint-buffer-action)' which appears to take precedence over
;; `display-buffer-alist'
(setopt display-comint-buffer-action
        '((display-buffer-reuse-mode-window display-buffer-in-side-window)
          (side . bottom)
          (slot . 0)
          (dedicated . t)))

(setq display-buffer-alist
      `(
        ;; forge.el posts and replies
        ((derived-mode . forge-post-mode)
         (display-buffer-below-selected)
         (dedicated . t))

        ;; Shells, eshells, inferior-python-mode and more
        ((derived-mode . comint-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (dedicated . t))

        ;; eat terminals
        ((derived-mode . eat-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (dedicated . t))

        ;; Compilation buffers
        ((derived-mode . compilation-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (dedicated . t))

        ;; Flymake buffers
        ((derived-mode . flymake-diagnostics-buffer-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (dedicated . t))

        ((derived-mode . flymake-project-diagnostics-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (dedicated . t))

        ;; magit-process
        ((derived-mode . magit-process-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (dedicated . t))

        ;; dired
        ;; ((derived-mode . dired-mode)
        ;;  (display-buffer-reuse-mode-window display-buffer-in-side-window)
        ;;  (side . left)
        ;;  (slot . 0)
        ;;  (dedicated . t))
        ))

(provide 'alc-window)
