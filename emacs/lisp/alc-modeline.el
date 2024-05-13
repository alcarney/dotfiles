;;; alc-modeline.el --- Modeline configuration -*- lexical-binding: t -*-
;;; Code:

(defgroup alc-modeline nil
  "My custom modeline"
  :group 'mode-line)

(defgroup alc-modeline-faces nil
  "Faces for my custom modeline"
  :group 'alc-modeline)

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

(defvar-local alc-modeline-remote-indication
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " ☁ "
                    'face '(bold)))))
(put 'alc-modeline-remote-indication 'risky-local-variable t)

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

(defun alc-modeline-buffer-position-face ()
  "Return the face(s) to apply to the buffer position in the modeline."
  (if (mode-line-window-selected-p)
      'mode-line
    'mode-line-inactive))

(defvar-local alc-modeline-buffer-position
    '(:eval
      (propertize "%l:%c"
                  'face (alc-modeline-buffer-position-face))))
(put 'alc-modeline-buffer-position 'risky-local-variable t)

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

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                alc-modeline-window-dedicated
                alc-modeline-project-identification
                "  "
                alc-modeline-remote-indication
                alc-modeline-buffer-identification
                " "
                alc-modeline-buffer-position
                ))

(with-eval-after-load 'ef-themes
  (defun alc-modeline-apply-ef-colors ()
    "Style the modeline using colors provided by the `ef-themes'"
    (if (ef-themes--list-enabled-themes) ; Only if an ef-theme is active.
        (ef-themes-with-colors
          (set-face-attribute 'mode-line nil
                              :background bg-main
                              :overline cursor
                              :box `(:line-width 6 :color ,bg-main :style nil))
          (set-face-attribute 'mode-line-inactive nil
                              :background bg-main
                              :overline border
                              :box `(:line-width 6 :color ,bg-main :style nil))
          (set-face-attribute 'alc-modeline-project-id-face nil
                              :background bg-main
                              :foreground modeline-info))))

  (alc-modeline-apply-ef-colors)
  (add-hook 'ef-themes-post-load-hook #'alc-modeline-apply-ef-colors))

(provide 'alc-modeline)
