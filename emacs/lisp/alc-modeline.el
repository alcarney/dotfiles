;;; alc-modeline.el --- Modeline

(column-number-mode)
(display-time-mode)
(setq display-time-format "%H:%M"
      display-time-load-average nil
      mode-line-position-column-line-format '(" %l:%c"))

(use-package minions
  :ensure t
  :config (minions-mode 1))


(defun alc-modeline-theme-tweaks (_theme)
  "Adjust how the modeline is themed.

Thanks Protesilaos!
https://git.sr.ht/~protesilaos/dotfiles/tree/8ba474ce7058cb686d9dc9ed6d1abea6d24d644e/item/emacs/.emacs.d/prot-lisp/prot-modeline.el#L558"
  (let ((line-color (face-foreground 'shadow)))
    (message "Line color is %s" line-color)
    (message "BG color id %s" (face-background 'default))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,line-color)))
     `(mode-line-inactive ((t :background unspecified :box unspecified :overline ,line-color))))))

(alc-modeline-theme-tweaks nil)
(add-hook 'enable-theme-functions #'alc-modeline-theme-tweaks)

(provide 'alc-modeline)
