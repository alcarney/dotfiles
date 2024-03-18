;;; alc-editing.el --- General editing config


(defun define-pair (open &optional close)
  "Define an open-close pair of characters."
  (list
   (string-to-char open)
   (string-to-char (or close open))))

(setq insert-pair-alist `(,(define-pair "'")
                          ,(define-pair "\"")
                          ,(define-pair "`")
                          ,(define-pair "(" ")")
                          ,(define-pair "{" "}")
                          ,(define-pair "[" "]")
                          ,(define-pair "<" ">")))

(defun my/surround-or-self-insert (arg)
  "Insert the typed character, unless the region is active, then
surround it with the character instead.

This is used to mimic the behavior you see in VSCode, where you
can easily surround a region with some characters. Uses
`insert-pair' which means it respects the `insert-pair-alist'."
  (interactive "P")
  (if (use-region-p)
      (insert-pair)
    (self-insert-command (or arg 1))))

(keymap-set global-map "'" #'my/surround-or-self-insert)
(keymap-set global-map "\"" #'my/surround-or-self-insert)
(keymap-set global-map "`" #'my/surround-or-self-insert)
(keymap-set global-map "[" #'my/surround-or-self-insert)
(keymap-set global-map "{" #'my/surround-or-self-insert)
(keymap-set global-map "(" #'my/surround-or-self-insert)
(keymap-set global-map "<" #'my/surround-or-self-insert)

(use-package whitespace
  :init
  (setq whitespace-style '(face empty tab-mark trailing missing-newline-at-eof))
  (setq-default indent-tabs-mode nil)
  (global-whitespace-mode)
  (add-hook 'before-save-hook #'whitespace-cleanup))

(use-package combobulate
  :preface
  (setq combobulate-key-prefix "C-c o")

  :hook
  ((python-ts-mode . combobulate-mode)))

(provide 'alc-editing)
