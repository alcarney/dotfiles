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

(define-key global-map (kbd "'") #'my/surround-or-self-insert)
(define-key global-map (kbd "\"") #'my/surround-or-self-insert)
(define-key global-map (kbd "`") #'my/surround-or-self-insert)
(define-key global-map (kbd "[") #'my/surround-or-self-insert)
(define-key global-map (kbd "{") #'my/surround-or-self-insert)
(define-key global-map (kbd "(") #'my/surround-or-self-insert)
(define-key global-map (kbd "<") #'my/surround-or-self-insert)

(provide 'alc-editing)
