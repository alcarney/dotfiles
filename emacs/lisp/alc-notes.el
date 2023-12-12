;;; alc-notes.el --- Note taking.

(use-package denote
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config

  (defun alc-denote-format-keywords-for-rst (keywords)
    (string-join keywords ", "))

  ;; Add support for reStructuredText to denote.
  (add-to-list 'denote-file-types `(rst
                                    :extension ".rst"
                                    :date-function denote-date-iso-8601
                                    :front-matter ":title: %s\n:date: %s\n:tags: %s\n:identifier: %s\n\n"
                                    :title-key-regexp "^:title:"
                                    :title-value-function identity
                                    :title-value-reverse-function denote-trim-whitespace
                                    :keywords-key-regexp "^:tags:"
                                    :keywords-value-function alc-denote-format-keywords-for-rst
                                    :keywords-value-reverse-function denote-extract-keywords-from-front-matter
                                    :link ":note:`%2$s <%1$s>`"
                                    :link-in-context-regexp ,(concat ":note:`.*?<\\(?1:" denote-id-regexp "\\)>`")))

  (setq denote-directory (expand-file-name "~/Notes")
        denote-dired-directories (list denote-directory)
        denote-file-type 'rst))

(provide 'alc-notes)
