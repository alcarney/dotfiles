;;; alc-isearch.el -- isearch configuration -*- lexical-binding: t -*-

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

(use-package isearch
  :bind (:map isearch-mode-map
         ("C-S-s" . alc-transient-isearch))
  :config
  (setq isearch-lazy-count t))

(provide 'alc-isearch)
