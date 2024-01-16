;;; alc-git.el --- Configuration for git and related features -*- lexical-binding: t -*-

(use-package magit
  :bind (("C-x g" . magit-status)))

(provide 'alc-git)
