;;; init.el --- Bootstrap literate config -*- lexical-binding: t -*-

(defvar my/config-file (expand-file-name "README.org" user-emacs-directory))
(defvar my/tangled-file (expand-file-name "config.el" user-emacs-directory))

(when (or (not (file-exists-p my/tangled-file))
          (file-newer-than-file-p my/config-file my/tangled-file))
  (require 'org)
  (org-babel-tangle-file my/config-file my/tangled-file))

(load my/tangled-file nil t)
