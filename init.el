;;; init.el --- Bootstrap literate config -*- lexical-binding: t -*-

(defvar my/modules '("core" "writing" "navigation" "keybindings" "coding"))

(defun my/load-modules ()
  "Tangle and load all config modules whose .org is newer than .el."
  (dolist (mod my/modules)
    (let ((org-file (expand-file-name (concat mod ".org") user-emacs-directory))
          (el-file (expand-file-name (concat mod ".el") user-emacs-directory)))
      (when (or (not (file-exists-p el-file))
                (file-newer-than-file-p org-file el-file))
        (require 'org)
        (org-babel-tangle-file org-file el-file))
      (load el-file nil t))))

(my/load-modules)
