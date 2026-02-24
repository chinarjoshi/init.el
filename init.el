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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
