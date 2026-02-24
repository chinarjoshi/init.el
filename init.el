;;; init.el --- Bootstrap literate config -*- lexical-binding: t -*-

(defvar my/modules '("core" "writing" "navigation" "keybindings" "coding"))
(defvar my/config-file (expand-file-name "config.el" user-emacs-directory))

(defun my/load-modules ()
  "Tangle all config modules into config.el and load it."
  (let* ((org-files (mapcar (lambda (m) (expand-file-name (concat m ".org") user-emacs-directory)) my/modules))
         (needs-tangle (or (not (file-exists-p my/config-file))
                           (cl-some (lambda (f) (file-newer-than-file-p f my/config-file)) org-files))))
    (when needs-tangle
      (require 'org)
      (delete-file my/config-file nil)
      (dolist (f org-files)
        (org-babel-tangle-file f my/config-file)))
    (load my/config-file nil t)))

(my/load-modules)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
