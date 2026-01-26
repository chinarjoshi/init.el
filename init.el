;;; init.el --- Bootstrap literate config -*- lexical-binding: t -*-

(defvar my/config-file (expand-file-name "README.org" user-emacs-directory))
(defvar my/tangled-file (expand-file-name "config.el" user-emacs-directory))

(when (or (not (file-exists-p my/tangled-file))
          (file-newer-than-file-p my/config-file my/tangled-file))
  (require 'org)
  (org-babel-tangle-file my/config-file my/tangled-file))

(load my/tangled-file nil t)
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
