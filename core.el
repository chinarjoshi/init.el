;;; core.el --- Theme, fonts, clipboard -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq inhibit-startup-echo-area-message "c"
      server-client-instructions nil
      use-package-always-demand t)

(setq my/color-black "#000000"
      my/color-dim "#0a0a0a"
      my/color-red "#e06c75"
      my/color-blue "#61afef"
      my/color-yellow "#e5c07b"
      my/color-cyan "#a1d6e2"
      my/color-green "#98c379"
      my/font-fixed "Inconsolata Nerd Font"
      my/font-variable "Inter"
      my/font-height 140
      my/title-height 1.5
      my/heading-height 1.3
      my/scroll-margin 2
      my/scroll-conservatively 101
      my/olivetti-width 80
      my/vertico-count 15
      my/corfu-delay 0
      my/corfu-prefix 1
      my/consult-async-min-input 0
      my/consult-async-delay 0.1
      my/consult-async-debounce 0.05
      my/consult-async-throttle 0.1
      my/prose-line-spacing 0.15
      my/prose-fg "#bbbbbb")

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      use-short-answers t
      vc-follow-symlinks t
      echo-keystrokes 0
      scroll-conservatively my/scroll-conservatively
      scroll-margin my/scroll-margin
      fast-but-imprecise-scrolling t
      confirm-kill-processes nil
      confirm-kill-emacs nil
      read-process-output-max (* 1024 1024)  ; 1MB for faster subprocess IO
      gc-cons-threshold (* 100 1024 1024)    ; 100MB to reduce GC pauses
      explicit-shell-file-name "/bin/bash") ; zsh-syntax-highlighting breaks eat

(setq display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default mode-line-format nil)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (dolist (face '(default fringe line-number line-number-current-line))
    (set-face-attribute face nil :background my/color-black)))
(when-let ((cell (assq 'continuation fringe-indicator-alist)))
  (setcdr cell '(nil nil)))
(when-let ((cell (assq 'truncation fringe-indicator-alist)))
  (setcdr cell '(nil nil)))

(defun my/set-fonts ()
  (set-face-attribute 'default nil :family my/font-fixed :height my/font-height)
  (set-face-attribute 'fixed-pitch nil :family my/font-fixed :height my/font-height)
  (set-face-attribute 'variable-pitch nil :family my/font-variable :height my/font-height))

(defvar my/saved-frame-geometry nil)

(defun my/save-frame-geometry (&optional frame)
  "Save current frame's position and size."
  (let ((f (or frame (selected-frame))))
    (when (frame-parameter f 'display)
      (setq my/saved-frame-geometry
            `((left . ,(frame-parameter f 'left))
              (top . ,(frame-parameter f 'top))
              (width . ,(frame-parameter f 'width))
              (height . ,(frame-parameter f 'height)))))))

(add-to-list 'default-frame-alist '(undecorated . t))

(defun my/restore-frame-geometry (frame)
  "Restore saved position and size to new frame, and raise it."
  (with-selected-frame frame
    (my/set-fonts)
    (when my/saved-frame-geometry
      (modify-frame-parameters frame my/saved-frame-geometry))
    (raise-frame frame)
    (select-frame-set-input-focus frame)))

(add-hook 'delete-frame-functions #'my/save-frame-geometry)

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/restore-frame-geometry)
  (add-hook 'after-init-hook #'my/set-fonts))

(setq interprogram-cut-function nil
      interprogram-paste-function nil)

(defvar my/clipboard-copy-cmd (if (eq system-type 'darwin) "pbcopy" "wl-copy"))
(defvar my/clipboard-paste-cmd (if (eq system-type 'darwin) "pbpaste" "wl-paste -n"))

(defun my/clipboard-get ()
  (shell-command-to-string my/clipboard-paste-cmd))

(defun my/clipboard-paste ()
  (interactive)
  (let ((content (my/clipboard-get)))
    (if (derived-mode-p 'vterm-mode)
        (vterm-send-string content)
      (insert content))))
