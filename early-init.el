;;; early-init.el --- Early init -*- lexical-binding: t -*-

;; Faster startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Disable package.el (we use use-package with :ensure)
(setq package-enable-at-startup nil)

;; Set default frame parameters immediately (prevents white flash)
(setq default-frame-alist
      '((background-color . "#000000")
        (foreground-color . "#ffffff")
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars)
        (undecorated . t)
        (font . "Inconsolata Nerd Font-13")))

;; Disable mode-line early
(setq-default mode-line-format nil)

;; No startup screens
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "c")
(setq initial-scratch-message nil)

;; No warnings from native comp
(setq native-comp-async-report-warnings-errors 'silent)

;; Don't resize frame at startup
(setq frame-inhibit-implied-resize t)

;; Restore gc after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))
