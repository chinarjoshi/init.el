;;; early-init.el --- Early init -*- lexical-binding: t -*-

;; Faster startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Disable package.el (we use use-package with :ensure)
(setq package-enable-at-startup nil)

;; Prevent UI flash before init.el disables them
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; No title bar
(push '(undecorated . t) default-frame-alist)

;; Disable mode-line early
(setq-default mode-line-format nil)

;; No startup screens
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
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
