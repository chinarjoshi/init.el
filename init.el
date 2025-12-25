;;; init.el --- Minimal Emacs config -*- lexical-binding: t -*-

;;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default mode-line-format nil)
(global-display-line-numbers-mode 1)

;;; Fonts
(set-face-attribute 'default nil :family "Inconsolata" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :family "Garamond")

;;; Org
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Periodic notes (like Obsidian)
(defvar notes-directory "~/notes")

(defun notes--daily-file (time)
  (expand-file-name (format-time-string "daily/%m-%d.org" time) notes-directory))

(defun notes--weekly-file (time)
  (expand-file-name (format-time-string "weekly/%V.org" time) notes-directory))

(defun notes--monthly-file (time)
  (expand-file-name (format-time-string "monthly/%B.org" time) notes-directory))

(defun notes--ensure-dir (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun notes-open-daily ()
  (interactive)
  (let ((file (notes--daily-file (current-time))))
    (notes--ensure-dir file)
    (find-file file)))

(defun notes-open-weekly ()
  (interactive)
  (let ((file (notes--weekly-file (current-time))))
    (notes--ensure-dir file)
    (find-file file)))

(defun notes-open-monthly ()
  (interactive)
  (let ((file (notes--monthly-file (current-time))))
    (notes--ensure-dir file)
    (find-file file)))

(defun notes--current-type ()
  (when buffer-file-name
    (let ((name (file-name-nondirectory buffer-file-name))
          (dir (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))))
      (cond
       ((string= dir "daily") 'daily)
       ((string= dir "weekly") 'weekly)
       ((string= dir "monthly") 'monthly)))))

(defun notes--parse-date ()
  (let ((name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
        (type (notes--current-type))
        (year (string-to-number (format-time-string "%Y"))))
    (pcase type
      ('daily (let ((parts (split-string name "-")))
                (encode-time 0 0 0
                             (string-to-number (cadr parts))
                             (string-to-number (car parts))
                             year)))
      ('weekly (let ((week (string-to-number name)))
                 (encode-time 0 0 0 (1+ (* (1- week) 7)) 1 year)))
      ('monthly (encode-time 0 0 0 1
                             (1+ (seq-position
                                  '("January" "February" "March" "April" "May" "June"
                                    "July" "August" "September" "October" "November" "December")
                                  name #'string=))
                             year)))))

(defun notes-prev ()
  (interactive)
  (let ((type (notes--current-type))
        (time (notes--parse-date)))
    (when type
      (let* ((offset (pcase type
                       ('daily (days-to-time 1))
                       ('weekly (days-to-time 7))
                       ('monthly (days-to-time 30))))
             (new-time (time-subtract time offset))
             (file (pcase type
                     ('daily (notes--daily-file new-time))
                     ('weekly (notes--weekly-file new-time))
                     ('monthly (notes--monthly-file new-time)))))
        (notes--ensure-dir file)
        (find-file file)))))

(defun notes-next ()
  (interactive)
  (let ((type (notes--current-type))
        (time (notes--parse-date)))
    (when type
      (let* ((offset (pcase type
                       ('daily (days-to-time 1))
                       ('weekly (days-to-time 7))
                       ('monthly (days-to-time 30))))
             (new-time (time-add time offset))
             (file (pcase type
                     ('daily (notes--daily-file new-time))
                     ('weekly (notes--weekly-file new-time))
                     ('monthly (notes--monthly-file new-time)))))
        (notes--ensure-dir file)
        (find-file file)))))

(global-set-key (kbd "|") 'notes-open-daily)
(global-set-key (kbd "C-\\") 'notes-open-weekly)
(global-set-key (kbd "C-|") 'notes-open-monthly)
(global-set-key (kbd "M-[") 'notes-prev)
(global-set-key (kbd "M-]") 'notes-next)

;; Pieces
(defvar pieces-directory (expand-file-name "pieces" notes-directory))

(defun pieces--find-file (act piece)
  (let* ((act-dir (expand-file-name (format "Act %d" act) pieces-directory))
         (pattern (format "^%d_.*\\.org$" piece))
         (files (and (file-directory-p act-dir)
                     (directory-files act-dir t pattern))))
    (if files
        (car files)
      (expand-file-name (format "%d_.org" piece) act-dir))))

(defun pieces-open-or-capture (act piece)
  (let ((file (pieces--find-file act piece)))
    (pieces--ensure-dir file)
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-max))
            (insert "\n" text)
            (save-buffer))
          (deactivate-mark)
          (message "Captured to %s" (file-name-nondirectory file)))
      (find-file file))))

(defun pieces--ensure-dir (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun pieces--make-binding (act piece)
  (lambda () (interactive) (pieces-open-or-capture act piece)))

(defun pieces-search ()
  (interactive)
  (let* ((files (directory-files-recursively pieces-directory "\\.org$"))
         (names (mapcar (lambda (f) (file-relative-name f pieces-directory)) files))
         (selected (completing-read "Piece: " names nil t))
         (file (expand-file-name selected pieces-directory)))
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-max))
            (insert "\n" text)
            (save-buffer))
          (deactivate-mark)
          (message "Captured to %s" selected))
      (find-file file))))

;; Piece tags
(defun pieces--name-to-tag (filename)
  "Convert '3_The Big Fight.org' to 'TheBigFight'."
  (let* ((base (file-name-sans-extension (file-name-nondirectory filename)))
         (name (if (string-match "^[0-9]+_\\(.*\\)" base)
                   (match-string 1 base)
                 base)))
    (replace-regexp-in-string " " "" name)))

(defun pieces--tag-to-file (tag)
  "Find file matching tag 'TheBigFight'."
  (let ((files (directory-files-recursively pieces-directory "\\.org$")))
    (seq-find (lambda (f) (string= tag (pieces--name-to-tag f))) files)))

(defun pieces--all-tags ()
  "Get all piece tags."
  (let ((files (directory-files-recursively pieces-directory "\\.org$")))
    (mapcar #'pieces--name-to-tag files)))

(defun pieces-update-tags ()
  "Update org-tag-alist with piece tags."
  (interactive)
  (setq org-tag-alist
        (mapcar (lambda (tag) (cons tag ?p)) (pieces--all-tags)))
  (message "Updated %d piece tags" (length org-tag-alist)))

(defun pieces-goto-tag-at-point ()
  "Jump to piece file for tag at point."
  (interactive)
  (let* ((tag (org-get-tags nil t))
         (piece-tags (pieces--all-tags))
         (found (seq-find (lambda (t) (member t piece-tags)) tag)))
    (if found
        (find-file (pieces--tag-to-file found))
      (message "No piece tag at point"))))

(defun pieces-show-backlinks ()
  "Show all daily entries tagged with current piece."
  (interactive)
  (let* ((tag (pieces--name-to-tag buffer-file-name))
         (daily-dir (expand-file-name "daily" notes-directory)))
    (consult-ripgrep daily-dir (format ":%s:" tag))))

;; Auto-update tags when entering org-mode in notes
(add-hook 'org-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-prefix-p (expand-file-name notes-directory)
                                        (expand-file-name buffer-file-name)))
              (pieces-update-tags))))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t))

(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 80))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 80)
  (setq visual-fill-column-center-text t))

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Completion (minibuffer)
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)))

;;; Navigation
(use-package avy
  :ensure t)

(use-package vi-tilde-fringe
  :ensure t
  :hook (prog-mode . vi-tilde-fringe-mode))

;;; Completion (in-buffer)
(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 1)
  (global-corfu-mode 1))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;;; Wayland clipboard
(setq interprogram-cut-function
      (lambda (text)
        (call-process "wl-copy" nil 0 nil text)))

(setq interprogram-paste-function
      (lambda ()
        (when (zerop (call-process "wl-paste" nil nil nil "-n"))
          (with-temp-buffer
            (call-process "wl-paste" nil t nil "-n")
            (buffer-string)))))

;;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-numbers
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)
              ("C-x" . evil-numbers/dec-at-pt)))

;;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;;; General (leader key)
(use-package general
  :ensure t
  :after evil
  :config
  (general-create-definer leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")

  (leader-def
    "" '(nil :which-key "leader")
    "SPC" '(pieces-search :which-key "pieces")
    "." '(find-file :which-key "find file")
    "," '(switch-to-buffer :which-key "switch buffer")
    ";" '(execute-extended-command :which-key "M-x")
    "x" '(scratch-buffer :which-key "scratch")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "switch")
    "bd" '(kill-current-buffer :which-key "kill")
    "bs" '(save-buffer :which-key "save")

    ;; File
    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find")
    "fs" '(save-buffer :which-key "save")
    "fr" '(consult-recent-file :which-key "recent")

    ;; Search
    "s" '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "line")
    "sp" '(consult-ripgrep :which-key "project")
    "si" '(consult-imenu :which-key "imenu")
    "so" '(consult-outline :which-key "outline")

    ;; Jump
    "j" '(:ignore t :which-key "jump")
    "jj" '(avy-goto-char-2 :which-key "char")
    "jl" '(avy-goto-line :which-key "line")
    "jw" '(avy-goto-word-1 :which-key "word")

    ;; Git
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "status")
    "gb" '(magit-blame :which-key "blame")
    "gl" '(magit-log-current :which-key "log")

    ;; Window
    "w" '(:ignore t :which-key "window")
    "wv" '(split-window-right :which-key "vsplit")
    "ws" '(split-window-below :which-key "split")
    "wd" '(delete-window :which-key "delete")
    "ww" '(other-window :which-key "other")

    ;; Quit
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit")

    ;; Pieces navigation
    "p" '(:ignore t :which-key "piece")
    "pp" '(pieces-goto-tag-at-point :which-key "goto tag")
    "pb" '(pieces-show-backlinks :which-key "backlinks")
    "pt" '(org-set-tags-command :which-key "set tags")

    ;; Pieces (Acts 1-9, Pieces 1-9)
    "1" '(:ignore t :which-key "Act 1")
    "2" '(:ignore t :which-key "Act 2")
    "3" '(:ignore t :which-key "Act 3")
    "4" '(:ignore t :which-key "Act 4")
    "5" '(:ignore t :which-key "Act 5")
    "6" '(:ignore t :which-key "Act 6")
    "7" '(:ignore t :which-key "Act 7")
    "8" '(:ignore t :which-key "Act 8")
    "9" '(:ignore t :which-key "Act 9"))

  ;; Generate piece bindings for each act
  (dolist (act (number-sequence 1 9))
    (dolist (piece (number-sequence 1 9))
      (leader-def
        (format "%d%d" act piece)
        (list (pieces--make-binding act piece)
              :which-key (format "piece %d" piece))))))

;;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

;;; Eglot + Languages
(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure)
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'go-mode)
                (eglot-format-buffer)
                (eglot-code-action-organize-imports (point-min) (point-max))))))

;; Python (uses pyright or pylsp)
(add-hook 'python-mode-hook 'eglot-ensure)

;; Shell (uses bash-language-server)
(add-hook 'sh-mode-hook 'eglot-ensure)

;; Web development
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.vue\\'")
  :hook (web-mode . eglot-ensure)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook (js2-mode . eglot-ensure)
  :config
  (setq js2-basic-offset 2))

;; CSS (built-in)
(add-hook 'css-mode-hook 'eglot-ensure)

;; JSON (built-in)
(add-hook 'json-mode-hook 'eglot-ensure)

;; Lua
(use-package lua-mode
  :ensure t
  :hook (lua-mode . eglot-ensure)
  :config
  (setq lua-indent-level 2))

;;; Treesitter
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))
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
