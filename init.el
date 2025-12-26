;;; init.el --- Minimal Emacs config -*- lexical-binding: t -*-

;;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Backups and autosave
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;; Use y/n instead of yes/no
(setq use-short-answers t)

;;; Don't prompt about unsaved buffers on quit
(setq confirm-kill-processes nil)
(setq confirm-kill-emacs nil)
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (set-buffer-modified-p nil)))
            t))

;;; UI
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq modus-themes-common-palette-overrides
      '((bg-main "#000000")
        (bg-dim "#0a0a0a")
        (bg-alt "#0a0a0a")))
(load-theme 'modus-vivendi t)
(set-face-attribute 'fringe nil :background "#000000")
(set-face-attribute 'line-number nil :background "#000000")
(set-face-attribute 'line-number-current-line nil :background "#000000")
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))

;;; Fonts
(defun my/set-fonts ()
  (set-face-attribute 'default nil :family "Inconsolata Nerd Font" :height 130)
  (set-face-attribute 'fixed-pitch nil :family "Inconsolata Nerd Font")
  (set-face-attribute 'variable-pitch nil :family "Lora"))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my/set-fonts))))
  (add-hook 'after-init-hook #'my/set-fonts))

;;; Org
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'auto-save-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-block-end-line ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch))))
 '(org-meta-line ((t (:inherit fixed-pitch))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch)))))

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
  (let* ((time (current-time))
         (file (notes--daily-file time))
         (day-name (format-time-string "%A" time))
         (new-file (not (file-exists-p file))))
    (notes--ensure-dir file)
    (find-file file)
    (when (and new-file (= (buffer-size) 0))
      (insert (format "* :%s:\n" day-name))
      (save-buffer))))

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
  (let ((fn (lambda () (interactive) (pieces-open-or-capture act piece))))
    (defalias (intern (format "pieces-open-%d-%d" act piece)) fn)
    (intern (format "pieces-open-%d-%d" act piece))))

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
  (setq olivetti-body-width 100)
  (set-face-attribute 'olivetti-fringe nil :background "#000000"))

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package org-transclusion
  :ensure t
  :after org
  :config
  (set-face-attribute 'org-transclusion-fringe nil :foreground "dim gray")
  (set-face-attribute 'org-transclusion nil
                      :background nil
                      :foreground "gray60"))

;; Auto-transclusion for periodic notes
(defun notes--week-days (week-num year)
  "Return list of (month day) pairs for a given ISO week."
  (let* ((jan4 (encode-time 0 0 0 4 1 year))
         (jan4-dow (string-to-number (format-time-string "%u" jan4)))
         (week1-mon (time-subtract jan4 (days-to-time (1- jan4-dow))))
         (week-mon (time-add week1-mon (days-to-time (* 7 (1- week-num)))))
         (days '()))
    (dotimes (i 7)
      (let ((day (time-add week-mon (days-to-time i))))
        (push (cons (string-to-number (format-time-string "%m" day))
                    (string-to-number (format-time-string "%d" day)))
              days)))
    (nreverse days)))

(defun notes--month-weeks (month-name year)
  "Return list of week numbers that have days in the given month."
  (let* ((months '("January" "February" "March" "April" "May" "June"
                   "July" "August" "September" "October" "November" "December"))
         (month-num (1+ (seq-position months month-name #'string=)))
         (weeks '()))
    (dotimes (week 53)
      (let ((days (notes--week-days (1+ week) year)))
        (when (seq-find (lambda (d) (= (car d) month-num)) days)
          (push (1+ week) weeks))))
    (nreverse weeks)))

(defun notes--get-first-heading (file)
  "Get the first heading text from FILE for transclusion target."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^\\*+[ \t]+\\(.*\\)$" nil t)
      (match-string 1))))

(defun notes--update-transclusions ()
  "Update transclusion links in weekly/monthly notes."
  (when buffer-file-name
    (let* ((name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
           (dir (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))))
           (year (string-to-number (format-time-string "%Y"))))
      (cond
       ;; Weekly note - transclude daily notes (first heading only)
       ((string= dir "weekly")
        (let* ((week-num (string-to-number name))
               (days (notes--week-days week-num year))
               (daily-dir (expand-file-name "daily" notes-directory))
               (links '()))
          (dolist (day days)
            (let ((file (expand-file-name (format "%02d-%02d.org" (car day) (cdr day)) daily-dir)))
              (when (file-exists-p file)
                (let ((heading (notes--get-first-heading file)))
                  (when heading
                    (push (format "#+transclude: [[file:%s::*%s]]" file heading) links))))))
          (notes--replace-transclusions (nreverse links))))
       ;; Monthly note - transclude weekly notes (first heading only)
       ((string= dir "monthly")
        (let* ((weeks (notes--month-weeks name year))
               (weekly-dir (expand-file-name "weekly" notes-directory))
               (links '()))
          (dolist (week weeks)
            (let ((file (expand-file-name (format "%02d.org" week) weekly-dir)))
              (when (file-exists-p file)
                (let ((heading (notes--get-first-heading file)))
                  (when heading
                    (push (format "#+transclude: [[file:%s::*%s]]" file heading) links))))))
          (notes--replace-transclusions (nreverse links))))))))

(defun notes--replace-transclusions (links)
  "Replace transclusion block in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((start (if (re-search-forward "^# BEGIN TRANSCLUSIONS$" nil t)
                     (line-beginning-position)
                   nil))
          (end (if (re-search-forward "^# END TRANSCLUSIONS$" nil t)
                   (line-end-position)
                 nil)))
      (if (and start end)
          (delete-region start (1+ end))
        (goto-char (point-max))))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "# BEGIN TRANSCLUSIONS\n")
    (dolist (link links)
      (insert link "\n"))
    (insert "# END TRANSCLUSIONS\n")
    (save-buffer)))

(defun notes--periodic-note-p ()
  (when buffer-file-name
    (let ((dir (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))))
      (member dir '("weekly" "monthly")))))

(add-hook 'find-file-hook
          (lambda ()
            (when (notes--periodic-note-p)
              (notes--update-transclusions)
              (org-transclusion-mode 1))))

;;; Completion (minibuffer)
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides nil)
  (setq completion-category-defaults nil))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer))
  :config
  (setq consult-preview-key "any"))

;;; Navigation
(use-package avy
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package dirvish
  :ensure t
  :after nerd-icons
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes '(nerd-icons file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-side-width 35)
  :bind
  (:map dirvish-mode-map
        ("a" . dirvish-quick-access)
        ("TAB" . dirvish-subtree-toggle)
        ("q" . dirvish-quit)))

(use-package vi-tilde-fringe
  :ensure t
  :hook (prog-mode . vi-tilde-fringe-mode))

;;; Completion (in-buffer)
(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 3)
  (global-corfu-mode 1)
  (add-hook 'org-mode-hook (lambda () (corfu-mode -1))))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

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
  (setq which-key-idle-delay 0)
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
    "TAB" '(vterm :which-key "terminal")
    "\\" '(restart-emacs :which-key "restart")
    "SPC" '(pieces-search :which-key "pieces")
    "." '(consult-fd :which-key "find file")
    "," '(consult-buffer :which-key "switch buffer")
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
    "fd" '(consult-fd :which-key "fd")
    "fs" '(save-buffer :which-key "save")
    "fr" '(consult-recent-file :which-key "recent")
    "fe" '(dirvish :which-key "dirvish")
    "ft" '(dirvish-side :which-key "sidebar")

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

    ;; Theme
    "T" '(consult-theme :which-key "theme")

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
      (let ((fn (pieces--make-binding act piece))
            (key (format "%d%d" act piece)))
        (general-define-key
         :states '(normal visual motion)
         :keymaps 'override
         :prefix "SPC"
         key fn)))))

;;; Terminal
(use-package vterm
  :ensure t
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

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

