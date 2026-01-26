;;; config.el --- Emacs config (tangled from README.org) -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq inhibit-startup-echo-area-message "c"
      server-client-instructions nil)

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
      my/corfu-delay 0.2
      my/corfu-prefix 3
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
      confirm-kill-emacs nil)

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

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my/set-fonts))))
  (add-hook 'after-init-hook #'my/set-fonts))

(setq interprogram-cut-function nil
      interprogram-paste-function nil)

(defun my/clipboard-get ()
  (shell-command-to-string (if (eq system-type 'darwin) "pbpaste" "wl-paste -n")))

(defun my/clipboard-paste ()
  (interactive)
  (let ((content (my/clipboard-get)))
    (if (derived-mode-p 'vterm-mode)
        (vterm-send-string content)
      (insert content))))

(defun my/prose-mode-setup ()
  (setq-local line-spacing my/prose-line-spacing)
  (face-remap-add-relative 'default :foreground my/prose-fg))

(add-hook 'org-mode-hook #'my/prose-mode-setup)
(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :hook ((markdown-mode . variable-pitch-mode)
         (markdown-mode . my/prose-mode-setup)))

;; Save on idle (VSCode-style)
;; Note: window-selection-change-functions breaks frame resizing
(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

(defun notes--set-header-title ()
  "Display filename as title overlay at buffer start."
  (when buffer-file-name
    (let* ((title (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
           (styled-title (propertize title
                                     'face `(:height ,my/title-height :weight bold :foreground ,my/color-red :family ,my/font-variable))))
      ;; Remove any existing title overlay
      (dolist (ov (overlays-at 1))
        (when (overlay-get ov 'notes-title)
          (delete-overlay ov)))
      ;; Create new title overlay
      (let ((ov (make-overlay 1 1)))
        (overlay-put ov 'notes-title t)
        (overlay-put ov 'before-string (concat "\n" styled-title "\n\n"))))))

(add-hook 'org-mode-hook #'notes--set-header-title)
(add-hook 'markdown-mode-hook #'notes--set-header-title)
(with-eval-after-load 'org
  (dolist (face '(org-block org-block-begin-line org-block-end-line
                  org-code org-meta-line org-table org-verbatim))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  (set-face-attribute 'org-block nil :background my/color-black)
  (set-face-attribute 'org-level-1 nil :height my/heading-height :weight 'bold)
  (font-lock-add-keywords
   'org-mode
   '(("@\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\([^)]*\\))"
      (0 'org-macro t)))))

;; TODO: migrate to org
(defvar notes-directory "~/notes")
(defvar notes-daily-directory (expand-file-name "periodic/daily" notes-directory))

(defun notes--daily-file (time)
  (expand-file-name (format-time-string "%m-%d.md" time) notes-daily-directory))

(defun notes-open-daily ()
  (interactive)
  (let ((file (notes--daily-file (current-time))))
    (unless (file-exists-p notes-daily-directory)
      (make-directory notes-daily-directory t))
    (find-file file)))

(defun notes--current-time ()
  (when-let* ((name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
              (parts (split-string name "-")))
    (encode-time 0 0 0
                 (string-to-number (cadr parts))
                 (string-to-number (car parts))
                 (string-to-number (format-time-string "%Y")))))

(defun notes-prev ()
  (interactive)
  (when-let ((time (notes--current-time)))
    (cl-loop for i from 1 to 365
             for file = (notes--daily-file (time-subtract time (days-to-time i)))
             when (file-exists-p file) return (find-file file)
             finally (message "No previous note"))))

(defun notes-next ()
  (interactive)
  (when-let ((time (notes--current-time)))
    (cl-loop for i from 1 to 365
             for file = (notes--daily-file (time-add time (days-to-time i)))
             when (file-exists-p file) return (find-file file)
             finally (message "No next note"))))

(defun pieces-search ()
  (interactive)
  (let* ((dir "~/neuralinux/acts/")
         (files (directory-files-recursively dir "\\.md$"))
         (file-alist (mapcar (lambda (f) (cons (file-name-base f) f)) files))
         (selected (completing-read "Piece: " file-alist nil t)))
    (find-file (cdr (assoc selected file-alist)))))

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (set-face-attribute 'org-block-end-line nil :foreground my/color-black))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))


(use-package olivetti
  :ensure t
  :hook ((org-mode markdown-mode) . olivetti-mode)
  :config
  (setq olivetti-body-width my/olivetti-width)
  (set-face-attribute 'olivetti-fringe nil :background my/color-black))

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  (setq vertico-count my/vertico-count
        vertico-resize nil)
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides nil
        completion-category-defaults nil
        orderless-matching-styles '(orderless-flex)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(setq recentf-save-file "~/.cache/emacs/recentf")
(make-directory "~/.cache/emacs" t)
(recentf-mode 1)

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer))
  :config
  (setq consult-async-min-input my/consult-async-min-input
        consult-async-refresh-delay my/consult-async-delay
        consult-async-input-debounce my/consult-async-debounce
        consult-async-input-throttle my/consult-async-throttle
        consult-buffer-filter '("\\`\\*" "\\` " "\\`[0-9]\\{2\\}-[0-9]\\{2\\}\\.md\\'")))

(use-package avy
  :ensure t
  :after evil
  :config
  (setq avy-timeout-seconds 0.3)
  (evil-define-key 'normal 'global "s" 'avy-goto-char-timer)
  (evil-define-key 'normal 'global "m" 'avy-move-line))

(use-package nerd-icons
  :ensure t)

(use-package neotree
  :ensure t
  :after nerd-icons
  :config
  (setq neo-theme 'nerd-icons
        neo-window-width 30
        neo-smart-open t))

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t
        corfu-auto-delay my/corfu-delay
        corfu-auto-prefix my/corfu-prefix)
  (global-corfu-mode 1)
  (add-hook 'org-mode-hook (lambda () (corfu-mode -1))))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo
        evil-disable-insert-state-bindings t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-insert-state-cursor 'bar)
  (blink-cursor-mode -1)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global
    "gd" 'xref-find-definitions
    "gy" 'eglot-find-typeDefinition
    "gr" 'xref-find-references
    "gi" 'eglot-find-implementation)
  (global-set-key (kbd "M-\\") 'notes-open-daily)
  (global-set-key (kbd "M-[") 'notes-prev)
  (global-set-key (kbd "M-]") 'notes-next)
  (global-set-key (kbd "M-SPC") 'vterm-full-toggle)
  (global-set-key (kbd "M-S-SPC") 'vterm-toggle))

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

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0
        which-key-show-prefix 'echo
        which-key-prefix-prefix ""
        which-key-echo-keystrokes 0
        which-key-allow-regexps '("^SPC"))

  (defun my/git-status ()
    "Get p10k-style git status string."
    (when-let* ((file (buffer-file-name))
                ((vc-backend file))
                (branch (car (vc-git-branches)))
                (default-directory (file-name-directory file))
                (out (shell-command-to-string "git status --porcelain -b 2>/dev/null")))
      (let ((ahead 0) (behind 0) (staged 0) (unstaged 0))
        (dolist (line (split-string out "\n" t))
          (cond
           ((string-match "ahead \\([0-9]+\\)" line) (setq ahead (string-to-number (match-string 1 line))))
           ((string-match "behind \\([0-9]+\\)" line) (setq behind (string-to-number (match-string 1 line))))
           ((string-match "^[MADRC]" line) (setq staged (1+ staged)))
           ((string-match "^.[MADRC]" line) (setq unstaged (1+ unstaged)))))
        (concat (propertize branch 'face `(:foreground ,my/color-cyan))
                (if (> ahead 0) (propertize (format " ↑%d" ahead) 'face `(:foreground ,my/color-green)) "")
                (if (> behind 0) (propertize (format " ↓%d" behind) 'face `(:foreground ,my/color-yellow)) "")
                (if (> staged 0) (propertize (format " +%d" staged) 'face `(:foreground ,my/color-green)) "")
                (if (> unstaged 0) (propertize (format " !%d" unstaged) 'face `(:foreground ,my/color-red)) "")))))

  (advice-add 'which-key--echo :override
              (lambda (&rest _)
                (let* ((path (propertize (abbreviate-file-name (or (buffer-file-name) (buffer-name)))
                                         'face `(:foreground ,my/color-blue)))
                       (mod (cond (buffer-read-only " %%") ((buffer-modified-p) " *") (t nil)))
                       (mod-str (if mod (propertize mod 'face `(:foreground ,my/color-yellow)) ""))
                       (git (my/git-status)))
                  (message "%s%s%s" path mod-str (if git (format " %s" git) "")))))
  (which-key-mode 1))

(use-package general
  :ensure t
  :after evil
  :config
  (general-create-definer leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")

  (general-define-key
   :states 'normal
   :keymaps 'override
   "K" 'eldoc-box-help-at-point)

  (leader-def
    "" '(nil :which-key "space")
    "SPC" '((lambda () (interactive)
              (if (project-current)
                  (magit-status)
                (magit-status (project-prompt-project-dir)))) :which-key "git")
    "\\" '(pieces-search :which-key "pieces")
    "|" '(restart-emacs :which-key "restart")

    "." '(consult-fd :which-key "find")
    "," '(consult-buffer :which-key "buffers")
    ";" '(neotree-toggle :which-key "tree")
    "x" '(scratch-buffer :which-key "scratch")

    "f" '(project-find-file :which-key "files")
    "F" '(find-file :which-key "file")
    "P" '(project-switch-project :which-key "projects")
    "j" '(evil-show-jumps :which-key "jumps")
    "s" '(consult-imenu :which-key "symbols")
    "S" '(xref-find-apropos :which-key "workspace-symbols")
    "d" '(consult-flymake :which-key "diagnostics")
    "D" '(flymake-show-project-diagnostics :which-key "project-diag")
    "r" '(consult-recent-file :which-key "recent")
    "a" '(eglot-code-actions :which-key "actions")
    "h" '(xref-find-references :which-key "references")

    "'" '(vertico-repeat :which-key "repeat")
    "c" '(evil-commentary-line :which-key "comment")
    "C" '((lambda () (interactive) (find-file "~/nixos/linux.nix")) :which-key "nixos")
    "E" '((lambda () (interactive) (find-file "~/.emacs.d/README.org")) :which-key "emacs")

    "p" '(my/clipboard-paste :which-key "paste")
    "y" '((lambda () (interactive)
            (call-process-region (region-beginning) (region-end)
                                 (if (eq system-type 'darwin) "pbcopy" "wl-copy") nil 0)
            (evil-exit-visual-state)) :which-key "yank")
    "R" '((lambda () (interactive)
            (delete-region (region-beginning) (region-end))
            (insert (my/clipboard-get))) :which-key "replace")

    "/" '(consult-ripgrep :which-key "search")
    "?" '(execute-extended-command :which-key "commands")

    "q" '(evil-quit :which-key "quit")))

(setq project-switch-commands 'project-find-file)

(use-package vterm
  :ensure t
  :hook (vterm-mode . evil-insert-state)
  :config
  (setq vterm-timer-delay 0.01
        vterm-max-scrollback 5000)
  (when (eq system-type 'gnu/linux)
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))
  (define-key vterm-mode-map (kbd "C-c") #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-u") #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-y") #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-z") #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-S-v") #'my/clipboard-paste)
  (define-key vterm-mode-map (kbd "M-SPC") #'vterm-full-toggle)
  (define-key vterm-mode-map (kbd "M-S-SPC") #'vterm-toggle)
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq-local global-hl-line-mode nil)
                               (setq-local truncate-lines t)))
  (add-hook 'vterm-set-directory-functions
            (lambda (dir)
              (setq-local default-directory dir)))
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.5))))

(defun vterm-toggle ()
  "Toggle vterm side window. Closes full-frame vterm if open."
  (interactive)
  (when (and (eq (current-buffer) (get-buffer "*vterm*"))
             (one-window-p))
    (previous-buffer))
  (if-let ((win (get-buffer-window "*vterm*")))
      (delete-window win)
    (vterm)))

(defun vterm-full-toggle ()
  "Toggle vterm in full frame. Closes side window if open."
  (interactive)
  (when-let ((win (get-buffer-window "*vterm*")))
    (when (window-parameter win 'window-side)
      (delete-window win)))
  (if (and (eq (current-buffer) (get-buffer "*vterm*"))
           (not (window-parameter (selected-window) 'window-side)))
      (previous-buffer)
    (let ((display-buffer-overriding-action '(display-buffer-same-window)))
      (vterm))
    (delete-other-windows)))

(setq magit-no-confirm '(stage-all-changes unstage-all-changes))
(use-package magit
  :ensure t
  :hook (git-commit-mode . evil-insert-state)
  :config
  (add-hook 'with-editor-post-finish-hook #'delete-window))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
(add-hook 'eglot-managed-mode-hook #'flymake-mode)
(setq eldoc-echo-area-use-multiline-p nil
      eldoc-display-functions '(eldoc-display-in-buffer))
(setq flymake-fringe-indicator-position nil)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp" "stdio")))
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(yaml-ts-mode . ("yaml-language-server" "--stdio")))
  (setq eglot-report-progress nil)
  (advice-add 'eglot--message :override #'ignore))

(use-package eldoc-box
  :ensure t)

(use-package eglot-booster
  :ensure t
  :after eglot
  :config
  (eglot-booster-mode))

(use-package go-mode
  :ensure t
  :hook ((go-mode go-ts-mode) . eglot-ensure)
  :config
  (add-hook 'before-save-hook
            (lambda ()
              (when (derived-mode-p 'go-mode 'go-ts-mode)
                (eglot-format-buffer)
                (eglot-code-action-organize-imports (point-min) (point-max))))))

(setq python-indent-guess-indent-offset-verbose nil)

(dolist (hook '(python-mode-hook python-ts-mode-hook
                sh-mode-hook bash-ts-mode-hook
                tsx-ts-mode-hook
                css-mode-hook css-ts-mode-hook
                json-mode-hook json-ts-mode-hook
                c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook
                toml-ts-mode-hook yaml-mode-hook yaml-ts-mode-hook))
  (add-hook hook 'eglot-ensure))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.vue\\'")
  :hook (web-mode . eglot-ensure)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook ((typescript-mode typescript-ts-mode) . eglot-ensure)
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook ((js2-mode js-ts-mode) . eglot-ensure)
  :config
  (setq js2-basic-offset 2))

(use-package lua-mode
  :ensure t
  :hook (lua-mode . eglot-ensure)
  :config
  (setq lua-indent-level 2))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode 1))
