;;; config.el --- Emacs config (tangled from README.org) -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq inhibit-startup-echo-area-message "c"
      inhibit-startup-message t
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
      my/which-key-delay 0.3
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
(advice-add 'save-buffers-kill-terminal :before
            (lambda (&rest _)
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (set-buffer-modified-p nil)))))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default mode-line-format nil)

(setq modus-themes-common-palette-overrides
      `((bg-main ,my/color-black)
        (bg-dim ,my/color-dim)
        (bg-alt ,my/color-dim)))
(load-theme 'modus-vivendi t)
(dolist (face '(fringe line-number line-number-current-line))
  (set-face-attribute face nil :background my/color-black))
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))

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

(add-to-list 'command-switch-alist
             '("--vterm" . (lambda (_) (add-hook 'emacs-startup-hook #'vterm-full))))
(add-to-list 'command-switch-alist
             '("--daily" . (lambda (_) (add-hook 'emacs-startup-hook #'notes-open-daily))))

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

(defvar pieces-directory (expand-file-name "pieces" notes-directory))

(defun pieces--file (act piece)
  (let* ((act-dir (expand-file-name (format "Act %d" act) pieces-directory))
         (pattern (format "^%d_.*\\.md$" piece))
         (files (and (file-directory-p act-dir)
                     (directory-files act-dir t pattern))))
    (if files
        (car files)
      (expand-file-name (format "%d_.md" piece) act-dir))))

(defun pieces-open (act piece)
  (let ((file (pieces--file act piece))
        (dir (expand-file-name (format "Act %d" act) pieces-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (find-file file)))

(defun pieces--make-binding (act piece)
  (let ((fn (lambda () (interactive) (pieces-open act piece))))
    (defalias (intern (format "pieces-open-%d-%d" act piece)) fn)
    (intern (format "pieces-open-%d-%d" act piece))))

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

(use-package org-transclusion
  :ensure t
  :after (org evil)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'insert org-transclusion-live-sync-map
      (kbd "C-c C-c") #'org-transclusion-live-sync-exit)))

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

(use-package vi-tilde-fringe
  :ensure t
  :hook (prog-mode . vi-tilde-fringe-mode))

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
  (evil-define-key 'normal 'global
    "|" 'notes-open-daily)
  (global-set-key (kbd "M-[") 'notes-prev)
  (global-set-key (kbd "M-]") 'notes-next))

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

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay my/which-key-delay
        which-key-show-prefix 'echo
        which-key-prefix-prefix ""
        which-key-echo-keystrokes 0)
  (defvar my/colors `((path . ,my/color-blue)
                      (modified . ,my/color-yellow)
                      (branch . ,my/color-cyan)
                      (ahead . ,my/color-green)
                      (behind . ,my/color-yellow)
                      (staged . ,my/color-green)
                      (unstaged . ,my/color-red)))

  (defun my/color (key)
    (alist-get key my/colors))

  (defun my/git-status ()
    "Get p10k-style git status string with colors."
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
        (concat (propertize branch 'face `(:foreground ,(my/color 'branch)))
                (if (> ahead 0) (propertize (format " ↑%d" ahead) 'face `(:foreground ,(my/color 'ahead))) "")
                (if (> behind 0) (propertize (format " ↓%d" behind) 'face `(:foreground ,(my/color 'behind))) "")
                (if (> staged 0) (propertize (format " +%d" staged) 'face `(:foreground ,(my/color 'staged))) "")
                (if (> unstaged 0) (propertize (format " !%d" unstaged) 'face `(:foreground ,(my/color 'unstaged))) "")))))

  (advice-add 'which-key--echo :override
              (lambda (&rest _)
                (let* ((path (propertize (abbreviate-file-name (or (buffer-file-name) (buffer-name)))
                                         'face `(:foreground ,(my/color 'path))))
                       (mod (cond (buffer-read-only " %%") ((buffer-modified-p) " *") (t nil)))
                       (mod-str (if mod (propertize mod 'face `(:foreground ,(my/color 'modified))) ""))
                       (git (my/git-status)))
                  (message "%s%s%s" path mod-str (if git (format " %s" git) "")))))
  (dolist (n (number-sequence 1 9))
    (push `((,(format "SPC %d" n) . nil) . t) which-key-replacement-alist))
  (push '(("g" . "evil-.*") . t) which-key-replacement-alist)
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
    "SPC" '(project-find-file :which-key "find file (project)")
    "\\" '(pieces-search :which-key "pieces")
    "|" '(restart-emacs :which-key "restart")
    "TAB" '(vterm :which-key "terminal")

    "." '(consult-fd :which-key "find file")
    "," '(consult-buffer :which-key "switch buffer")
    ";" '(execute-extended-command :which-key "M-x")
    "x" '(scratch-buffer :which-key "scratch")

    "f" '(neotree-toggle :which-key "file tree")
    "F" '(find-file :which-key "file picker (cwd)")
    "b" '(consult-buffer :which-key "buffer picker")
    "j" '(evil-show-jumps :which-key "jumplist picker")

    "k" '(eldoc-box-help-at-point :which-key "hover docs")
    "s" '(consult-imenu :which-key "document symbols")
    "S" '(xref-find-apropos :which-key "workspace symbols")
    "d" '(consult-flymake :which-key "diagnostics (buffer)")
    "D" '(flymake-show-project-diagnostics :which-key "diagnostics (project)")
    "r" '(eglot-rename :which-key "rename symbol")
    "a" '(eglot-code-actions :which-key "code action")
    "h" '(xref-find-references :which-key "references")

    "'" '(vertico-repeat :which-key "last picker")
    "c" '(evil-commentary-line :which-key "comment")
    "l" '(toggle-file-lock :which-key "lock/unlock file")
    "C" '((lambda () (interactive) (find-file "~/nixos/home.nix")) :which-key "nixos config")
    "E" '((lambda () (interactive) (find-file "~/.emacs.d/README.org")) :which-key "emacs config")

    "p" '((lambda () (interactive)
            (insert (shell-command-to-string "wl-paste -n"))) :which-key "paste clipboard")
    "y" '((lambda () (interactive)
            (call-process-region (region-beginning) (region-end) "wl-copy" nil 0)) :which-key "yank to clipboard")
    "R" '((lambda () (interactive)
            (delete-region (region-beginning) (region-end))
            (insert (shell-command-to-string "wl-paste -n"))) :which-key "replace with clipboard")

    "g" '((lambda () (interactive)
            (if (project-current)
                (magit-status)
              (magit-status (project-prompt-project-dir)))) :which-key "git")

    "/" '(consult-ripgrep :which-key "global search")
    "?" '(execute-extended-command :which-key "command palette")

    "q" '(evil-quit :which-key "quit"))

  (dolist (act (number-sequence 1 9))
    (dolist (piece (number-sequence 1 9))
      (let ((fn (pieces--make-binding act piece))
            (key (format "%d%d" act piece)))
        (general-define-key
         :states '(normal visual motion)
         :keymaps 'override
         :prefix "SPC"
         :wk-full-keys nil
         key `(,fn :which-key nil))))))

(use-package vterm
  :ensure t
  :hook (vterm-mode . evil-insert-state)
  :config
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"
        vterm-timer-delay 0.01)
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.3))))

(defun vterm-full ()
  "Open vterm in full frame."
  (interactive)
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (vterm))
  (delete-other-windows))

(setq magit-no-confirm '(stage-all-changes unstage-all-changes))
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :hook (git-commit-mode . evil-insert-state)
  :config
  (add-hook 'with-editor-post-finish-hook #'delete-window)

  ;; Visit worktree file (editable) with quick magit keys
  (defun my/magit-visit-file ()
    (interactive)
    (call-interactively #'magit-diff-visit-worktree-file-other-window)
    (evil-local-set-key 'normal "q" (lambda () (interactive) (kill-buffer) (delete-window)))
    (evil-local-set-key 'normal "B" #'magit-blame))

  (define-key magit-diff-mode-map (kbd "RET") #'my/magit-visit-file)
  (define-key magit-file-section-map (kbd "RET") #'my/magit-visit-file)
  (define-key magit-hunk-section-map (kbd "RET") #'my/magit-visit-file))

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
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
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
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

(add-hook 'sh-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)
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

(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook ((js2-mode js-ts-mode) . eglot-ensure)
  :config
  (setq js2-basic-offset 2))

(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'css-ts-mode-hook 'eglot-ensure)

(add-hook 'json-mode-hook 'eglot-ensure)
(add-hook 'json-ts-mode-hook 'eglot-ensure)

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)

(add-hook 'toml-ts-mode-hook 'eglot-ensure)
(add-hook 'yaml-mode-hook 'eglot-ensure)
(add-hook 'yaml-ts-mode-hook 'eglot-ensure)
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

(defun toggle-file-lock ()
  "Toggle write permission on current file."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (let* ((modes (file-modes file))
             (writable (file-writable-p file)))
        (if writable
            (set-file-modes file (logand modes #o555))
          (set-file-modes file (logior modes #o200)))
        (revert-buffer t t t)
        (message "File %s" (if writable "locked" "unlocked")))
    (message "Buffer has no file")))
