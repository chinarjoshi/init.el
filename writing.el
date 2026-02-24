;;; writing.el --- Org, notes, markdown -*- lexical-binding: t -*-

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

(defvar notes-directory "~/notes")
(defvar notes-daily-directory (expand-file-name "periodic/daily" notes-directory))
(defvar notes-weekly-directory (expand-file-name "periodic/weekly" notes-directory))

(defun notes--daily-file (time)
  (expand-file-name (format-time-string "%m-%d.org" time) notes-daily-directory))

(defun notes--weekly-file (time)
  (expand-file-name (format-time-string "%Y-W%W.org" time) notes-weekly-directory))

(defun notes-open-daily ()
  (interactive)
  (let ((file (notes--daily-file (current-time))))
    (unless (file-exists-p notes-daily-directory)
      (make-directory notes-daily-directory t))
    (find-file file)))

(defun notes-open-weekly ()
  (interactive)
  (let ((file (notes--weekly-file (current-time))))
    (unless (file-exists-p notes-weekly-directory)
      (make-directory notes-weekly-directory t))
    (find-file file)))

(defun notes--daily-current-time ()
  (when-let* ((name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
              (parts (split-string name "-")))
    (encode-time 0 0 0
                 (string-to-number (cadr parts))
                 (string-to-number (car parts))
                 (string-to-number (format-time-string "%Y")))))

(defun notes--weekly-current-time ()
  (when-let* ((name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
              ((string-match "\\([0-9]+\\)-W\\([0-9]+\\)" name)))
    (let ((year (string-to-number (match-string 1 name)))
          (week (string-to-number (match-string 2 name))))
      (encode-time 0 0 0 (1+ (* 7 week)) 1 year))))

(defun notes--navigate-daily (dir)
  (when-let ((time (notes--daily-current-time)))
    (cl-loop for i from 1 to 365
             for file = (notes--daily-file (funcall dir time (days-to-time i)))
             when (file-exists-p file) return (find-file file)
             finally (message "No %s daily note" (if (eq dir #'time-subtract) "previous" "next")))))

(defun notes--navigate-weekly (dir)
  (when-let ((time (notes--weekly-current-time)))
    (cl-loop for i from 1 to 52
             for file = (notes--weekly-file (funcall dir time (days-to-time (* 7 i))))
             when (file-exists-p file) return (find-file file)
             finally (message "No %s weekly note" (if (eq dir #'time-subtract) "previous" "next")))))

(defun notes-prev () (interactive) (notes--navigate-daily #'time-subtract))
(defun notes-next () (interactive) (notes--navigate-daily #'time-add))
(defun notes-weekly-prev () (interactive) (notes--navigate-weekly #'time-subtract))
(defun notes-weekly-next () (interactive) (notes--navigate-weekly #'time-add))

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

(use-package snap
  :ensure t
  :vc (:url "https://github.com/chinarjoshi/snap" :rev :newest :lisp-dir "emacs")
  :hook (org-mode . snap-mode))
