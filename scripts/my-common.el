;;; my-common.el-- - Common Utitlies
;;; Commentary
;; The `my-common' package is used to load the common utilities

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-prompt-before-update t)

  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;; Code
;; Diminish and Delight
(use-package diminish
  :ensure t)
(use-package delight
  :ensure t)

;; Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; show paren mode
(show-paren-mode t)

;; pending delete
(pending-delete-mode 1)

;; Display time
(display-time-mode t)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :delight)

;; Expand Region
(use-package expand-region
  :ensure t
  :delight
  :bind(("C-;" . er/expand-region))
  :after (org))
  

;; Smex
(use-package smex
  :ensure t)

;; Multiple Cursors
(use-package multiple-cursors
  :delight
  :ensure t
  :bind (("C-." . mc/mark-next-like-this)
         ("C->" . mc/skip-to-next-like-this)
         ("C-c n" . mc/unmark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-<" . mc/skip-to-previous-like-this)
         ("C-c p" . mc/unmark-previous-like-this)
         ("C-)" . mc/edit-lines)
         ("<s-mouse-1>" . mc/add-cursor-on-click)
         ))

;;Which key
(use-package which-key
  :ensure t
  :demand t
  :delight
  :config(which-key-mode 1))

;; Smartparens
(use-package smartparens
  :delight smartparens-mode
  :ensure t
  :demand t
  :config (progn
	    (require 'smartparens-config)
	    (smartparens-global-mode)
	    (smartparens-strict-mode t)
	    (setq-default sp-escape-quotes-after-insert nil)
            (sp-local-pair '(c++-mode ruby-mode java-mode python-mode) "'" "'"))
  :bind (("C-c r" . sp-rewrap-sexp)))

;; Avy
(use-package avy
  :delight
  :ensure t
  :demand t
  :bind (("M-g c" . 'avy-goto-char))
  )

;; Move Text
(use-package move-text
  :demand t
  :ensure t
  :delight
  :init (move-text-default-bindings))

;; Company
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (define-key company-active-map (kbd "<tab>") nil)
    ;(setq completion-styles '(substring basic partial-completion emacs22))
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil)
    (push 'company-robe company-backends))
  :diminish company-mode)

;; Company Quickhelp
(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :delight
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :delight
  :config (yas-global-mode 1))

;; Yasnippet snippets
;(use-package yasnippet-snippets
;    :ensure t)

;; Flycheck
(use-package flycheck
  :delight
  :ensure t
  :commands flycheck-mode
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode))

;; Magit
(use-package magit
  :ensure t
  :delight
  :bind (("C-x g" . magit-status)))

;; Recentf-mode
(use-package recentf
  :demand t
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode 1)
  :bind (("C-x C-r" . recentf-open-files)))

;; Bookmarks
(global-set-key (kbd "C-'") 'bookmark-bmenu-list)
(global-set-key (kbd "C-\"") 'bookmark-set)

;; Backup Files
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Projectile
(use-package projectile
  :ensure t
  :delight
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode +1))


;; Json Mode
(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

;; Dired
(use-package dired
  :init)
  ;(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)(dired-sort-toggle-or-edit))))

;; Phi search
(use-package phi-search
  :ensure t
  :config)

;; Abbrev
(use-package abbrev
  :delight
  )

;; Smerge
(use-package smerge-mode
  :init
  (setq smerge-command-prefix "C-v")
  :config
  (add-hook 'smerge-mode-hook (lambda ()(define-key smerge-mode-map (kbd ".") 'smerge-keep-current)))
  )

;; ag
(use-package ag
  :ensure t)

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;; RestClient
(use-package restclient
  :ensure t)

;; Vimish Fold
(use-package vimish-fold
  :ensure t
  :init
  (vimish-fold-global-mode 1)
  (global-set-key (kbd "C-c f") #'vimish-fold)
  (global-set-key (kbd "C-c F") #'vimish-fold-delete)
  )

;; Tiny
(use-package tiny
  :ensure t
  :config
  (global-set-key (kbd "C-c _") 'tiny-expand)
  )

;; YaTemplate
(use-package yatemplate
  :ensure t
  :config
  (auto-insert-mode t)
  (setq auto-insert-query nil)
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist))

;; Switch Windows
(use-package ace-window
  :init
  :ensure t
  :config
  :bind (("C-c o" . ace-window))
  )

;; persistent-scratch
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

;; Hash Library
(use-package ht
  :init
  :demand t
  :ensure t)

;; Diff Highlight
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(provide 'my-common)

;;; my-common.el ends here
