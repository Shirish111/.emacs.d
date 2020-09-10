;;; my-common.el-- - Common Utitlies
;;; Commentary
;; The `my-common' package is used to load the common utilities

;; Hash Library
(use-package ht
  :init
  :demand t
  :ensure t)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t
  :delight
  :config
  )

;; Expand Region
(use-package expand-region
  :ensure t
  :delight
  :bind(("C-;" . er/expand-region))
  :after (org))

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

;; Which key
(use-package which-key
  :ensure t
  :demand t
  :delight)
  ;;:config(which-key-mode 1))

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
  :bind (("s-l" . 'avy-goto-char)))

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

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

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
  :config
  (yas-global-mode 1)
  (setq yas-triggers-in-field t)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  :bind (
         ("s-i" . yas-insert-snippet)
         )
  )

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
    :bind (("C-x g" . magit-status))
    :config
    (setq magit-status-)
)

;; Forge
(use-package forge
  :ensure t
  :after magit)

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
  ;;(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (global-set-key (kbd "C-f") 'find-file)
  (projectile-mode +1))

;; Json Mode
(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

;; Phi search
(use-package phi-search
  :ensure t
  :config)

;; Abbrev
(use-package abbrev
  :delight
  :config
  (setq-default abbrev-mode t)
  )

;; Smerge
(use-package smerge-mode
  :init
  (setq smerge-command-prefix "C-v")
  :config
  (add-hook 'smerge-mode-hook (lambda () (define-key smerge-mode-map (kbd ".") 'smerge-keep-current)))
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

;; Power Thesaurus
(use-package powerthesaurus
  :init
  :ensure t
  :config
  (global-set-key (kbd "s-.") 'powerthesaurus-lookup-word-at-point)
  (global-set-key (kbd "s-,") 'powerthesaurus-lookup-word)
  )

;; Inhibit Startup Screen
(setq inhibit-startup-message t)

;; Dumb Jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :demand t
  :ensure t)

;; Evil
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  ;;(defalias 'evil-insert-state 'evil-emacs-state)
  (setq evil-default-state 'emacs)
  (setq evil-want-C-u-scroll t)
  (setq evil-normal-state-cursor '(box "orange"))
  (setq evil-emacs-state-cursor '(box "white"))
  :ensure t
  :config
  (evil-mode t)
  )
(eval-after-load 'evil-core
  '(evil-set-initial-state 'magit-popup-mode 'emacs))
(eval-after-load 'evil-core
  '(evil-set-initial-state 'shell-mode 'emacs))

(defadvice evil-insert-state (around emacs-state-instead-of-insert-state activate)
  (evil-emacs-state))
(define-key evil-emacs-state-map (kbd "<backtab>") 'evil-normal-state)

;; Evil Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; General Keybindings
(use-package general
  :init
  :ensure t
  :config
  (general-swap-key nil 'motion
  ";" ":")
  )
(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(my-leader-def
  :keymaps 'normal
  )

(my-leader-def
  :keymaps 'visual
  "c" 'comment-dwim)

(use-package annotate
  :init
  :ensure t
  :config
  (global-set-key (kbd "s-\\") 'annotate-mode)
  )

(use-package itail
  :init
  :ensure t
  :config
  )

(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :ensure t
  :config
  )

(use-package paradox
  :init
  :ensure t
  :config
  (paradox-enable)
  )

(use-package calfw
  :init
  :ensure t
  :config
  )

(use-package calfw-org
  :init
  :ensure t
  :config
  (global-set-key (kbd "s-/") 'cfw:open-org-calendar)
  )

(use-package calfw-cal
  :init
  :ensure t
  :config
  )

(use-package calfw-ical
  :init
  :ensure t
  :config
  )

(use-package visual-regexp-steroids
  :init
  :ensure t
  :config
  )

(use-package prodigy
  :init
  :ensure t
  :config
  (setq prodigy-services nil)
  (prodigy-define-service
    :name "Rails server Development"
    :command "rails"
    :args '("server")
    :cwd "~/Rails/api/viv-colending-api"
    :tags '(work)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Rails server QA"
    :command "rails"
    :args '("server" "-e" "qa")
    :cwd "~/Rails/api/viv-colending-api"
    :tags '(work)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Rails server Staging"
    :command "rails"
    :args '("server" "-e" "staging")
    :cwd "~/Rails/api/viv-colending-api"
    :tags '(work)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Rails server Production"
    :command "rails"
    :args '("server" "-e" "production")
    :cwd "~/Rails/api/viv-colending-api"
    :tags '(work)
    :stop-signal 'sigint
    :kill-process-buffer-on-stop t)
  )

(use-package epa-file
  :init
  :config
  (epa-file-enable)
  )

(provide 'my-common)

;;; my-common package ends here
