;;; my-common.el-- - Common Utitlies
;;; Commentary
;; The `my-common' package is used to load the common utilities

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
  :delight)

;; Expand Region
(use-package expand-region
  :ensure t
  :delight
  :bind(("C-;" . er/expand-region)))

;; Multiple Cursors
(use-package multiple-cursors
  :delight
  :ensure t)

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
	    (setq-default sp-escape-quotes-after-insert nil))
  :bind (("C-c r" . sp-rewrap-sexp)))

;; Avy
(use-package avy
  :delight
  :ensure t
  :demand t
  :bind (("C-l" . 'avy-goto-char))
  )

;; Move Text
(use-package move-text
  :demand t
  :ensure t
  :delight
  :config (move-text-default-bindings))

;; Company
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (define-key company-active-map (kbd "<tab>") nil)

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

;; Dashboard
(use-package dashboard
  :ensure t
  :delight dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-center-content t))

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
(global-set-key (kbd "C-b") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c b") 'bookmark-set)

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

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

(provide 'my-common)

;;; my-common.el ends here
