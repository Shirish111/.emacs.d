;;; my-common.el-- - Common Utitlies
;;; Commentary
;; The `my-common' package is used to load the common utilities

;;; Code
;;; Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; show paren mode
(show-paren-mode t)

;; pending delete
(pending-delete-mode 1)

;; Rainbow Delimiters
(use-package rainbow-delimiters)

;; Expand Region
(use-package expand-region
  :ensure t
  :bind(("C-;" . er/expand-region)))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t)

;;Which key
(use-package which-key
  :ensure t
  :demand t
  :config(which-key-mode 1))

;; Smartparens
(use-package smartparens
  :ensure t
  :demand t
  :config (progn
	    (require 'smartparens-config)
	    (smartparens-global-mode)
	    (smartparens-strict-mode t)
	    (setq-default sp-escape-quotes-after-insert nil))
  :delight t
  :bind (("C-c r" . sp-rewrap-sexp)))

;; Autocomplete
;(use-package auto-complete
;  :ensure t
;  :demand t
;  :config (progn
;	    (ac-config-default)
;	    (global-auto-complete-mode t)
;	    (define-key ac-mode-map (kbd "TAB") nil)
;	    (define-key ac-completing-map (kbd "TAB") nil)
;	    (define-key ac-completing-map [tab] nil)
;	    (define-key ac-completing-map (kbd ".") 'ac-expand)))

;; Avy
(use-package avy
  :ensure t
  :demand t
  :bind (("C-l" . 'avy-goto-char))
  )

;; Move Text
(use-package move-text
  :demand t
  :ensure t
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
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

;; Company Quickhelp
(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

;; Yasnippet snippets
;(use-package yasnippet-snippets
;    :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))

;; Clang-format
(use-package clang-format
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  ;(setq clang-format-executable "/usr/local/clang-9.0.0/bin/clang-format")
  :config
  (add-hook 'before-save-hook '(lambda () (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)) (clang-format-buffer))))
  (setq clang-format-style "Google")
  )

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Dashboard
(use-package dashboard
  :ensure t
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

(provide 'my-common)

;;; my-common.el ends here
