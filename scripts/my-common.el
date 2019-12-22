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

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(provide 'my-common)

;;; my-common.el ends here
