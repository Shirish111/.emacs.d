;;; my-lsp.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-lsp' package is used to load the required configuration

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :ensure t
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (XXX-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp
;;   :ensure t
;;   :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Additional Settings
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(provide 'my-lsp)

;;; my-lsp package ends here
