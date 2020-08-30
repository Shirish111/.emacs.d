;;; my-java.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-java' package is used to load the required configuration

;; autodisass-java-bytecode
(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

;; Meghanada
;; `NOTE': Run meghanada-update-server on installation
(use-package meghanada
  :ensure t
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)
              (meghanada-mode t)
              (smartparens-mode t)
              (rainbow-delimiters-mode t)
              ;(highlight-symbol-mode t)
              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))
            )

  :config
  (use-package realgud
    :ensure t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(provide 'my-java)

;;; my-java package ends here
