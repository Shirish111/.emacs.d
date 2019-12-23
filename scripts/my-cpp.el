;;; my-cpp.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-cpp' package is used to load the required configuration
;; Clang-format
(use-package clang-format
  :delight
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  ;(setq clang-format-executable "/usr/local/clang-9.0.0/bin/clang-format")
  :config
  (add-hook 'before-save-hook '(lambda () (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)) (clang-format-buffer))))
  (setq clang-format-style "Google")
  )

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(provide 'my-cpp)

;;; my-cpp package ends here

