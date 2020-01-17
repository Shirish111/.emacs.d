;;; my-html.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-html' package is used to load the required configuration

(use-package web-mode
   :ensure t)

(use-package mustache-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)
  )

(provide 'my-html)

;;; my-html package ends here

