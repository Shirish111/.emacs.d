;;; my-html.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-html' package is used to load the required configuration

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (add-hook 'web-mode-hook 'electric-pair-mode))

;; Prettier formatting
;; `NOTE': npm -g install prettier
(use-package prettier-js
  :ensure t
  :init
  (add-hook 'html-mode 'prettier-js-mode)
  (add-hook 'web-mode 'prettier-js-mode)
  (add-hook 'js-mode 'prettier-js-mode)
  (add-hook 'js2-mode 'prettier-js-mode)
  (add-hook 'css-mode 'prettier-js-mode))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode 'prettier-js-mode)
  )

(provide 'my-html)

;;; my-html package ends here

