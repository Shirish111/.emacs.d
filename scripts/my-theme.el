;;; my-theme.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-theme' package is used to load the required configuration

;; Window Preferences
;; Left Margin
(setq-default left-margin-width 2)

;; Menu bar disable
(menu-bar-mode -1)

;; Scroll bar disable
(toggle-scroll-bar -1)

;; Tool bar disable
(tool-bar-mode -1)

;; Linum Mode
(global-linum-mode t)

;; ;; Cyberpunk theme
;; (use-package cyberpunk-theme
;;   :ensure t
;;   :config (load-theme 'cyberpunk t))
;; ;; Zenburn theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config (load-theme 'zenburn t))

;; Spacemacs theme
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))

(provide 'my-theme)

;;; my-theme package ends here
