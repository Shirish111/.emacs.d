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

;; Spacemacs theme
;; (use-package spacemacs-theme
;;   :delight 
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t))

;; Monokai theme
;; (use-package monokai-theme
;;   :init
;;   :ensure t
;;   :config
;;   )

;; Dracula theme
(use-package dracula-theme
  :init
  (load-theme 'dracula t)
  :ensure t
  :config
  )

(provide 'my-theme)

;;; my-theme package ends here
