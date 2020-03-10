;;; my-theme.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-theme' package is used to load the required configuration

;; Window Preferences
;; Left Margin
(setq-default left-margin-width 2)

;; Menu bar disable
(menu-bar-mode -1)

;; Scroll bar disable
(scroll-bar-mode -1)

;; Tool bar disable
(tool-bar-mode -1)

;; Linum Mode
;(global-linum-mode t)

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
;; (use-package dracula-theme
;;  :init
;;  (load-theme 'dracula t)
;;  :ensure t
;;  :config
;;  )

(use-package hlinum
  :init
  :ensure t
  :config
  (hlinum-activate)
  (hlinum-highlight-line)
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
 (doom-themes-org-config))

(provide 'my-theme)

;;; my-theme package ends here
