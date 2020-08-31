;;; my-theme.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-theme' package is used to load the required configuration

;; Window Preferences
;; Left Margin
;(setq-default left-margin-width 2)

;; Menu bar disable
(menu-bar-mode -1)

;; Scroll bar disable
(scroll-bar-mode -1)

;; Tool bar disable
(tool-bar-mode -1)

;; Font size
(set-face-attribute 'default nil :height 120)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
 (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(provide 'my-theme)

;;; my-theme package ends here
