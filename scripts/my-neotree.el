;;; my-neotree.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-neotree' package is used to load the required configuration

;; `Note' Install fonts after installing all-the-icons by doing
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; Neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-fixed-size nil)
  :bind(:map neotree-mode-map
             (("r" . neotree-change-root)
              ("c" . neotree-copy-node)
              ("p" . neotree-copy-filepath-to-yank-ring)
              ("C" . neotree-create-node)
              ("m" . neotree-rename-node)
              ))
  )
(global-set-key (kbd "C-'") 'neotree-toggle)


(provide 'my-neotree)

;;; my-neotree package ends here

