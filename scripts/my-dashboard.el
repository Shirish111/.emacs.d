;;; my-dashboard.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-dashboard' package is used to load the required configuration

;; Dashboard
(use-package dashboard
  :ensure t
  :delight dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-center-content nil))



(provide 'my-dashboard)

;;; my-dashboard package ends here

