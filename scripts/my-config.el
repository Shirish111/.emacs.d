;;; my-config.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-config' package is used to load the required configuration

(use-package my-theme)
(use-package my-common)
(use-package my-custom)
(use-package my-ivy-config)
(use-package my-org)
(use-package my-cpp)
(use-package my-ruby)
(use-package my-python)
(use-package my-hydra)

(provide 'my-config)

;;; my-config package ends here

