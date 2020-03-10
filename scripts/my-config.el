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
(use-package my-java)
(use-package my-html)
(use-package my-js)
(use-package my-hydra)
;(use-package my-neotree)
(use-package my-treemacs)
(use-package my-eshell)
(add-to-list 'load-path "~/my-emacs/scripts/emacs_custom")
;(use-package my-dashboard)
(use-package my-personal)

(provide 'my-config)

;;; my-config package ends here
