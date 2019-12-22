;;; my-org.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-org' package is used to load the required configuration


(use-package org
  :defer t
  :ensure org-plus-contrib)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
  :ensure t
  :config (message "Org bullets loaded")
  )

(use-package org-babel-eval-in-repl
  :config
  (message "org babel-eval executed")
  :init
  (add-hook 'org-mode-hook   (lambda ()
			       (org-babel-do-load-languages
				'org-babel-load-languages
				'(
				  (python . t)
				  (shell . t)
				  (C . t)))
			       ))
  :ensure t)

(provide 'my-org)

;;; my-org package ends here
