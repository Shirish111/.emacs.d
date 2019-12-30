;;; my-org.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-org' package is used to load the required configuration


(use-package org
  :defer t
  :ensure org-plus-contrib
  :config
  (setq org-agenda-files '("~/org/Agenda/agenda.org"))
  (setq org-default-notes-file "~/org/Notes/notes.org")
  (setq org-capture-templates '(
                                ("t" "Todo" entry (file+headline "~/todo.org" "TODO")
                                 "* TODO %^{Item}\n** %^{Description}\n")
                                ))
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-log-into-drawer t)
  )

(use-package org-drill
  :ensure t)

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
