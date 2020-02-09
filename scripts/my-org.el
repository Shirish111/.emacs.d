;;; my-org.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-org' package is used to load the required configuration


(use-package org
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode) (auto-fill-mode t)))
  :defer t
  :ensure org-plus-contrib
  :config
  (define-key org-mode-map (kbd "C-'") nil)
  ;; (setq org-agenda-files '("/path/to/agenda.org"))
  ;; (setq org-default-notes-file "/path/to/notes.org")

  ;; Capture Templates
  ;; (setq org-capture-templates '(
  ;;                               ;; Set org capture templates here...
  ;;                               ("t" "Todo" entry (file+headline "/path/to/todo.org" "TODO")
  ;;                              "* TODO %^{Item}\n** %^{Description}\n")
  ;;                               ))
  (setq org-log-done 'time)
  (global-set-key (kbd "C-c c") 'org-capture)
  (use-package ox-twbs
    :ensure t
    :demand t
    )
  (use-package ox-twbs
    :ensure t)
  ;; Org Publish
  (require 'ox-publish)
  ;; (setq org-publish-project-alist
  ;;       '(
  ;;         ;; ... add all the components here
  ;;         ("org-notes"
  ;;          :base-directory "/directory/path/to/org/files"
  ;;          :base-extension "org"
  ;;          :publishing-directory "/directory/path/to/publish"
  ;;          :publishing-function org-twbs-publish-to-html
  ;;          :recursive t
  ;;          :headline-levels 4
  ;;          :auto-preamble t
  ;;          )
  ;;         )
  ;;       )
  (setq org-src-fontify-natively t)
  (use-package htmlize
    :ensure t
    :demand t)
  ;(add-hook 'before-save-hook (lambda () (when (eq major-mode 'org-mode) (save-excursion (org-publish-current-file)))))
  )

(set-language-environment "UTF-8")

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
  (use-package ob-restclient
  :ensure t)
  (add-hook 'org-mode-hook   (lambda ()
			       (org-babel-do-load-languages
				'org-babel-load-languages
				'(
				  (python . t)
				  (shell . t)
				  (C . t)
                                  (restclient . t)
                                  (dot . t )
                                  (latex . t)))
			       ))
  :ensure t)

(use-package org-journal
  :ensure t
  :demand t
  :config
  ;;(setq org-journal-dir "/path/to/org-journal/")
  (setq org-journal-date-format "%A, %d %B %Y"))

(provide 'my-org)

;;; my-org package ends here
