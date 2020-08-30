;;; my-ruby.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-ruby' package is used to load the required configuration

;; Ruby mode
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)

  :bind
  ((("C-c C-e" . ruby-send-region)))
  :config
  (define-key ruby-mode-map (kbd "C-c r") nil))  ;; Rebind since Rubocop uses C-c C-r

;; Web mode
(use-package web-mode
  :ensure t
  :mode "\\.erb\\'")

;; RVM
(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

;; Yari
(use-package yari
  :ensure t
  :init
  (add-hook 'ruby-mode-hook
            (lambda ()
              (local-set-key [f1] 'yari))))

;; Inf-ruby
(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Rubocop
(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

;; Robe
(use-package robe
  :ensure t
  :bind (("C-M-." . robe-jump)
         ("s-." . find-tag))

  :init
  (add-hook 'ruby-mode-hook 'robe-mode)

  :config
  (defadvice inf-ruby-console-auto
    (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

;; Ruby Tools
(use-package ruby-tools
  :ensure t
  :config
  (define-key ruby-tools-mode-map (kbd "C-;") nil)
  (define-key ruby-tools-mode-map (kbd "C-'") nil)
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :diminish ruby-tools-mode)

;; Rubocopfmt
(use-package rubocopfmt
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocopfmt-mode)
  )

;; Projectile-rails
(use-package projectile-rails
  :ensure t
  :init
  (add-hook 'ruby-mode-hook (lambda () (projectile-rails-global-mode)))
  :config
  (defun projectile-rails-find-job ()
    "Find a Job."
    (interactive)
    (projectile-rails-find-resource
     "job: "
     '(("app/jobs/" "\\(.+?\\)\\(_job\\)?\\.rb$"))
     "app/jobs/${filename}_job.rb"))

  (defun projectile-rails-find-policy ()
    "Find a Policy."
    (interactive)
    (projectile-rails-find-resource
     "policy: "
     '(("app/policies/" "\\(.+?\\)\\(_policy\\)?\\.rb$"))
     "app/jobs/${filename}_policy.rb"))
  
  (defun projectile-rails-find-service ()
    "Find a Service."
    (interactive)
    (projectile-rails-find-resource
     "service: "
     '(("app/services/" "\\(.+?\\)\\(\\)?\\.rb$"))
     "app/services/${filename}.rb")))

(provide 'my-ruby)

;;; my-ruby package ends here
