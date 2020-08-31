;;; my-ivy.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-ivy' package is used to load the required configuration

;; Ivy Mode
(use-package ivy
  :delight ivy-mode
  :ensure t
  :demand t
  :config (progn
	    (ivy-mode 1)
            (setq ivy-height 30)
	    (setq ivy-use-virtual-buffers t)
	    (setq ivy-count-format "(%d/%d) ")
            (setq ivy-extra-directories ()))
  :bind (("C-c C-r" . ivy-resume)))

  (setq ivy-re-builders-alist
  '((read-file-name-internal . ivy--regex-ignore-order)
  (counsel-M-x . ivy--regex-ignore-order)
  (counsel-projectile-find-file . ivy--regex-ignore-order)
  (ivy-switch-buffer . ivy--regex-ignore-order)
  (projectile-completing-read . ivy--regex-ignore-order)
  (t . ivy--regex-ignore-order)))
  ;; Views
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Counsel
(use-package counsel
  :delight
  :ensure t
  :demand t
  :bind (
         ("M-x" . counsel-M-x)
         ("M-s s" . swiper)
         ("C-x C-f". counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("s-r" . counsel-recentf)
         ;;("C-c c" . counsel-compile)
         ("s-f" . counsel-projectile-find-file)
         ;("C-c g" . counsel-git)
         ;("C-c j" . counsel-git-grep)
         ("s-g" . counsel-ag)
         ;("C-x l" . counsel-locate)
         ;("C-<tab>" . counsel-company)
         )
  :config
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :delight
  :ensure t
  :demand t
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; Counsel Tramp
(use-package counsel-tramp
  :init
  :ensure t
  :config
  (setq tramp-default-method "ssh")
  (define-key global-map (kbd "C-c s") 'counsel-tramp))

(use-package prescient
  :init
  :demand t
  :ensure t
  :config
  (prescient-persist-mode +1)
  )

(use-package ivy-prescient
  :init
  :demand t
  :ensure t
  :config
  (ivy-prescient-mode +1)
  (add-to-list 'ivy-prescient-sort-commands 'counsel-recentf)
  (setq ivy-prescient-retain-classic-highlighting t)
  )

(use-package company-prescient
  :init
  :demand t
  :ensure t
  :config
  (company-prescient-mode +1)
  )

;; Helm
(use-package helm
  :init
  :ensure t
  :config
  (setq helm-ff-skip-boring-files t)
  :bind
  ;("M-x" . helm-M-x)
  ;("C-x C-f". helm-find-files)
  )

;; Helm Swoop
(use-package helm-swoop
  :init
  :ensure t
  :config
  )

;; Helm Projectile
(use-package helm-projectile
  :init
  :ensure t
  :config
  )

;; Helm Ag
(use-package helm-ag
  :init
  :ensure t
  :config
  (setq helm-follow-mode-persistent t)
  )

(provide 'my-ivy)

;;; my-ivy package ends here
