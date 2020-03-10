;;; my-ivy-config.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-ivy-config' package is used to load the required configuration

;; Ivy Mode
(use-package ivy
  :delight ivy-mode
  :ensure t
  :demand t
  :config (progn
	    (ivy-mode 1)
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

;; Counsel
(use-package counsel
  :delight
  :ensure t
  :demand t
  :bind (("M-s s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f". counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
         ;;("C-c c" . counsel-compile)
         ("C-x f" . counsel-projectile-find-file)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)
         ("C-<tab>" . counsel-company))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package counsel-projectile
  :delight
  :ensure t
  :demand t
  :init
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

;; Views
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Ivy Posframe
;; (use-package ivy-posframe
;;   :init
;;   :ensure t
;;   :config
;;   ;; display at `ivy-posframe-style'
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;   ;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-)))
;;   (ivy-posframe-mode 1)
;;   )



(provide 'my-ivy-config)

;;; my-ivy-config package ends here
