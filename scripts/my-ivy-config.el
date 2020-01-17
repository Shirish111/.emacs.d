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
	    (setq ivy-count-format "(%d/%d) "))
  :bind (("C-c C-r" . ivy-resume)))

(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (counsel-M-x . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

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
	 ;("C-c c" . counsel-compile)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-x l" . counsel-locate)))

(provide 'my-ivy-config)

;;; my-ivy-config package ends here
