;;; my-eshell.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-eshell' package is used to load the required configuration

;; (setq eshell-prompt-function
;;   (lambda ()
;;     (concat (user-login-name) (s-replace "Directory " " " (pwd))
;;       (if (= (user-uid) 0) " # " " \n$ "))))


(provide 'my-eshell)

;;; my-eshell package ends here

