;;; my-eshell.el --- Configuration to be loaded while starting emacs
;;; Commentary
;; The `my-eshell' package is used to load the required configuration

;; (setq eshell-prompt-function
;;   (lambda ()
;;     (concat (user-login-name) (s-replace "Directory " " " (pwd))
;;       (if (= (user-uid) 0) " # " " \n$ "))))

;; Set up the Correct Path
;; (setenv "PATH"
;;         (concat
;;          "/usr/local/bin:/usr/local/sbin:"
;;          (getenv "PATH")))

;;Pager Setup
(setenv "PAGER" "cat")


(use-package esh-help
  :ensure t
  :config
  (setup-esh-help-eldoc))

(use-package eshell-did-you-mean
  :init ()
  (add-hook 'eshell-mode-hook 'eshell-did-you-mean-setup)
  :ensure t
  )


(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline)
  )

(use-package eshell-fringe-status
  :ensure t
  :config
  :init
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)
  )


(use-package eshell-z
  :ensure t
  :demand t
  )

(setq eshell-prompt-function
      (lambda ()
              "A simple prompt."
              (concat
               (propertize (concat "shirish@ ") 'face `(:foreground "orange"))
               (propertize (concat (eshell/pwd) "$: ") 'face `(:foreground "orange"))
              )))

;; Bash Aliases
;; (use-package load-bash-alias
;;   :ensure t
;;   :config
;;   (setq load-bash-alias-bashrc-file "/home/shirish/.bash_it/aliases/custom.aliases.bash"))

(defun crontab-e ()
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

(provide 'my-eshell)

;;; my-eshell package ends here
