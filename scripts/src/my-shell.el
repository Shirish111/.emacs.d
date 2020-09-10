;;; my-shell.el --- Configuration to be loaded while starting emacs
;;; Commentary
;; The `my-shell' package is used to load the required configuration

(use-package term
  :init
  :ensure t
  :config
  (setq my-prompt-regexp "→ \\|> ")
  (define-key term-mode-map (kbd "C-<up>") 'term-previous-input)
  (define-key term-mode-map (kbd "C-<down>") 'term-next-input)
  (define-key term-mode-map (kbd "C-a") (lambda ()
                                          (interactive)
                                          (beginning-of-line)
                                          (re-search-forward my-prompt-regexp (line-end-position) t)))
  (define-key term-raw-map (kbd "s-j") 'term-line-mode)
  (define-key term-mode-map (kbd "s-j") 'term-char-mode)
  (define-key term-mode-map (kbd "C-l") 'term-send-raw)
  (define-key term-mode-map (kbd "C-c C-k") 'term-kill-subjob)
  )

;; Vterm
  (use-package vterm
    :bind ("s-t" . vterm-toggle)
    :demand t
    :ensure t
)

(use-package shx
  :init
  :ensure t
  :config
  )

(provide 'my-shell)

;;; my-shell package ends here
