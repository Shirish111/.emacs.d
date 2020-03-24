;;; my-custom.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-custom' package is used to load the required configuration

;; Navigation

(global-set-key (kbd "C-<up>") '(lambda () (interactive) (next-line -4)))

(global-set-key (kbd "C-<down>") '(lambda () (interactive) (next-line 4)))

(global-set-key (kbd "C-<left>") '(lambda () (interactive) (left-char 4)))

(global-set-key (kbd "C-<right>") '(lambda () (interactive) (right-char 4)))

(global-set-key (kbd "s-<up>") '(lambda () (interactive) (next-line -12)))

(global-set-key (kbd "s-<down>") '(lambda () (interactive) (next-line 12)))

(global-set-key (kbd "s-<left>") '(lambda () (interactive) (left-char 12)))

(global-set-key (kbd "s-<right>") '(lambda () (interactive) (right-char 12)))

(global-set-key (kbd "s-k") 'mark-whole-buffer)

;; Kill Whole line
(global-set-key (kbd "C--") '(lambda () (interactive) (kill-whole-line)))

;; Copy Whole line
(global-set-key (kbd "M-z") '(lambda () (interactive) (kill-ring-save (line-beginning-position) (line-end-position))))

;; Make Directory
(defun my-make-dir ()
  "My Make directory"
  (interactive)
  (let* ((yank_content (current-kill 0))
         (dirpath (concat "../" (s-upper-camel-case (s-replace-all '(("II" ."2")("III" ."3")("IV". "4"))  yank_content))))
         (filepath (concat dirpath "/a.cpp")))
    (make-directory dirpath)
    (insert yank_content (find-file filepath))
    )
  )
(eval-after-load 'c++-mode '(define-key c++-mode-map (kbd "C-c m") 'my-make-dir))

;; Interactive Shell
(setq shell-command-switch "-c") ; Disable this for mac os


;; Keyboard macros
;; Document Ruby Class
(fset 'my-ruby-document-class-or-module
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 134217830 right 67108896 134217830 134217847 up return 35 32 25] 0 "%d")) arg)))

(add-hook 'ruby-mode-hook (lambda () (define-key ruby-mode-map (kbd "C-c d") 'my-ruby-document-class-or-module)))

(fset 'my-swap-params
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\346\346\364\364\342\342\342\364\364" 0 "%d")) arg)))

(global-set-key (kbd "M-s p") 'my-swap-params)
(global-set-key (kbd "M-z") 'beginning-of-line-text)

;; Highlight current line
(global-hl-line-mode 1)

;; Switch frames
(global-set-key (kbd "s-d") 'select-frame-by-name)

;; Switch Buffers
(global-set-key (kbd "s-b") 'counsel-switch-buffer)

(provide 'my-custom)

;;; my-custom package ends here
