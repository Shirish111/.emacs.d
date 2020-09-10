;;; my-custom.el-- - Common Utitlies
;;; Commentary
;; The `my-custom' package is used to load the common utilities

;; Navigation
(global-set-key (kbd "C-<up>") '(lambda () (interactive) (next-line -4)))

(global-set-key (kbd "C-<down>") '(lambda () (interactive) (next-line 4)))

(global-set-key (kbd "C-<left>") '(lambda () (interactive) (left-char 4)))

(global-set-key (kbd "C-<right>") '(lambda () (interactive) (right-char 4)))

(global-set-key (kbd "s-<up>") '(lambda () (interactive) (next-line -12)))

(global-set-key (kbd "s-<down>") '(lambda () (interactive) (next-line 12)))

(global-set-key (kbd "s-y") 'helm-show-kill-ring)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key (kbd "s-<left>") 'beginning-of-line-text)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "s-a") 'mark-whole-buffer)

(global-set-key (kbd "<s-return>") (kbd "C-e C-m <tab>"))

;;(global-set-key (kbd "C-l") '(lambda() (interactive) (copy-region-as-kill (line-beginning-position) (line-end-position)) (message "Line Copied")))
(global-set-key (kbd "s-c") '(lambda() (interactive) (copy-region-as-kill (beginning-of-buffer) (end-of-buffer) (message "Copied Buffer"))))
(global-set-key (kbd "s-k") '(lambda() (interactive) (mark-whole-buffer) (kill-region (region-beginning) (region-end)) (message "Cut Buffer")))

;; Copy filename
(defun my-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(global-set-key (kbd "C-c C-f") 'my-copy-file-name-to-clipboard)

;; Kill Whole line
(global-set-key (kbd "C--") '(lambda () (interactive) (kill-whole-line)))

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
(setq shell-command-switch "-c") ; Disable this for mac operating system
(define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)

;; Keyboard macros
;; Document Ruby Class
(fset 'my-ruby-document-class-or-module
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 134217830 right 67108896 134217830 134217847 up 5 return 35 32 25] 0 "%d")) arg)))

(add-hook 'ruby-mode-hook (lambda () (define-key ruby-mode-map (kbd "C-c d") 'my-ruby-document-class-or-module)))

(fset 'my-swap-params
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\346\346\364\364\342\342\342\364\364" 0 "%d")) arg)))

(global-set-key (kbd "M-s p") 'my-swap-params)
                                        ;(global-set-key (kbd "M-z") 'beginning-of-line-text)

;; Copy Source Code Block
(fset 'my-org-copy-src-block
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([down 3 39 8388715 134217847 3 39] 0 "%d")) arg)))

(add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "s-x") 'my-org-copy-src-block)))

(add-hook 'shell-mode-hook (lambda () (define-key shell-mode-map (kbd "C-c C-k") 'comint-kill-subjob)))

;; Highlight current line
;;(global-hl-line-mode t)

(pending-delete-mode t)

;; Switch Buffers
(global-set-key (kbd "s-b") 'counsel-switch-buffer)

;; Yes No alias
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'my-custom)

;;; my-custom package ends here
