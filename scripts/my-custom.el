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
         (dirname (s-upper-camel-case (s-replace-all '(("II" ."2")("III" ."3")("IV". "4"))  yank_content)))
         (filepath (concat "../" dirname "/a.cpp")))
    (find-file filepath)
    (insert (concat "// " yank_content))
    )
  )

(global-set-key (kbd "C-c m") 'my-make-dir)

;; Interactive Shell
(setq shell-command-switch "-ic")

(provide 'my-custom)

;;; my-custom package ends here
