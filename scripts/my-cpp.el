;;; my-cpp.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-cpp' package is used to load the required configuration
;; Clang-format
(use-package clang-format
  :delight
  :ensure t
  :init
  (add-hook 'before-save-hook (lambda () (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)) (clang-format-buffer))))
  :config
  (setq clang-format-style "Google")
  ;; `NOTE': Set clang-format executable location
  (setq clang-format-executable "/usr/local/bin/clang-format")
  )
(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(defun my-cpp-fetch-func-params()
  "
This function copies function paramters as a string
Example
void f(int a, int b) {
  // Some Code...
}
On running the function it will return \"int a, int b\".
"
  (interactive)
  (c-beginning-of-defun)
  (search-forward "(")
  (setq my-cpp-begin-pos (point))
  (search-forward ")")
  (backward-char)
  (buffer-substring-no-properties my-cpp-begin-pos (point))
  )
(defun my-cpp-debug-func-params()
  "my-cpp-debug-func-params is an interactive lisp command.
It inserts the print syntax for the function paramters in C++.
Example:
void f(int a, int b) {
  //... some code
}
When you run the command it will become
void f(int a, int b) {
  cout << \" a = \" << a << \" b = \" << b << \"\\n\";
  //... some code
}"

  (interactive)
  (save-excursion
    (let ((my-cpp-line (my-cpp-fetch-func-params))
          (my-cpp-ht (ht ("int" t)
                         ("short")
                         ("Node")
                         ("TreeNode")
                         ("List")
                         ("float" t)
                         ("double" t)
                         ("long long" t)
                         ("long double" t)
                         ("string" t)
                         ("long" t))))
      (forward-line)
      (insert
       (string-join
        (-concat '("cout")
                 (--map (format " << \" %s: \" << %s " it it)
                        (--remove
                         (ht-get my-cpp-ht it)
                         (s-split-words my-cpp-line))) '(" << endl;\n"))))
      (clang-format-buffer))))

(eval-after-load 'cc-mode
  '(progn
     (define-key c++-mode-map (kbd "C-c l") 'my-cpp-debug-func-params)
     )
  )

(defun my-cpp-cout-variable-f()
  (interactive)
  (let ((end_pos (point))
        (start_pos (progn (left-word) (point)))
        )
    (kill-region start_pos end_pos)
    (setq my-cpp-word (current-kill 0))
  (insert (concat "<< " "\" " my-cpp-word ": \" << " my-cpp-word))))
(defun my-cpp-cin-variable-f()
  (interactive)
  (let ((end_pos (point))
        (start_pos (progn (left-word) (point)))
        )
    (kill-region start_pos end_pos)
    (setq my-cpp-word (current-kill 0))
  (insert (concat  ">> " my-cpp-word))))


(global-set-key (kbd "M-]") 'my-cpp-cout-variable-f)
(global-set-key (kbd "M-[") 'my-cpp-cin-variable-f)

;; Flycheck
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")(setq flycheck-clang-language-standard "c++14")))
  
(provide 'my-cpp)

;;; my-cpp package ends here

