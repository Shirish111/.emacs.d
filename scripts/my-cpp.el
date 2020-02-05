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
  (setq clang-format-executable "/usr/local/clang-9.0.0/bin/clang-format")
  )

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))

(use-package compile
  :init
  (define-key prog-mode-map (kbd "C-c x") 'recompile))

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
  (require 'ht)
  (interactive)
  (save-excursion
    (let ((my-cpp-line (my-cpp-fetch-func-params))
          (my-cpp-ht (ht ("int" t)
                         ("float" t)
                         ("double" t)
                         ("string" t)
                         ("long" t))))
      (forward-line)
      (insert
       (string-join
        (-concat '("cout")
                 (--map (format " << \"%s = \" << %s " it it)
                        (--remove
                         (ht-get my-cpp-ht it)
                         (s-split-words my-cpp-line))) '(" << \"\\n\";\n"))))
      (clang-format-buffer))))

(eval-after-load 'c++-mode '(define-key c++-mode-map (kbd "C-c l") 'my-cpp-debug-func-params))
(provide 'my-cpp)

;;; my-cpp package ends here

