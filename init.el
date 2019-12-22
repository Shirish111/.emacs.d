;
;
; Package Archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Use Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/scripts")
(use-package my-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-format-fallback-style "llvm")
 '(clang-format-style "Google")
 '(package-selected-packages
   (quote
    (dashboard magit clang-format flycheck yasnippet company-quickhelp company move-text avy Avy ivy-mode counsel ivy auto-complete which-key smartparens hydra multiple-cursors expand-region spacemacs-theme zenburn-theme cyberpunk-theme rainbow-delimiters highlight-parentheses use-package smex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
