;
;
; Package Archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;;; Use Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/my-emacs/scripts/src")
(use-package my-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-format-fallback-style "llvm")
 '(clang-format-style "Google")
 '(counsel-projectile-sort-buffers t)
 '(counsel-projectile-sort-files t)
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(eshell-modules-list
   '(eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-smart eshell-term eshell-tramp eshell-unix eshell-xtra))
 '(eshell-prompt-regexp "^[^$
]*\\$ ")
 '(org-agenda-files nil)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(live-py-mode yaml-mode highlight-indent-guides company-box itail annotate deft ivy-posframe kaolin-themes solarized-theme ivy-hydra helm-ag helm-projectile helm-swoop evil-leader general evil-surround evil god-mode appt paradox company-prescient dumb-jump vterm ivy-prescient prescient amx forge powerthesaurus diff-hl counsel-tramp doom-modeline auto-package-update persistent-scratch hlinum switch-window treemacs-magit eshell-z eshell-git-prompt eshell-fringe-status eshell-did-you-mean eshell-did-you-mean-setup esh-help tiny auctex doom-themes all-the-icons neotree ox-latex prettier-js counsel-projectile vimish-fold emmet-mode mustache-mode ob-restclient restclient epresent htmlize ox-twbs ox-publish org-journal monokai-theme sanityinc-tomorrow-bright markdown-mode org-drill alert ag dracula-theme phi-search projectile-rails js-comint json-mode indium nodejs-repl rubocopfmt treemacs-evil treemacs org-bullets org-mode org-babel-eval-in-repl org-contrib-mode org-plus-contrib delight dashboard magit clang-format flycheck yasnippet company-quickhelp company move-text avy Avy ivy-mode counsel ivy auto-complete which-key smartparens hydra multiple-cursors expand-region spacemacs-theme zenburn-theme cyberpunk-theme rainbow-delimiters highlight-parentheses use-package smex))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'org-reveal-export-to-html)
     (eval add-hook 'after-save-hook 'org-babel-tangle nil t)))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "DarkOrange1"))))
 '(term-color-blue ((t (:background "gray0" :foreground "White")))))
(put 'downcase-region 'disabled nil)
