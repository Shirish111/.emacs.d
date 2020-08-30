;;; my-hydra.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-hydra' package is used to load the required configuration

;; Hydra
(use-package hydra
  :ensure t
  :delight
  )

;; Java
(defhydra hydra-meghanada (:hint nil :exit t)
"
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
_i_: meghanada-import-all        _r_: meghanada-reference
_g_: magit-status                _T_: meghanada-typeinfo
_l_: helm-ls-git-ls
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ("l" helm-ls-git-ls)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))

;; Rails Hydra
(defhydra my-hydra-projectile-rails-find (:color blue :columns 8)
      "Find a resources"
      ("m" projectile-rails-find-model       "model")
      ("v" projectile-rails-find-view        "view")
      ("c" projectile-rails-find-controller  "controller")
      ("j" my-projectile-rails-find-job      "job")
      ("p" my-projectile-rails-find-policy   "policy")
      ("h" projectile-rails-find-helper      "helper")
      ("l" projectile-rails-find-lib         "lib")
      ("d" (lambda ()(interactive) (projectile-rails-goto-file "config/database.yml")) "database.yml")
      ("S" (lambda ()(interactive) (projectile-rails-goto-file "db/schema.rb")) "schema")
      ("a" (lambda ()(interactive) (projectile-rails-goto-file "config/application.yml")) "application.yml")
      ("e" (lambda ()(interactive) (projectile-rails-goto-file "config/initializers/00_custom_env.rb")) "Custom Env")
      ("s" my-projectile-rails-find-service     "service")
      ("i" projectile-rails-find-initializer "initializer")
      ("g" projectile-rails-goto-gemfile     "Gemfile")
      ("r" projectile-rails-goto-routes      "routes")
      ("@" projectile-rails-find-mailer      "mailer"))

;(define-key projectile-rails-mode-map (kbd "s-r") 'my-hydra-projectile-rails-find/body)
;(define-key projectile-rails-mode-map (kbd "s-r") 'hydra-projectile-rails/body)

(provide 'my-hydra)

;;; my-hydra package ends here
