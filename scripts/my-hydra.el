;;; my-hydra.el --- Configuration to be loaded while starting emacs
;;; Commentary

;; The `my-hydra' package is used to load the required configuration

;; Hydra
(use-package hydra
  :ensure t
  :delight
  )

;; Multiple Cursors
(defhydra hydra-multiple-cursors (global-map "ESC m")
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_r_] Reverse Search
 [_S_] Mark all in region regexp
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("S" mc/mark-all-in-region-regexp :exit t)
  ("s" phi-search :exit t)
  ("r" phi-search-backward :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

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
      ("j" projectile-rails-find-job         "job")
      ("p" projectile-rails-find-policy      "policy")
      ("h" projectile-rails-find-helper      "helper")
      ("l" projectile-rails-find-lib         "lib")
      ("s" projectile-rails-find-service     "service")
      ("i" projectile-rails-find-initializer "initializer")
      ("r" projectile-rails-goto-gemfile     "Gemfile")
      ("g" projectile-rails-goto-            "routes")
      ("@" projectile-rails-find-mailer      "mailer"))

(define-key projectile-rails-mode-map (kbd "s-r") 'my-hydra-projectile-rails-find/body)
;(define-key projectile-rails-mode-map (kbd "s-r") 'hydra-projectile-rails/body)

(provide 'my-hydra)

;;; my-hydra package ends here
