;; Divert customaizations to separate file:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; Basic settings
(setq ring-bell-function 'ignore  ; No audible bell
      use-short-answers t) ; y/n in stead of yes/no

(load-theme 'modus-operandi)
