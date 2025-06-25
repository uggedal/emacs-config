;;; init.el --- Main configuration
;;; Commentary:

;; Minimal Emacs configuration. Strive to use built-ins where possible.

;;; Code:

;;;
;;; Bootstrap
;;;

(setopt custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;
;;; Core
;;;

(use-package emacs
  :init (setopt
	 ;; Disable backup files:
	 create-lockfiles nil
	 make-backup-files nil
	 backup-inhibited t
	 ;; ⌘ as Meta, left ⌥ for Super and right ⌥ for writing non-ASCII:
	 ns-command-modifier 'meta
	 ns-option-modifier 'super
	 ns-right-option-modifier 'none))

;;;
;;; Appearance
;;;

(use-package faces
  :init
  (set-face-attribute 'default nil :font "SF Mono" :height 160)
  (set-face-attribute 'fixed-pitch nil :font "SF Mono")
  (set-face-attribute 'variable-pitch nil :font "New York"))

(use-package modus-themes
  :init (load-theme 'modus-operandi t))

;;;
;;; Writing
;;;

(setopt calendar-week-start-day 1
        calendar-date-style 'iso)

;;;
;;; Development
;;;

(use-package treesit)

(use-package sh-script
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
  (unless (treesit-language-available-p 'bash)
    (treesit-install-language-grammar 'bash)))

(use-package python-ts-mode
  :mode "\\.py\\'"
  :init
  (add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (unless (treesit-language-available-p 'bash)
    (treesit-install-language-grammar 'bash)))

(use-package eglot
  :hook ((python-base-mode sh-base-mode) . eglot-ensure)
  :config (add-to-list 'eglot-server-programs '(python-mode . ("uv" "run" "pylsp"))))

;;;
;;; Third Party
;;;

(use-package magit
  :ensure t
  :init (setopt magit-define-global-key-bindings 'recommended))

;;;
;;; Help
;;;

(use-package which-key
  :defer t
  :hook (after-init-hook . which-key-mode))

(provide 'init)
;;; init.el ends here
