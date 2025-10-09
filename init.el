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
	 ns-right-option-modifier 'none
	 use-short-answers t)
  :hook ((text-mode prog-mode conf-mode) .
         (lambda () (setq-local show-trailing-whitespace t
                                indicate-empty-lines t))))

(use-package simple
  :custom (column-number-mode t "display col number in mode line")
  :hook (before-save . delete-trailing-whitespace))

;;;
;;; Appearance
;;;

(use-package faces
  :init
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 140)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono"  :height 1.0))

(use-package modus-themes
  :init (require-theme 'modus-themes)
  :custom (modus-themes-common-palette-overrides
	   `(
	     ;; Dim and no background for line numbers:
	     (fg-line-number-active fg-main)
	     (fg-line-number-inactive "gray50")
	     (bg-line-number-active unspecified)
	     (bg-line-number-inactive unspecified)))
  :config (load-theme 'modus-operandi :no-confirm))

;;;
;;; Writing
;;;

(setopt calendar-week-start-day 1
        calendar-date-style 'iso)

(use-package org-capture
  :custom (org-capture-templates
           '(
             ("j" "Journal Entry"
              entry (file+olp+datetree "~/src/notes/work.org")
              "* %?"
              :empty-lines 1)))
  :bind ("C-c c" . org-capture))

;;;
;;; Shell
;;;

(use-package comint
  :hook (comint-output-filter-functions . comint-osc-process-output))

(use-package shell
  :custom (shell-kill-buffer-on-exit t))

;;;
;;; Remote
;;;

(use-package files-x
  :init
  (connection-local-set-profile-variables
   'remote-bash
   '((explicit-shell-file-name . "/bin/bash")
     (tramp-remote-path . (tramp-own-remote-path))))

  (connection-local-set-profiles '(:protocol "ssh") 'remote-bash))

;;;
;;; Development
;;;

(use-package display-line-numbers
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

(use-package treesit)

(use-package sh-script
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-to-list 'treesit-language-source-alist
	       '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
  (unless (treesit-language-available-p 'bash)
    (treesit-install-language-grammar 'bash)))

(use-package python-ts-mode
  :mode "\\.py\\'"
  :init
  (add-to-list 'treesit-language-source-alist
	       '(python "https://github.com/tree-sitter/tree-sitter-python"))
  (unless (treesit-language-available-p 'python)
    (treesit-install-language-grammar 'python)))

(use-package eglot
  :hook ((python-base-mode sh-base-mode) . eglot-ensure)
  :config (add-to-list 'eglot-server-programs
		       '(python-mode . ("uv" "run" "pylsp"))))

(use-package vc-hooks
  :custom (vc-follow-symlinks t))

;;;
;;; Third Party
;;;

(use-package magit
  :ensure t
  :init (setopt magit-define-global-key-bindings 'recommended))

(use-package diff-hl
  :ensure t
  :hook (((text-mode prog-mode conf-mode) . turn-on-diff-hl-mode)
         (vc-dir-mode . turn-on-diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t "syntax highlight code blocks"))

(use-package gptel
  :ensure t
  :init
  (setopt gptel-model 'gemini-2.5-flash
	  gptel-backend (gptel-make-gemini "Gemini"
			  :key (getenv "GEMINI_API_KEY")
			  :stream t))

  (add-hook 'gptel-mode-hook
	    (lambda () (setq-local show-trailing-whitespace nil
				   markdown-line-break-properties nil))))


;;;
;;; Help
;;;

(use-package which-key
  :defer t
  :hook (after-init-hook . which-key-mode))

(provide 'init)
;;; init.el ends here
