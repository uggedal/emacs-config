;;
;; Package archives
;;

(require 'package)
(require 'use-package)

;;
;; Appearance
;;

(setq modus-themes-italic-constructs t
      modus-themes-subtle-line-numbers t
      modus-themes-mode-line '(borderless (padding . 4)))

(load-theme 'modus-operandi)

(set-face-attribute 'default nil :font "SF Mono" :height 130)

;;
;; Built-in packages
;;

(use-package emacs
  :init
  (setq ring-bell-function 'ignore
        use-short-answers t
        tab-always-indent 'complete
        truncate-lines t
        read-extended-command-predicate #'command-completion-default-include-p)
  (setq-default fill-column 79)
  (when (and (eq system-type 'darwin) (display-graphic-p))
    (setq ns-alternate-modifier nil
          ns-command-modifier 'meta))
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil))

(use-package simple
  :init
  (setq column-number-mode t
        save-interprogram-paste-before-kill t)
  (setq-default indent-tabs-mode nil)
  :bind ([remap zap-to-char] . zap-up-to-char)
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill)))

(use-package minibuffer
  :init
  (setq completion-styles '(flex basic partial-completion emacs22)
        completions-format 'one-column
        completions-max-height 15
        completion-auto-help 'visible
        completion-auto-select 'second-tab
        completion-show-help nil
        completions-sort nil
        completions-header-format nil)
  :bind (:map completion-in-region-mode-map
              ("C-n" . minibuffer-previous-completion)
              ("C-p" . minibuffer-next-completion)))

(use-package display-line-numbers
  :hook (conf-mode prog-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package frame
  :bind (("C-x C-p" . previous-window-any-frame)
         ("C-x C-n" . next-window-any-frame)))

(use-package pixel-scroll
  :init
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(use-package files
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "auto-save/") t))
        backup-directory-alist
        `(("." . ,(expand-file-name
                   (concat user-emacs-directory "backups"))))))

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package autorevert
  :config
  (global-auto-revert-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

(use-package dired
  :config
  (setq dired-listing-switches "-alFh \"-D %Y-%m-%d %H:%M\""))

(use-package paragraphs
  :config
  (setq sentence-end-double-space nil))

(use-package ispell
  :init
  (setq ispell-program-name "aspell"
        ispell-silently-savep t))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        org-cycle-separator-lines 1
        org-todo-keywords '((sequence "TODO" "NEXT" "DOING" "|" "DONE"))
        org-todo-keyword-faces '(
                                 ("NEXT" .
                                  '(modus-themes-intense-blue org-todo))
                                 ("DOING" .
                                  '(modus-themes-intense-yellow org-todo)))
        org-startup-with-inline-images t
        org-image-actual-width nil
        org-goto-interface 'outline-path-completion
        org-outline-path-complete-in-steps nil
        org-agenda-files '("~/src/notes/journal.org")
        org-capture-templates
	'(
	  ("j" "Journal Entry"
           entry (file+olp+datetree "~/src/notes/journal.org")
           "* %?"
           :empty-lines 1))))

(use-package eglot
  :bind (("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f" . eglot-format)
         ("C-c l r" . eglot-rename)
         ("C-c l c" . eglot-code-actions))
  :hook (python-base-mode . eglot-ensure))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

;;
;; Third party packages
;;

(use-package which-key
  :ensure t
  :defer 0
  :config
  (which-key-mode))
