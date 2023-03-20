;;;;
;;;; Package archives
;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa" . 70)))
(require 'use-package)

;;;;
;;;; Appearance
;;;;

(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package nimbus-theme
  :ensure t
  :config
  (load-theme 'nimbus t))

(use-package faces
  :config
  (set-face-attribute 'default nil :font "SF Mono" :height 130)

  ;; Remove link underline:
  (set-face-attribute 'link nil :underline nil)

  ;; Mode line padding:
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 6 :color "#2b2b47"))
  (set-face-attribute 'mode-line-inactive nil
                      :box '(:line-width 6 :color "#2b2b2b")))

(use-package display-line-numbers
  :hook (conf-mode prog-mode))

(use-package display-fill-column-indicator
  :hook (conf-mode prog-mode))

(use-package whitespace
  :config
  (setq whitespace-line-column 79
        whitespace-style '(face tabs lines-tail))
  :hook (conf-mode prog-mode))

(use-package frame
  :config
  (blink-cursor-mode 0)
  :bind ("M-`" . other-frame))

(use-package pixel-scroll
  :init
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;;;;
;;;; Core
;;;;

(use-package emacs
  :init
  (setq ring-bell-function 'ignore
        use-short-answers t
        create-lockfiles nil
        tab-always-indent 'complete
        truncate-lines t
        sentence-end-double-space nil
        read-extended-command-predicate #'command-completion-default-include-p
        enable-recursive-minibuffers t
        bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t
        x-stretch-cursor t
        scroll-conservatively 999
        scroll-margin 3)

  (setq-default fill-column 79
                history-length 1000)

  (when (and (eq system-type 'darwin) (display-graphic-p))
    (setq ns-alternate-modifier nil
          ns-command-modifier 'meta))

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  :hook ((prog-mode conf-mode text-mode) .
         (lambda () (setq-local show-trailing-whitespace t
                                indicate-empty-lines t))))

(use-package simple
  :init
  (setq column-number-mode t
        save-interprogram-paste-before-kill t)
  (setq-default indent-tabs-mode nil)
  :bind (([remap zap-to-char] . zap-up-to-char)
         ([remap upcase-word] . upcase-dwim)
         ([remap downcase-word] . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill)))

(use-package server
  :custom
  (server-start))

;;;;
;;;; Files
;;;;

(use-package so-long
  :config
  (global-so-long-mode))

(use-package files
  :config
  (setq  make-backup-files nil)
  (setq-default require-final-newline t)

  (let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-dir t)))
    (make-directory (expand-file-name auto-save-dir) t)))

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package recentf
  :bind ([remap find-file-read-only] . recentf-open)
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package desktop
  :config
  (desktop-save-mode))

(use-package autorevert
  :config
  (global-auto-revert-mode))

;;;;
;;;; Built-in packages
;;;;

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

(use-package dired
  :bind (:map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :config
  (setq dired-listing-switches "-alFh \"-D%Y-%m-%d %H:%M\"")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package ispell
  :init
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell"
        ispell-silently-savep t)
  :config
  (let ((dicts "en_US,nb_NO"))
    (setq ispell-dictionary dicts)
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic dicts)))

(use-package flyspell
  :hook ((text-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package calendar
  :init
  (setq calendar-week-start-day 1))

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-hide-emphasis-markers t
        org-startup-indented t
        org-cycle-separator-lines 1
        org-blank-before-new-entry (quote ((heading .t)
                                           (plain-list-item . nil)))
        org-todo-keywords '((sequence "TODO" "NEXT" "DOING" "|" "DONE"))
        org-todo-keyword-faces '(
                                 ("NEXT" .
                                  '(org-level-3 org-todo))
                                 ("DOING" .
                                  '(org-level-4 org-todo)))
        org-startup-with-inline-images t
        org-ellipsis " …"
        org-image-actual-width nil
        org-goto-interface 'outline-path-completion
        org-special-ctrl-a/e t
        org-outline-path-complete-in-steps nil
        org-directory "~/src/notes"
        org-agenda-files '("work.org" "personal.org")
        org-agenda-span 14
        org-deadline-warning-days 7
        org-capture-templates
        '(
          ("j" "Journal Entry"
           entry (file+olp+datetree "~/src/notes/work.org")
           "* %?"
           :empty-lines 1))))

(use-package eglot
  :config
  (setq eglot-autoshutdown t)

  (defun my-eglot-format ()
    (if (eglot-managed-p)
        (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
      (remove-hook 'before-save-hook #'eglot-format-buffer)))

  :bind (("C-c l f" . eglot-format)
         ("C-c l r" . eglot-rename)
         ("C-c l c" . eglot-code-actions))
  :hook ((python-base-mode . eglot-ensure)
         (eglot-managed-mode . my-eglot-format)))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(use-package goto-addr
  :init
  (setq goto-address-uri-schemes '("http://" "https://"))
  :hook ((vterm-mode . goto-address-mode)
         ((prog-mode conf-mode) . goto-address-prog-mode)))

(use-package sh-script
  :config
  (setq sh-basic-offset 8)
  :hook (sh-mode . (lambda () (setq-local indent-tabs-mode t))))

;;;;
;;;; Third party packages
;;;;

(use-package which-key
  :ensure t
  :defer 0
  :config
  (which-key-mode))

(use-package vertico
  :ensure t
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)
  (advice-add 'marginalia--time-absolute
              :override
              (lambda (time)
                (let ((system-time-locale "C"))
                  (format-time-string "%Y-%m-%d %H:%M" time)))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
  :config
  (setq corfu-cycle t))

(use-package corfu-echo
  :after corfu
  :config
  (setq corfu-echo-delay t)
  (corfu-echo-mode))

(use-package corfu-popupinfo
  :after corfu
  :config
  (setq corfu-popupinfo-delay nil)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :bind ([remap dabbrev-expand] . cape-dabbrev))

(use-package consult
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode conf-mode text-mode vc-dir-mode) . turn-on-diff-hl-mode))


(use-package diff-hl-dired
  :hook (dired-mode))

(use-package org-appear
  :ensure t
  :hook org-mode)

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ([remap flyspell-auto-correct-previous-word] .
                                 flyspell-correct-wrapper)))

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (define-key vterm-mode-map "\M-`" nil)))
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key)))

(use-package multi-vterm
  :ensure t
  :after vterm
  :bind ([remap project-shell] . multi-vterm-project))

(use-package git-commit
  :ensure t)

;;;;
;;;; Programming modes
;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-enable-highlighting-syntax t))

(use-package terraform-mode
  :ensure t)
