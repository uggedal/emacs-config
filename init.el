;;; init.el --- Main configuration
;;; Commentary:

;; Minimal Emacs configuration. Strive to use built-ins where possible.

;;; Code:

;;;
;;; Package archives
;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa" . 70)))
(require 'use-package)

;;;
;;; Appearance
;;;

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
  (keymap-global-unset "C-z")
  :bind ("M-`" . other-frame))

(use-package pixel-scroll
  :init
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;;;
;;; Core
;;;

(use-package emacs
  :init
  (setq ring-bell-function 'ignore
        use-short-answers t
        create-lockfiles nil
        tab-always-indent 'complete
        sentence-end-double-space nil
        read-extended-command-predicate #'command-completion-default-include-p
        enable-recursive-minibuffers t
        bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t
        x-stretch-cursor t
        scroll-conservatively 999
        scroll-margin 3)

  (setq-default fill-column 79
                display-fill-column-indicator-character ?\u254e
                history-length 1000
                truncate-lines t)

  (when (and (eq system-type 'darwin) (display-graphic-p))
    (setq ns-alternate-modifier nil
          ns-command-modifier 'meta))

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  :hook ((prog-mode conf-mode text-mode) .
         (lambda () (setq-local show-trailing-whitespace t
                                indicate-empty-lines t))))

(use-package server
  :config
  (server-start))

;;;
;;; Editing
;;;

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

(use-package subword
  :config
  (subword-mode))

(use-package move-dup
  :ensure t
  :bind (("M-<up>" . move-dup-move-lines-up)
         ("M-<down>" . move-dup-move-lines-down)))

;;;
;;; Files
;;;

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

(use-package so-long
  :config
  (global-so-long-mode))

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))

;;;
;;; History
;;;

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

;;;
;;; Navigation and Search
;;;

(use-package isearch
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char)))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

(use-package dired
  :bind (:map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :config
  (setq dired-listing-switches "-alFh \"-D%Y-%m-%d %H:%M\"")
  (put 'dired-find-alternate-file 'disabled nil))

(use-package goto-addr
  :init
  (setq goto-address-uri-schemes '("http://" "https://"))
  :hook ((vterm-mode . goto-address-mode)
         ((prog-mode conf-mode) . goto-address-prog-mode)))

;;;
;;; Completion
;;;

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
  :custom (corfu-echo-delay t)
  :config (corfu-echo-mode))

(use-package corfu-popupinfo
  :after corfu
  :custom (corfu-popupinfo-delay nil)
  :config (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :bind ([remap dabbrev-expand] . cape-dabbrev))

(use-package consult
  :ensure t
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :bind (([remap yank-pop] . consult-yank-from-kill-ring)
         ([remap goto-line] . consult-goto-line)))

;;;
;;; Shell
;;;

(use-package vterm
  :ensure t
  :preface
  (defun unbind-vterm-meta-backtick ()
    "For making other-frame binding work"
    (keymap-set vterm-mode-map "M-`" nil))
  :defines vterm-mode-map
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key))
  :hook (vterm-mode . unbind-vterm-meta-backtick))

(use-package multi-vterm
  :ensure t
  :bind ([remap project-shell] . multi-vterm-project))

;;;
;;; VCS
;;;

(use-package vc-hooks
  :custom (vc-handled-backends '(Git) "Only enable git backend"))

(use-package git-commit
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook ((prog-mode conf-mode text-mode vc-dir-mode) . turn-on-diff-hl-mode))

(use-package diff-hl-dired
  :hook (dired-mode))

;;;
;;; IDE
;;;

(use-package eglot
  :preface (defun toggle-eglot-format-hook ()
             (if (eglot-managed-p)
                 (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
               (remove-hook 'before-save-hook #'eglot-format-buffer)))
  :custom (eglot-autoshutdown t)
  :bind (("C-c l f" . eglot-format)
         ("C-c l r" . eglot-rename)
         ("C-c l c" . eglot-code-actions))
  :hook ((python-base-mode . eglot-ensure)
         (eglot-managed-mode . toggle-eglot-format-hook)))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :hook (emacs-lisp-mode-hook . flymake-mode))

(use-package eldoc
  :custom (eldoc-echo-area-use-multiline-p nil "Single-line doc string"))

;;;
;;; Writing
;;;

(use-package ispell
  :custom
  (ispell-program-name "hunspell")
  (ispell-silently-savep t)
  (ispell-dictionary "en_US,nb_NO")
  :init
  (setenv "LANG" "en_US.UTF-8")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,nb_NO"))

(use-package flyspell
  :hook ((text-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ([remap flyspell-auto-correct-previous-word] .
               flyspell-correct-wrapper)))

(use-package calendar
  :custom (calendar-week-start-day 1))

(use-package org
  :defer t
  :custom
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-cycle-separator-lines 1)
  (org-blank-before-new-entry (quote ((heading .t) (plain-list-item . nil))))
  (org-todo-keywords '((sequence "TODO" "NEXT" "DOING" "|" "DONE")))
  (org-todo-keyword-faces '(("NEXT" . '(org-level-3 org-todo))
                            ("DOING" . '(org-level-4 org-todo))))
  (org-startup-with-inline-images t)
  (org-ellipsis " …")
  (org-image-actual-width nil)
  (org-special-ctrl-a/e t)
  (org-directory "~/src/notes")
  (org-agenda-files '("work.org" "personal.org")))

(use-package org-goto
  :after org
  :custom (org-goto-interface 'outline-path-completion))

(use-package org-agenda
  :custom (org-agenda-span 14)
  :bind ("C-c a" . org-agenda))

(use-package org-capture
  :custom (org-capture-templates
           '(
             ("j" "Journal Entry"
              entry (file+olp+datetree "~/src/notes/work.org")
              "* %?"
              :empty-lines 1)))
  :bind ("C-c c" . org-capture))

(use-package org-appear
  :ensure t
  :hook org-mode)

;;;
;;; Programming modes
;;;

(use-package sh-script
  :preface (defun enable-indent-tabs-mode ()
             (setq-local indent-tabs-mode t))
  :custom (sh-basic-offset 8)
  :hook (sh-mode . enable-indent-tabs-mode))

(use-package markdown-mode
  :ensure t
  :custom (markdown-enable-highlighting-syntax t "Highlight code blocks")
  :mode (("README\\.md\\'" . gfm-mode))
  :commands (markdown-mode gfm-mode))

(use-package terraform-mode
  :ensure t)

;;;
;;; Misc
;;;

(use-package which-key
  :ensure t
  :defer 0
  :functions which-key-mode
  :config
  (which-key-mode))

(provide 'init)
;;; init.el ends here
