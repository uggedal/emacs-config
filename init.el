;;
;; Package archives
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa" . 70)))
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
        sentence-end-double-space nil
        read-extended-command-predicate #'command-completion-default-include-p
        bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t)
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
  :bind (([remap zap-to-char] . zap-up-to-char)
         ([remap upcase-word] . upcase-dwim)
         ([remap downcase-word] . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill)))

(use-package display-line-numbers
  :hook (conf-mode prog-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package frame
  :custom
  (blink-cursor-mode -1)
  :bind (("C-x C-p" . previous-window-any-frame)
         ("C-x C-n" . next-window-any-frame)))

(use-package pixel-scroll
  :init
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(use-package files
  :config
  (let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-dir t))
          backup-directory-alist
          `(("." . ,(expand-file-name
                     (concat user-emacs-directory "backups")))))
    (make-directory (expand-file-name auto-save-dir) t)))

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(use-package recentf
  :config
  (recentf-mode))

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

(use-package ispell
  :init
  (setq ispell-program-name "aspell"
        ispell-silently-savep t))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("M-g h" . consult-org-heading))
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
        org-ellipsis " …"
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
  :custom
  (setq eglot-autoshutdown t)
  :bind (("C-c l f" . eglot-format)
         ("C-c l r" . eglot-rename)
         ("C-c l c" . eglot-code-actions))
  :hook (python-base-mode . eglot-ensure))

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

(use-package cape
  :ensure t
  :bind ([remap dabbrev-expand] . cape-dabbrev)
  :hook (org-mode . (lambda () (add-to-list 'completion-at-point-functions
                                            #'cape-ispell))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package consult
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-find-args  "find . -not ( -wholename */.git* -prune )"
        ;; TODO: adapt to unreleased 0.33 changes:
        consult-ripgrep-args '("rg" "--null" "--line-buffered" "--color=never"
                              "--max-columns=1000" "--path-separator" "/"
                              "--smart-case" "--no-heading" "--line-number"
                              "--hidden" "-g" "!.git" "."))
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap isearch-forward] . consult-line)
         ([remap yank] . consult-yank-from-kill-ring)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ("M-g d" . consult-flymake)
         ("M-g o" . consult-outline)
         ("M-g d" . consult-flymake)
         ("M-s f" . consult-find)
         ("M-s g" . consult-ripgrep)
         :map minibuffer-local-map
         ([remap next-matching-history-element] . consult-history)
         ([remap previous-matching-history-element] . consult-history)))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode conf-mode text-mode vc-dir-mode) . turn-on-diff-hl-mode))


(use-package diff-hl-dired
  :hook (dired-mode . diff-hl-dired-mode))
