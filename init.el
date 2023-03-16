;;;;
;;;; Package archives
;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
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

(set-face-attribute 'default nil :font "SF Mono" :height 130)

;; Mode line padding:
(set-face-attribute 'mode-line nil
                :box '(:line-width 6 :color "#2b2b47"))
(set-face-attribute 'mode-line-inactive nil
                :box '(:line-width 6 :color "#2b2b2b"))

;;;;
;;;; Built-in packages
;;;;

(use-package emacs
  :init
  (setq ring-bell-function 'ignore
        use-short-answers t
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
  (setq-default fill-column 79)
  (when (and (eq system-type 'darwin) (display-graphic-p))
    (setq ns-alternate-modifier nil
          ns-command-modifier 'meta))
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  :hook ((prog-mode conf-mode text-mode) .
         (lambda () (setq show-trailing-whitespace t
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

(use-package display-line-numbers
  :hook (conf-mode prog-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package whitespace
  :config
  (setq whitespace-line-column 79
        whitespace-style '(face tabs lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package frame
  :config
  (blink-cursor-mode 0)
  :bind (("C-x C-p" . previous-window-any-frame)
         ("C-x C-n" . next-window-any-frame)))

(use-package pixel-scroll
  :init
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))

(use-package files
  :config
  (setq  backup-directory-alist
         `(("." . ,(expand-file-name
                    (concat user-emacs-directory "backups"))))
         confirm-kill-processes nil)
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
  :config
  (setq recentf-max-saved-items 100)
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

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

(use-package dired
  :config
  (setq dired-listing-switches "-alFh \"-D%Y-%m-%d %H:%M\""))

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
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package calendar
  :init
  (setq calendar-week-start-day 1))

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("M-g h" . consult-org-heading))
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
        org-agenda-files '("~/src/notes/work.org"
                           "~/src/notes/personal.org")
        org-agenda-span 14
        org-deadline-warning-days 7
        org-capture-templates
	'(
	  ("j" "Journal Entry"
           entry (file+olp+datetree "~/src/notes/work.org")
           "* %?"
           :empty-lines 1))))

(defun my-eglot-format ()
  (if (eglot-managed-p)
      (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (remove-hook 'before-save-hook #'eglot-format-buffer)))

(use-package eglot
  :config
  (setq eglot-autoshutdown t)
  :bind (("C-c l f" . eglot-format)
         ("C-c l r" . eglot-rename)
         ("C-c l c" . eglot-code-actions))
  :hook ((python-base-mode . eglot-ensure)
         (eglot-managed-mode . my-eglot-format)))

(use-package vc-hooks
  :config
  (setq vc-handled-backends '(Git)))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

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
  :bind (:map minibuffer-local-map
             ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotator-registry
      (assq-delete-all 'file marginalia-annotator-registry)))

(use-package cape
  :ensure t
  :bind ([remap dabbrev-expand] . cape-dabbrev)
  :hook (org-mode . (lambda () (add-to-list 'completion-at-point-functions
                                            #'cape-ispell))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-cycle t))

(use-package corfu-echo
  :after corfu
  :config
  (setq corfu-echo-delay t)
  (corfu-echo-mode))

(use-package consult
  :ensure t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-find-args  "find . -not ( -wholename */.git* -prune )"
        consult-ripgrep-args '("rg" "--null" "--line-buffered" "--color=never"
                              "--max-columns=1000" "--path-separator" "/"
                              "--smart-case" "--no-heading" "--with-filename"
                              "--line-number" "--hidden" "-g" "!.git"))

  (add-to-list 'consult-buffer-sources
          `(:name     "Known Project"
            :narrow   (?P . "Project")
            :category project
            :face     consult-project-extra-projects
            :history  file-name-history
            :action   ,#'consult--file-action
            :items    ,#'project-known-project-roots)
          'append)

  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap isearch-forward] . consult-line)
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

(use-package org-appear
  :ensure t
  :hook org-mode)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-enable-highlighting-syntax t))
