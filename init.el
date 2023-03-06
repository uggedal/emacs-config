;;
;; Package archives
;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)
                          ("nongnu" . 80)
                          ("melpa-stable" . 70)))

(require 'use-package)

;;
;; Basic settings
;;

(setq ring-bell-function 'ignore  ; No audible bell
      use-short-answers t ; y/n in stead of yes/no
      column-number-mode t
      uniquify-buffer-name-style 'forward)
(setq-default indent-tabs-mode nil) ; Use space for indent

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(dolist (mode-hook '(conf-mode-hook prog-mode-hook))
  (add-hook mode-hook 'display-line-numbers-mode))

(save-place-mode 1)

;;
;; Appearance
;;

(setq modus-themes-italic-constructs t
      modus-themes-subtle-line-numbers t
      modus-themes-mode-line '(borderless (padding . 4)))

(load-theme 'modus-operandi)

(set-face-attribute 'default nil :font "SF Mono" :height 140)


;;
;; Built-in packages
;;

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"
        ispell-silently-savep t))

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
        org-image-actual-width 1000
        org-agenda-files '("~/src/notes/journal.org")
        org-capture-templates
	'(
	  ("j" "Journal Entry"
           entry (file+olp+datetree "~/src/notes/journal.org")
           "* %?"
           :empty-lines 1))))

;;
;; Third party packages
;;

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	custom-file
	(no-littering-expand-etc-file-name "custom.el")))

(use-package which-key
  :ensure t
  :defer 0
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch)))

(use-package org-appear
  :ensure t
  :hook org-mode)

(use-package org-download
  :ensure t
  :config
  (setq org-download-timestamp "")
  (setq-default org-download-image-dir "img"
                org-download-heading-lvl nil))
