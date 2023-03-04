;; Setup package archive:
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)
                          ("nongnu" . 80)
                          ("melpa-stable" . 70)))
(unless package-archive-contents
  (package-refresh-contents))

;; Basic settings
(setq ring-bell-function 'ignore  ; No audible bell
      use-short-answers t) ; y/n in stead of yes/no

(require 'use-package)

(load-theme 'modus-operandi)

(use-package faces
  :config
  (set-face-attribute 'default nil :font "SF Mono" :height 140))

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	custom-file
	(no-littering-expand-etc-file-name "custom.el")))

(use-package simple
  :config
  ;; Indent with spaces:
  (setq-default indent-tabs-mode nil))

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

(use-package org
  :config
  (setq org-capture-templates
	'(
	  ("j" "Journal Entry"
           entry (file+olp+datetree "~/src/notes/journal.org")
           "* %?"
           :empty-lines 1))))
