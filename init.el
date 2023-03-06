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

(use-package emacs
  :init
  (setq ring-bell-function 'ignore
        use-short-answers t
        tab-always-indent 'complete
        read-extended-command-predicate #'command-completion-default-include-p)
  (when (and (eq system-type 'darwin) (display-graphic-p))
    (setq ns-alternate-modifier nil
          ns-command-modifier 'meta)))

(use-package simple
  :init
  (setq column-number-mode t
        save-interprogram-paste-before-kill t)
  (setq-default indent-tabs-mode nil)
  :bind ([remap zap-to-char] . zap-up-to-char)
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill)))

(use-package display-line-numbers
  :hook (conf-mode prod-mode))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

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

(use-package vertico
  :ensure t
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (setq marginalia-max-relative-age 0)
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  :init
  (setq corfu-cycle t)
  :config
  (global-corfu-mode))

(defun org-completion-at-point-functions ()
  (add-to-list 'completion-at-point-functions #'cape-ispell))

(use-package cape
  :ensure t
  :bind ([remap dabbrev-expand] . cape-dabbrev)
  :hook (org-mode . org-completion-at-point-functions))

(use-package which-key
  :ensure t
  :defer 0
  :config
  (which-key-mode))

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
  :init
  (setq org-download-timestamp "")
  (setq-default org-download-image-dir "img"
                org-download-heading-lvl nil))
