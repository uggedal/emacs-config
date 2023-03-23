;;; init.el --- Main configuration
;;; Commentary:

;; Minimal Emacs configuration. Strive to use built-ins where possible.

;;; Code:

;;;
;;; Bootstrap
;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa" . 70)))

;;;
;;; Appearance
;;;

(use-package faces
  :custom-face (default ((t (:font "SF Mono" :height 130)))))

(use-package nimbus-theme
  :ensure t
  :custom-face
  ;; Remove link underline:
  (link ((t (:underline nil))))

  ;; Mode line padding:
  (mode-line ((t (:box (:line-width 6 :color "#2b2b47")))))
  (mode-line-inactive ((t (:box (:line-width 6 :color "#2b2b2b")))))
  :config (load-theme 'nimbus t))

(use-package display-line-numbers
  :hook (conf-mode prog-mode))

(use-package display-fill-column-indicator
  :hook (conf-mode prog-mode))

(use-package whitespace
  :custom
  (whitespace-line-column 79 "Highlight lines above this column")
  (whitespace-style '(face tabs lines-tail) "Show tabs and tailing whitespace")
  :hook (conf-mode prog-mode))

(use-package pixel-scroll
  :custom (pixel-scroll-precision-large-scroll-height 35.0)
  :config
  (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1))

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward "a/f.txt and b/f.txt"))

;;;
;;; Core
;;;

(use-package emacs
  :custom
  (ring-bell-function 'ignore)
  (use-short-answers t)

  (create-lockfiles nil)

  (read-extended-command-predicate
   #'command-completion-default-include-p
   "Hide commands in M-x not applicable to active mode")
  (enable-recursive-minibuffers
   t
   "Allow minibuffer commands while in minibuffer")

  ;; Disable right-to-left text:
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)

  (tab-always-indent 'complete "TAB first indents then completes")
  (x-stretch-cursor t "Make cursor fill entire tab width")
  (sentence-end-double-space nil)

  (scroll-conservatively 101 "Make point follow scrolling")
  (scroll-margin 3 "Always show 3 lines above/below when scrolling")

  (fill-column 79 "Line wrap automatically beyond this column")
  (display-fill-column-indicator-character ?\u254e "Dashed indicator line")
  (truncate-lines t "Do not display line continuation lines")

  (echo-keystrokes 1e-6 "Show unfinished keystrokes immediately")

  (initial-scratch-message nil)

  (kill-buffer-query-functions nil "Don't ask to kill buffers with processes")

  (history-delete-duplicates t)
  (history-length 1000)

  (ns-alternate-modifier nil)
  (ns-command-modifier 'meta)

  :hook ((prog-mode conf-mode text-mode) .
         (lambda () (setq-local show-trailing-whitespace t
                                indicate-empty-lines t)))
  :config
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil))

(use-package menu-bar
  ;; Don't use selection when killing buffer:
  :bind ([remap kill-buffer] . kill-this-buffer))

(use-package server
  :config (server-start))

;;;
;;; Editing
;;;

(use-package simple
  :custom
  (column-number-mode t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (indent-tabs-mode nil)
  :bind (([remap zap-to-char] . zap-up-to-char)
         ([remap upcase-word] . upcase-dwim)
         ([remap downcase-word] . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . turn-on-auto-fill)))

(use-package subword
  :config (subword-mode))

(use-package move-text
  :ensure t
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;;;
;;; Files
;;;

(use-package files
  :preface (setq auto-save-dir (concat user-emacs-directory "auto-save/"))
  :custom
  (make-backup-files nil)
  (require-final-newline t)
  (auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  :config (make-directory (expand-file-name auto-save-dir) t))

(use-package cus-edit
  :custom (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config (when (file-exists-p custom-file)
            (load custom-file)))

(use-package so-long
  :config (global-so-long-mode))

(use-package autorevert
  :custom (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode))

;;;
;;; History
;;;

(use-package recentf
  :custom (recentf-max-saved-items 1000)
  :bind ([remap find-file-read-only] . recentf-open)
  :config (recentf-mode))

(use-package savehist
  :custom (savehist-additional-variables '(search-ring
                                           regexp-search-ring
                                           last-kbd-macro
                                           shell-command-history
                                           log-edit-comment-ring))
  :config (savehist-mode 1))

(use-package saveplace
  :config (save-place-mode 1))

(use-package desktop
  :config (desktop-save-mode))

;;;
;;; Navigation and Search
;;;

(use-package frame
  :config
  (blink-cursor-mode 0)
  ;; Disable suspend frame:
  (keymap-global-unset "C-z")
  :bind (("C-<tab>" . next-multiframe-window)
         ("C-S-<tab>" . previous-multiframe-window)))

(use-package ns-win
  :bind (("M-`" . ns-next-frame)
         ("M-~" . ns-prev-frame)))

(use-package isearch
  :custom (isearch-lazy-count t)
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char)))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer-list-buffers))

(use-package dired
  :custom (dired-listing-switches "-alFh \"-D%Y-%m-%d %H:%M\"")
  :bind (:map dired-mode-map
         ("RET" . dired-find-alternate-file)
         ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :config (put 'dired-find-alternate-file 'disabled nil))

(use-package goto-addr
  :custom (goto-address-uri-schemes '("http://" "https://"))
  :hook ((vterm-mode . goto-address-mode)
         ((prog-mode conf-mode) . goto-address-prog-mode)))

;;;
;;; Completion
;;;

(use-package vertico
  :ensure t
  :functions vertico-mode
  :custom (vertico-cycle t)
  :config (vertico-mode))

(use-package vertico-directory
  :after vertico
  :defines vertico-map
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :functions marginalia-mode
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode)
  :config (advice-add 'marginalia--time-absolute
                      :override
                      (lambda (time)
                        (let ((system-time-locale "C"))
                          (format-time-string "%Y-%m-%d %H:%M" time)))))

(use-package corfu
  :ensure t
  :defines corfu-map
  :functions (corfu-mode global-corfu-mode)
  :preface (defun corfu-enable-in-minibuffer ()
             "Enable for M-: and M-!"
             (when (where-is-internal #'completion-at-point
                                      (list (current-local-map)))
               (setq-local corfu-echo-delay nil)
               (corfu-mode)))
  :custom (corfu-cycle t)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
  :hook (minibuffer-setup . corfu-enable-in-minibuffer)
  :init (global-corfu-mode))

(use-package corfu-echo
  :after corfu
  :functions corfu-echo-mode
  :custom (corfu-echo-delay t)
  :config (corfu-echo-mode))

(use-package corfu-popupinfo
  :after corfu
  :functions corfu-popupinfo-mode
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

(use-package with-editor
  :ensure t
  :hook (vterm-mode . with-editor-export-editor))

;;;
;;; VCS
;;;

(use-package vc-hooks
  :custom (vc-handled-backends '(Git) "Only enable git backend"))

(use-package log-edit
  :custom
  (log-edit-comment-ring (make-ring 1000)))

(use-package git-commit
  :ensure t
  :defiens git-commit-mode-map
  :preface (defun commit-message-completion ()
             (interactive)
             (require 'dash)
             (insert (completing-read
                      "History: "
                      (delete-dups
                       ;; Remove unnecessary newlines (at beg and end).
                       (mapcar (lambda (text) (string-trim text))
                               (ring-elements log-edit-comment-ring))))))
  :bind (:map git-commit-mode-map
              ("M-r" . commit-message-completion)))

(use-package diff-mode
  :defer
  :custom (diff-font-lock-prettify t))

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
             (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
                 ;; Places hook before (-10) Eglot's willSave notification,
                 ;; so that that notification reports the actual contents that
                 ;; will be saved:
                 (add-hook 'before-save-hook 'eglot-format-buffer -10 t)
               (remove-hook 'before-save-hook 'eglot-format-buffer)))
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
  (if (fboundp 'ispell-set-spellchecker-params)
      (ispell-set-spellchecker-params))
  (if (fboundp 'ispell-hunspell-add-multi-dic)
      (ispell-hunspell-add-multi-dic "en_US,nb_NO")))

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
  :config (which-key-mode))

(provide 'init)
;;; init.el ends here
