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

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa" . 70)))

(defun ensure-package (package)
  "Install PACKAGE if not already installed."
  (unless (package-installed-p package)
    (unless (memq package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;;
;;; Appearance
;;;

(set-face-attribute 'default nil :font "SF Mono" :height 130)


(ensure-package 'nimbus-theme)
(load-theme 'nimbus t)
;; Remove link underline:
(set-face-attribute 'link nil :underline nil)
;; Mode line padding:
(set-face-attribute 'mode-line nil :box '(:line-width 6 :color "#2b2b47"))
(set-face-attribute 'mode-line-inactive nil :box
                    '(:line-width 6 :color "#2b2b2b"))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setopt whitespace-line-column 79
        whitespace-style '(face tabs lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(setopt pixel-scroll-precision-large-scroll-height 35.0)
(pixel-scroll-mode)
(pixel-scroll-precision-mode 1)

(setopt uniquify-buffer-name-style 'forward)

;;;
;;; Core
;;;

(setopt ring-bell-function 'ignore
        use-short-answers t
        create-lockfiles nil
        ;; Hide commands in M-x not applicable to active mode:
        read-extended-command-predicate
        #'command-completion-default-include-p
        enable-recursive-minibuffers t
        ;; Disable right-to-left text:
        bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t
        ;;TAB first indents then completes:
        tab-always-indent 'complete
        ;;Make cursor fill entire tab width:
        x-stretch-cursor t
        sentence-end-double-space nil
        ;;Make point follow scrolling:
        scroll-conservatively 101
        scroll-margin 3
        fill-column 79
        ;;Dashed indicator line"
        display-fill-column-indicator-character ?\u254e
        ;;Do not display line continuation lines:
        truncate-lines t
        ;;Show unfinished keystrokes immediately:
        echo-keystrokes 1e-6
        initial-scratch-message nil
        ;;Don't ask to kill buffers with processes:
        kill-buffer-query-functions nil
        history-delete-duplicates t
        history-length 1000
        ns-alternate-modifier nil
        ns-command-modifier 'meta)

(dolist (mode-hook '(prog-mode-hook text-mode-hook conf-mode-hook))
         (add-hook mode-hook
                   (lambda () (setq-local show-trailing-whitespace t
                                          indicate-empty-lines t))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't need confirmation/selection when killing buffers:
(keymap-global-set "C-x k" 'kill-this-buffer)

(require 'server)
(unless (server-running-p)
  (server-start))

;;;
;;; Editing
;;;

(setopt column-number-mode t
        save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t
        indent-tabs-mode nil)

(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(subword-mode)

(ensure-package 'move-text)
(with-eval-after-load 'prog-mode
  (keymap-set prog-mode-map "M-<up>" 'move-text-up)
  (keymap-set prog-mode-map "M-<down>" 'move-text-down))

;;;
;;; Files
;;;


(setopt make-backup-files nil
        require-final-newline t)

(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
  (setopt auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (make-directory (expand-file-name auto-save-dir) t))

(global-so-long-mode)

(setopt global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

;;;
;;; History
;;;

(setopt recentf-max-saved-items 1000)
(keymap-global-set "C-x C-r" 'recentf-open)
(recentf-mode)

(setopt savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        last-kbd-macro
                                        shell-command-history
                                        log-edit-comment-ring))
(savehist-mode)
(save-place-mode)
(desktop-save-mode)

;;;
;;; Navigation and Search
;;;

(setopt blink-cursor-mode 0)
;; Disable suspend frame:
(keymap-global-unset "C-z")

;; MacOS like window and frame switching:
(keymap-global-set "C-<tab>" 'next-multiframe-window)
(keymap-global-set "C-S-<tab>" 'previous-multiframe-window)
(keymap-global-set "M-`" 'ns-next-frame)
(keymap-global-set "M-~" 'ns-prev-frame)

(setopt isearch-lazy-count t)
;; Sane isearch query editing:
(keymap-set isearch-mode-map "DEL" 'isearch-del-char)

(keymap-global-set "C-x C-b" 'ibuffer-list-buffers)

(setopt dired-listing-switches "-alFh \"-D%Y-%m-%d %H:%M\"")
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "RET" 'dired-find-alternate-file)
  (keymap-set dired-mode-map "^" (lambda ()
                                   (interactive)
                                   (find-alternate-file "..")))
  (put 'dired-find-alternate-file 'disabled nil))

(setopt goto-address-uri-schemes '("http://" "https://"))
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook 'goto-address-mode))
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode 'goto-address-prog-mode))

;;;
;;; Completion
;;;

(ensure-package 'vertico)
(setopt vertico-cycle t)
(vertico-mode)

(with-eval-after-load 'vertico
  (keymap-set vertico-map "RET" 'vertico-directory-enter)
  (keymap-set vertico-map "DEL" 'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" 'vertico-directory-delete-word))

(ensure-package 'orderless)
(setopt completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

(ensure-package 'marginalia)
(setopt marginalia-max-relative-age 0)
(with-eval-after-load 'marginalia
  (advice-add 'marginalia--time-absolute
              :override
              (lambda (time)
                (let ((system-time-locale "C"))
                  (format-time-string "%Y-%m-%d %H:%M" time)))))
(marginalia-mode)

(ensure-package 'corfu)
(setopt corfu-cycle t
        corfu-echo-delay t
        corfu-popupinfo-delay nil)

(defun corfu-enable-in-minibuffer ()
  "Enable for M-: and M-!"
  (when (where-is-internal #'completion-at-point
                           (list (current-local-map)))
    (setq-local corfu-echo-delay nil)
    (corfu-mode)))

(with-eval-after-load 'corfu
  (keymap-set corfu-map "SPC" 'corfu-insert-separator))

(add-hook 'corfu-mode-hook 'corfu-echo-mode)
(add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
(add-hook 'minibuffer-setup-hook 'corfu-enable-in-minibuffer)

(global-corfu-mode)

(ensure-package 'cape)
(keymap-global-set "M-/" 'cape-dabbrev)

(ensure-package 'consult)
(setopt xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
(keymap-global-set "M-y" 'consult-yank-from-kill-ring)
(keymap-global-set "<remap> <goto-line>" 'consult-goto-line)

;;;
;;; Shell
;;;

(ensure-package 'vterm)
(with-eval-after-load 'vterm
  (keymap-set vterm-mode-map "M-`" nil)
  (keymap-set vterm-mode-map "C-q" 'vterm-send-next-key))

(ensure-package 'multi-vterm)
(keymap-global-set "C-x p s" 'multi-vterm-project)

(ensure-package 'with-editor)
(add-hook 'vterm-mode-hook 'with-editor-export-editor)

;;;
;;; VCS
;;;

(setopt vc-handled-backends '(Git)
        log-edit-comment-ring (make-ring 1000))

(ensure-package 'git-commit)

(defun commit-message-completion ()
  (interactive)
  (require 'dash)
  (insert (completing-read
           "History: "
           (delete-dups
            ;; Remove unnecessary newlines (at beg and end).
            (mapcar (lambda (text) (string-trim text))
                    (ring-elements log-edit-comment-ring))))))
(with-eval-after-load 'git-commit
  (keymap-set git-commit-mode-map "M-r" 'commit-message-completion))

(ensure-package 'magit)
(keymap-global-set "C-x g" 'magit-status)

(defun automatic-commit-and-push ()
  "Automatically commit and push."
  (interactive)
  (require 'vc-git)
  (require 'diff-hl)

  (if (not (buffer-file-name))
      (error "Non-file buffer!"))
  (cond ((= 0 (length (magit-unstaged-files)))
         (message "No changes to commit"))
        (t
         (vc-git-command nil 0 nil "commit" "-am" "sync")
         (vc-git-command nil 0 nil "push")
         (diff-hl-update)
         (message "Committed and pushed")
         t)))
(keymap-global-set "C-x v p" 'automatic-commit-and-push)

(setopt diff-font-lock-prettify t)

(ensure-package 'diff-hl)
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))
(with-eval-after-load 'vc-dir
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))
(add-hook 'text-mode-hook 'turn-on-diff-hl-mode)

(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(add-hook 'dired-mode-hook 'diff-hl-dired)

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
  (org-startup-folded t)
  (org-blank-before-new-entry (quote ((heading .t) (plain-list-item . nil))))
  (org-todo-keywords '((sequence "TODO" "NEXT" "DOING" "|" "DONE")))
  (org-startup-with-inline-images t)
  (org-ellipsis " …")
  (org-image-actual-width nil)
  (org-special-ctrl-a/e t)
  (org-directory "~/src/notes")
  (org-agenda-files '("work.org" "personal.org" "tech.org"))
  :bind (:map org-mode-map
              ("C-M-<up>" . org-up-element)))

(use-package org-faces
  :after org
  :custom (org-todo-keyword-faces '(("NEXT" . '(org-level-3 org-todo))
                                    ("DOING" . '(org-level-4 org-todo)))))

(use-package org-cycle
  :after org
  :custom (org-cycle-separator-lines 1))

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

(use-package ol
  :bind ("C-c l" . org-store-link))

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
  :custom (markdown-fontify-code-blocks-natively t "Highlight code blocks")
  :mode (("\\.md\\'" . gfm-mode))
  :commands (markdown-mode gfm-mode))

(use-package terraform-mode
  :ensure t)

;;;
;;; Misc
;;;

(use-package help
  :custom
  (help-window-select t))

(use-package which-key
  :ensure t
  :defer 0
  :functions which-key-mode
  :config (which-key-mode))

(provide 'init)
;;; init.el ends here
