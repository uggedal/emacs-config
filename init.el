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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setopt package-archive-priorities '(("gnu" . 3)
                                     ("nongnu" . 2)
                                     ("melpa" . 1)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))


;;;
;;; Core
;;;

(setopt ring-bell-function 'ignore
        use-short-answers t
        y-or-n-p-use-read-key t
        use-dialog-box nil
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

(use-package emacs
  :hook ((text-mode prog-mode conf-mode) .
         (lambda () (setq-local show-trailing-whitespace t
                                indicate-empty-lines t))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package simple
  ;; Don't need confirmation/selection when killing buffers:
  :bind ("C-x k" . kill-current-buffer))

(use-package server
  :hook (after-init . server-start))

;;;
;;; Appearance
;;;

(require-theme 'modus-themes)

(setopt modus-themes-common-palette-overrides
        '(
          ;; Subtle line numbers:
          (fg-line-number-active fg-main)
          (fg-line-number-inactive "gray50")
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)

          ;; Disable link underline:
          (underline-link unspecified)
          (underline-link-visited unspecified)
          (underline-link-symbolic unspecified)

          ;; Subtle fringe:
          (fringe unspecified)

          ;; Border-less mode line:
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; Some color for region background:
          (bg-region bg-yellow-nuanced)

          ;; Some color for active mode line:
          (bg-mode-line-active bg-blue-subtle)

          ;; Normalize heading colors:
          (fg-heading-1 fg-alt)
          (fg-heading-2 fg-alt)
          (fg-heading-3 fg-alt)
          (fg-heading-4 fg-alt)
          (fg-heading-5 fg-alt)
          (fg-heading-6 fg-alt)
          (fg-heading-7 fg-alt)
          (fg-heading-8 fg-alt)

          ;; Lighter fringe diff highlights:
          (bg-added-fringe bg-added)
          (bg-changed-fringe bg-changed)
          (bg-removed-fringe bg-removed)
          ))

(defun modus-themes-custom-faces ()
  "Custom overrides of Modus Operandi theme."
  (modus-themes-with-colors
    (let ((mode-line-padding 6)
          (tab-bar-padding 4))
      (custom-set-faces
       ;; Padding for mode line and tab-bar:
       `(mode-line
         ((,c :box
              (:line-width ,mode-line-padding :color ,bg-mode-line-active))))
       `(mode-line-inactive
         ((,c :box
              (:line-width ,mode-line-padding :color ,bg-mode-line-inactive))))
       `(tab-bar ((,c :background ,bg-tab-current)))
       `(tab-bar-tab
         ((,c :box (:line-width ,tab-bar-padding :color ,bg-yellow-nuanced)
              :background ,bg-yellow-nuanced)))
       `(tab-bar-tab-inactive
         ((,c :box (:line-width ,tab-bar-padding :color ,bg-dim)
              :background ,bg-dim)))
       ))
    (custom-set-faces
     `(org-todo ((,c :foreground ,red :background ,bg-red-nuanced)))
     ;; Dim DONE Org headlines:
     `(org-done ((,c :foreground ,fg-dim)))
     ;; Bring back dashed fill column indicator (relative height and no bg):
     `(fill-column-indicator ((,c :height 1.0 :background ,bg-main
                                  :foreground ,bg-active)))
     ;; Less invasive Eglot highlights:
     `(eglot-highlight-symbol-face ((,c :background , bg-green-nuanced))))))

(add-hook 'modus-themes-after-load-theme-hook #'modus-themes-custom-faces)

(load-theme 'modus-operandi :noconfirm)

(with-eval-after-load 'faces
  (set-face-attribute 'default nil :font "SF Mono" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "SF Mono")
  (set-face-attribute 'variable-pitch nil :font "New York"))

(use-package display-line-numbers
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package display-fill-column-indicator
  :hook ((prog-mode conf-mode) . display-fill-column-indicator-mode))

(use-package diminish
  :ensure t)

(use-package whitespace
  :diminish
  :init (setopt whitespace-line-column 79
                whitespace-style '(face tabs lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package pixel-scroll
  :init
  (setopt pixel-scroll-precision-large-scroll-height 35.0)
  :hook ((after-init . pixel-scroll-mode)
         (after-init . pixel-scroll-precision-mode)))

(setopt uniquify-buffer-name-style 'forward)

;;;
;;; Editing
;;;

(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "M-;" 'comment-line)

(use-package simple
  :init (setopt column-number-mode t
                save-interprogram-paste-before-kill t
                kill-do-not-save-duplicates t
                indent-tabs-mode nil)
  :hook ((text-mode . turn-on-auto-fill)
	 (before-save . delete-trailing-whitespace))
  :bind (("M-=" . count-words)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim)))

(use-package emacs
  :diminish auto-fill-function)

(use-package subword
  :hook (after-init subword-mode))

(use-package move-text
  :ensure t
  :bind (:map prog-mode-map
              ("M-<up>" . move-text-up)
              ("M-<down>" . move-text-down)))

;;;
;;; Files
;;;


(setopt make-backup-files nil
        require-final-newline t)

(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
  (setopt auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (make-directory (expand-file-name auto-save-dir) t))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package autorevert
  :init (setopt global-auto-revert-non-file-buffers t)
  :hook (after-init . global-auto-revert-mode))

;;;
;;; History
;;;

(use-package recentf
  :init (setopt recentf-max-saved-items 1000)
  :hook (after-init recentf-mode)
  :bind ("C-x C-r" . recentf-open))

(use-package savehist
  :init (setopt savehist-additional-variables '(search-ring
                                                regexp-search-ring
                                                last-kbd-macro
                                                shell-command-history
                                                log-edit-comment-ring
                                                corfu-history))
  :hook (after-init . savehist-mode))

(use-package saveplace
  :hook (after-init . save-place-mode))

(desktop-save-mode)

;;;
;;; Navigation and Search
;;;

(setopt blink-cursor-blinks 0)

;; Disable suspend frame:
(keymap-global-unset "C-z")

(keymap-global-set "M-o" 'other-window)

;; MacOS like tab and frame switching:
(keymap-global-set "C-<tab>" 'tab-next)
(keymap-global-set "C-S-<tab>" 'tab-previous)
(keymap-global-set "M-`" 'ns-next-frame)
(keymap-global-set "M-~" 'ns-prev-frame)

(setopt isearch-lazy-count t
        isearch-wrap-pause 'no)
;; Sane isearch query editing:
(keymap-set isearch-mode-map "DEL" 'isearch-del-char)

(setopt ibuffer-old-time 24)
(keymap-global-set "C-x C-b" 'ibuffer-list-buffers)

(with-eval-after-load 'dired
  (eval-when-compile (require 'dired))
  (keymap-set dired-mode-map "RET" 'dired-find-alternate-file)
  (keymap-set dired-mode-map "^" (lambda ()
                                   (interactive)
                                   (find-alternate-file "..")))
  (setopt dired-listing-switches "-alFh \"-D%Y-%m-%d %H:%M\"")
  (put 'dired-find-alternate-file 'disabled nil))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setopt goto-address-uri-schemes '("http://" "https://")
        goto-address-mail-regexp "XXXXXXXXXXXXXXXX")
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook 'goto-address-mode))

(use-package goto-addr
  :hook ((prog-mode conf-mode) . goto-address-prog-mode))

;;;
;;; Completion
;;;

(use-package vertico
  :ensure t
  :init (setopt vertico-cycle t)
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :init
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init (setopt marginalia-max-relative-age 0)
  :hook (after-init . marginalia-mode)
  :config
  (advice-add 'marginalia--time-absolute
              :override
              (lambda (time)
                (let ((system-time-locale "C"))
                  (format-time-string "%Y-%m-%d %H:%M" time)))))

(use-package corfu
  :ensure t
  :init (setopt corfu-cycle t
                corfu-echo-delay 0.1
                corfu-popupinfo-delay nil)
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
  :hook ((minibuffer-setup . corfu-enable-in-minibuffer)
         (corfu-mode . corfu-echo-mode)
         (corfu-mode . corfu-popupinfo-mode)
         (corfu-mode . corfu-history-mode)
         (after-init . global-corfu-mode))
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable for M-: and M-!"
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil)
      (when (fboundp 'corfu-mode)
        (corfu-mode)))))

(use-package cape
  :ensure t
  :bind ("M-/" . cape-dabbrev))

(use-package consult
  :ensure t
  :init
  (setopt xref-show-xrefs-function 'consult-xref
          xref-show-definitions-function 'consult-xref)
  :bind (("M-y" . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map goto-map
         ("f" . consult-flymake)
         ("o" . consult-outline)
         :map org-mode-map
         ("M-g o" . consult-org-heading)
         :map search-map
         ("g" . consult-ripgrep)))


;;;
;;; Shell
;;;

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("M-`" . nil)
              ("C-q" . vterm-send-next-key)))

(use-package multi-vterm
  :ensure t
  :bind (:map project-prefix-map
              ("s" . multi-vterm-project)))

(use-package with-editor
  :ensure t
  :hook (vterm-mode . with-editor-export-editor))

;;;
;;; VCS
;;;

(setopt project-prompter 'project-prompt-project-name
        project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir")
                                  (multi-vterm-project "Shell")))

(setopt vc-handled-backends '(Git)
        log-edit-maximum-comment-ring-size 1000)

(autoload 'ring-elements "ring")

(use-package dash
  :ensure t)

(defun commit-message-completion ()
  "Search for previous commit messages from history."
  (interactive)
  (eval-and-compile (require 'dash)
                    (require 'log-edit))


  (insert (completing-read "History: "
                           (-remove
                            (lambda (item)
                              (string-match-p (regexp-quote "Summary: ") item))
                            (delete-dups
                             ;; Remove unnecessary newlines:
                             (mapcar (lambda (text)
                                       (string-trim text))
                                     (ring-elements
                                      log-edit-comment-ring)))))))

(use-package magit
  :ensure t
  :pin melpa
  :init (setopt magit-repository-directories `(("~/src" . 1))
                magit-clone-default-directory "~/src/")
  :bind (("C-x g" . magit-status)
         :map magit-status-mode-map
         ;; Conflicts with MacOS like window switching:
         ("C-<tab>" . nil)))

(use-package magit-repos
  :config (setopt magit-repolist-columns
                '(("Name" 25 magit-repolist-column-ident nil)
                  ("Version" 35 magit-repolist-column-version nil)
                  ("Flag" 4 magit-repolist-column-flag
                   ((:right-align t)
                    (:help-echo "U[N]tracked [U]nstaged [S]taged")))
                  ("⇣" 3 magit-repolist-column-unpulled-from-upstream
                   ((:right-align t)
                    (:help-echo "Upstream changes not in branch")))
                  ("⇡" 3 magit-repolist-column-unpushed-to-upstream
                   ((:right-align t)
                    (:help-echo "Local changes not in upstream")))
                  ("Path" 99 magit-repolist-column-path nil))))

(autoload 'magit-call-git "magit-process")
(autoload 'magit-run-git-async "magit-process")
(autoload 'magit-refresh "magit-mode")

(defun automatic-commit-and-push ()
  "Automatically commit and push."
  (interactive)

  (magit-call-git "commit" "-am" "sync")
  (magit-run-git-async "push")
  (magit-refresh))

(keymap-global-set "C-x v p" 'automatic-commit-and-push)

(with-eval-after-load 'git-commit
  (eval-when-compile (require 'git-commit))
  (keymap-set git-commit-mode-map "M-r" 'commit-message-completion))


(setopt diff-font-lock-prettify t)


(use-package diff-hl
  :ensure t
  :hook (((text-mode prog-mode conf-mode) . turn-on-diff-hl-mode)
         (vc-dir-mode . turn-on-diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)))

;;;
;;; IDE
;;;

(setopt xref-search-program 'ripgrep)

(defun toggle-eglot-format-hook ()
  "Run Eglot format on save and disable when Eglot is tuned off."
  (if (and (fboundp 'eglot-managed-p)
           (eglot-managed-p)
           (fboundp 'eglot--server-capable)
           (eglot--server-capable :documentFormattingProvider))
      ;; Places hook before (-10) Eglot's willSave notification,
      ;; so that that notification reports the actual contents that
      ;; will be saved:
      (add-hook 'before-save-hook 'eglot-format-buffer -10 t)
    (remove-hook 'before-save-hook 'eglot-format-buffer)))

(setopt eglot-autoshutdown t)

(with-eval-after-load 'eglot
  (eval-when-compile (require 'eglot))

  (keymap-set eglot-mode-map "C-c l r" 'eglot-rename)
  (keymap-set eglot-mode-map "C-c l c" 'eglot-code-actions))

(dolist (hook '(python-base-mode-hook sh-base-mode-hook))
  (add-hook hook 'eglot-ensure))
(add-hook 'eglot-managed-mode-hook 'toggle-eglot-format-hook)

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error))

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; Single line doc string:
(setopt eldoc-echo-area-use-multiline-p nil)

(use-package eldoc
  :diminish)

(use-package reformatter
  :ensure t
  :init
  (reformatter-define shfmt :program "shfmt")
  :hook (sh-base-mode . shfmt-on-save-mode))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

;;;
;;; Writing
;;;

(use-package jinx
  :ensure t
  :diminish
  :init
  (setopt jinx-languages "en_US nb_NO"
          jinx-include-faces '((prog-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (conf-mode font-lock-comment-face)))
  :hook ((text-mode prog-mode conf-mode) . jinx-mode)
  :bind (:map jinx-mode-map ("C-;" . jinx-correct)))

(setopt calendar-week-start-day 1
        calendar-date-style 'iso)

(setopt org-hide-emphasis-markers t
        org-startup-indented t
        org-startup-folded 'content
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-todo-keywords '((sequence "TODO" "NEXT" "DOING" "|" "DONE"))
        org-startup-with-inline-images t
        org-ellipsis " ▾"
        org-image-actual-width nil
        org-special-ctrl-a/e t
        org-deadline-warning-days 7
        org-directory "~/src/notes"
        org-agenda-files '("work.org" "personal.org" "tech.org"))
(with-eval-after-load 'org
  (eval-when-compile (require 'org))
  (keymap-set org-mode-map "C-M-<up>" 'org-up-element))

(setopt org-todo-keyword-faces '(("NEXT" .
                                  '(modus-themes-fg-green
                                    modus-themes-nuanced-green org-todo))
                                 ("DOING" .
                                  '(modus-themes-fg-yellow
                                    modus-themes-nuanced-yellow org-todo))))

(setopt org-cycle-separator-lines 1)

(setopt org-goto-interface 'outline-path-completion)

(setopt org-agenda-span 14
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)
(keymap-global-set "C-c a" 'org-agenda)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(setopt org-capture-templates
        '(
          ("j" "Journal Entry"
           entry (file+olp+datetree "~/src/notes/work.org")
           "* %?"
           :empty-lines 1)))

(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-c l" 'org-store-link)

(setopt org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil)

(use-package org-indent
  :diminish)

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

;;;
;;; Programming modes
;;;

;; Setup MacPorts provided tree-sitter parsers:
(defun ts-remap (regex-mode ts-mode)
  "Remap from REGEX-MODE to TS-MODE."
  (add-to-list 'major-mode-remap-alist `(,regex-mode . ,ts-mode)))

(ts-remap 'sh-mode 'bash-ts-mode)
(ts-remap 'csharp-mode 'csharp-ts-mode)
(ts-remap 'c++-mode 'c++-ts-mode)
(ts-remap 'css-mode 'css-ts-mode)
(ts-remap 'mhtml-mode 'html-ts-mode)
(ts-remap 'java-mode 'java-ts-mode)
(ts-remap 'js-mode 'js-ts-mode)
(ts-remap 'js-json-mode 'json-ts-mode)
(ts-remap 'python-mode 'python-ts-mode)
(ts-remap 'ruby-mode 'ruby-ts-mode)
(ts-remap 'conf-toml-mode 'toml-ts-mode)
(add-to-list 'auto-mode-alist
             '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
               . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(use-package hl-todo
  :ensure t
  :hook ((prog-mode conf-mode) . hl-todo-mode))

(defun enable-indent-tabs-mode ()
  "Enable tab indent."
  (setq-local indent-tabs-mode t))

(setopt sh-basic-offset 8)
(add-hook 'bash-ts-mode-hook 'enable-indent-tabs-mode)

(use-package markdown-mode
  :ensure t
  :init
  (setopt markdown-fontify-code-blocks-natively t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(use-package git-modes
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package systemd
  :ensure t)

;;;
;;; Web
;;;

(use-package rainbow-mode
  :ensure t
  :init (setopt rainbow-html-colors nil
                rainbow-x-colors nil))

;;;
;;; Help
;;;

(setopt help-window-select t
        what-cursor-show-names t)

(add-hook 'Info-mode-hook 'variable-pitch-mode)

(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode))

;; Make flymake know about our load path (ELPA):
(setq elisp-flymake-byte-compile-load-path load-path)

(provide 'init)
;;; init.el ends here
