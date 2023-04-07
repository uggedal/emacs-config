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
(setq package-archive-priorities '(("melpa" . 3)
                                   ("gnu" . 2)
                                   ("nongnu" . 1)))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;;;
;;; Utilities
;;;


(defun ensure-package (package)
  "Install PACKAGE if not already installed."
  (unless (package-installed-p package)
    (unless (memq package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun add-prog-and-conf-modes-hook (function)
  "Add FUNCTION as hook to prog and conf modes."
  (add-hook 'prog-mode-hook function)
  (add-hook 'conf-mode-hook function))

(defun add-editing-modes-hook (function)
  "Add FUNCTION as hook to prog, conf and text modes."
  (add-hook 'text-mode-hook function)
  (add-prog-and-conf-modes-hook function))


;;;
;;; Core
;;;

(setopt ring-bell-function 'ignore
        use-short-answers t
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


(add-editing-modes-hook (lambda () (setq-local show-trailing-whitespace t
                                               indicate-empty-lines t)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't need confirmation/selection when killing buffers:
(keymap-global-set "C-x k" 'kill-this-buffer)

(require 'server)
(unless (server-running-p)
  (server-start))

;;;
;;; Appearance
;;;

(ensure-package 'modus-themes)
(require 'modus-themes)

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
    (let ((padding 6))
      (custom-set-faces
       ;; Padding for mode line:
       `(mode-line
         ((,c :box (:line-width ,padding :color ,bg-mode-line-active))))
       `(mode-line-inactive
         ((,c :box (:line-width ,padding :color ,bg-mode-line-inactive))))))
    (custom-set-faces
     `(org-todo ((,c :foreground ,red :background ,bg-red-nuanced)))
     ;; Dim DONE Org headlines:
     `(org-done ((,c :foreground ,fg-dim)))
     ;; Bring back dashed fill column indicator (relative height and no bg):
     `(fill-column-indicator ((,c :height 1.0 :background ,nil
                                  :foreground ,bg-active)))
     ;; Less invasive Eglot highlights:
     `(eglot-highlight-symbol-face ((,c :background , bg-green-nuanced))))))

(add-hook 'modus-themes-after-load-theme-hook #'modus-themes-custom-faces)

(modus-themes-load-theme 'modus-operandi)

(with-eval-after-load 'faces
  (set-face-attribute 'default nil :font "SF Mono" :height 120))

(add-prog-and-conf-modes-hook 'display-line-numbers-mode)
(add-prog-and-conf-modes-hook 'display-fill-column-indicator-mode)

(ensure-package 'diminish)
(require 'diminish)

(setopt whitespace-line-column 79
        whitespace-style '(face tabs lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'whitespace-mode-hook (lambda () (diminish 'whitespace-mode)))

(setopt pixel-scroll-precision-large-scroll-height 35.0)
(pixel-scroll-mode)
(pixel-scroll-precision-mode 1)

(setopt uniquify-buffer-name-style 'forward)

;;;
;;; Editing
;;;

(setopt column-number-mode t
        save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t
        indent-tabs-mode nil)

(keymap-global-set "M-=" 'count-words)
(keymap-global-set "M-z" 'zap-up-to-char)
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)

(keymap-global-set "M-;" 'comment-line)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(diminish 'auto-fill-function)

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
                                        log-edit-comment-ring
                                        corfu-history))
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

(setopt goto-address-uri-schemes '("http://" "https://"))
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook 'goto-address-mode))
(add-prog-and-conf-modes-hook 'goto-address-prog-mode)

;;;
;;; Completion
;;;

(ensure-package 'vertico)
(setopt vertico-cycle t)
(when (fboundp 'vertico-mode)
  (vertico-mode))

(with-eval-after-load 'vertico
  (eval-when-compile (require 'vertico))
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
(when (fboundp 'marginalia-mode)
  (marginalia-mode))

(ensure-package 'corfu)
(setopt corfu-cycle t
        corfu-echo-delay 0.1
        corfu-popupinfo-delay nil)

(with-eval-after-load 'corfu
  (eval-when-compile (require 'corfu))

  (defun corfu-enable-in-minibuffer ()
    "Enable for M-: and M-!"
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil)
      (when (fboundp 'corfu-mode)
        (corfu-mode))))

  (keymap-set corfu-map "SPC" 'corfu-insert-separator)

  (add-hook 'minibuffer-setup-hook 'corfu-enable-in-minibuffer)
  (add-hook 'corfu-mode-hook 'corfu-echo-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  (add-hook 'corfu-mode-hook 'corfu-history-mode))

(when (fboundp 'global-corfu-mode)
  (global-corfu-mode))

(ensure-package 'cape)
(keymap-global-set "M-/" 'cape-dabbrev)

(ensure-package 'consult)
(setopt xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
(keymap-global-set "M-y" 'consult-yank-pop)
(keymap-global-set "<remap> <goto-line>" 'consult-goto-line)
(keymap-unset goto-map "M-g") ;; Remove double binding
(keymap-set goto-map "f" 'consult-flymake)
(keymap-set goto-map "o" 'consult-outline)
(with-eval-after-load 'org
  (eval-when-compile (require 'org))
  (keymap-set org-mode-map "M-g o" 'consult-org-heading))
(keymap-set search-map "g" 'consult-ripgrep)


;;;
;;; Shell
;;;

(ensure-package 'vterm)
(with-eval-after-load 'vterm
  (eval-when-compile (require 'vterm))
  (keymap-set vterm-mode-map "M-`" nil)
  (keymap-set vterm-mode-map "C-q" 'vterm-send-next-key))

(ensure-package 'multi-vterm)
(keymap-set project-prefix-map "s" 'multi-vterm-project)

(ensure-package 'with-editor)
(add-hook 'vterm-mode-hook 'with-editor-export-editor)

;;;
;;; VCS
;;;

(setopt project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir")
                                  (multi-vterm-project "Shell")))

(setopt vc-handled-backends '(Git)
        log-edit-maximum-comment-ring-size 1000)

(ensure-package 'git-commit)

(autoload 'ring-elements "ring")

(ensure-package 'dash)

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

(with-eval-after-load 'git-commit
  (eval-when-compile (require 'git-commit))
  (keymap-set git-commit-mode-map "M-r" 'commit-message-completion))

(ensure-package 'magit)

(setopt magit-repository-directories `(("~/src" . 1)))
(with-eval-after-load 'magit-repos
  (setopt magit-repolist-columns
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

(with-eval-after-load 'magit-status
  (eval-when-compile (require 'magit-status))
  ;; Conflicts with MacOS like window switching:
  (keymap-set magit-status-mode-map "C-<tab>" nil))

(keymap-global-set "C-x g" 'magit-status)

(autoload 'magit-unstaged-files "magit-git")
(autoload 'vc-git-command "vc-git")
(autoload 'diff-hl-update "diff-hl")

(defun automatic-commit-and-push ()
  "Automatically commit and push."
  (interactive)
  (require 'vc-git)
  (require 'diff-hl)
  (require 'magit-git)

  (if (not (buffer-file-name))
      (error "Non-file buffer!"))
  (cond ((= 0 (length (magit-unstaged-files)))
         (message "No changes to commit"))
        (t
         (vc-git-command nil 0 nil "commit" "-am" "sync")
         (vc-git-command nil 'async nil "push")
         (diff-hl-update)
         (message "Committed and pushed")
         t)))
(keymap-global-set "C-x v p" 'automatic-commit-and-push)

(setopt diff-font-lock-prettify t)

(ensure-package 'diff-hl)
(add-editing-modes-hook 'turn-on-diff-hl-mode)
(with-eval-after-load 'vc-dir
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(with-eval-after-load 'magit
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

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

(add-hook 'eldoc-mode-hook (lambda () (diminish 'eldoc-mode)))

(ensure-package 'reformatter)

(reformatter-define shfmt :program "shfmt")
(add-hook 'sh-base-mode-hook 'shfmt-on-save-mode)

;;;
;;; Writing
;;;

(ensure-package 'jinx)

(setopt jinx-languages '("en_US" "nb_NO"))
(add-editing-modes-hook 'jinx-mode)

(with-eval-after-load 'jinx
  (eval-when-compile (require 'jinx))

  ;; Disable for strings:
  (setopt jinx-include-faces '((prog-mode font-lock-comment-face
                                          font-lock-doc-face)
                               (conf-mode font-lock-comment-face)))

  (keymap-set jinx-mode-map "C-;" 'jinx-correct))

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

(setopt org-agenda-span 14)
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

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))

(ensure-package 'org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)

;;;
;;; Programming modes
;;;

;; Setup MacPorts provided tree-sitter parsers:
(defun ts-remap (regex-mode ts-mode)
  "Remap from REGEX-MODE to TS-MODE."
  (add-to-list 'major-mode-remap-alist `(,regex-mode . ,ts-mode)))

(ts-remap 'conf-toml-mode 'toml-ts-mode)
(ts-remap 'python-mode 'python-ts-mode)
(ts-remap 'sh-mode 'bash-ts-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(ensure-package 'hl-todo)
(add-prog-and-conf-modes-hook 'hl-todo-mode)

(defun enable-indent-tabs-mode ()
  "Enable tab indent."
  (setq-local indent-tabs-mode t))

(setopt sh-basic-offset 8)
(add-hook 'sh-mode-hook 'enable-indent-tabs-mode)

(ensure-package 'markdown-mode)
(setopt markdown-fontify-code-blocks-natively t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(ensure-package 'terraform-mode)

(ensure-package 'systemd)

;;;
;;; Web
;;;

(ensure-package 'rainbow-mode)
(setopt rainbow-html-colors nil
        rainbow-x-colors nil)

;;;
;;; Misc
;;;

(setopt help-window-select t)

(ensure-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(add-hook 'which-key-mode-hook (lambda () (diminish 'which-key-mode)))

;; Make flymake know about our load path (ELPA):
(setq elisp-flymake-byte-compile-load-path load-path)

(provide 'init)
;;; init.el ends here
