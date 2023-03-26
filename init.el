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
(unless (bound-and-true-p package--initialized)
  (package-initialize))
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
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode 'goto-address-prog-mode))

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
        corfu-echo-delay t
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
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode))

(when (fboundp 'global-corfu-mode)
  (global-corfu-mode))

(ensure-package 'cape)
(keymap-global-set "M-/" 'cape-dabbrev)

(ensure-package 'consult)
(setopt xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
(keymap-global-set "M-y" 'consult-yank-from-kill-ring)
(keymap-global-set "<remap> <goto-line>" 'consult-goto-line)

;;;
;;; Shell
;;;

(ensure-package 'vterm)
(with-eval-after-load 'vterm
  (eval-when-compile (require 'vterm))
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
        log-edit-maximum-comment-ring-size 1000)

(ensure-package 'git-commit)

(autoload 'ring-elements "ring")

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

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

;;;
;;; IDE
;;;

(defun toggle-eglot-format-hook ()
  "Run Eglot format on save and disable when Eglot is tuned off."
  (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
      ;; Places hook before (-10) Eglot's willSave notification,
      ;; so that that notification reports the actual contents that
      ;; will be saved:
      (add-hook 'before-save-hook 'eglot-format-buffer -10 t)
    (remove-hook 'before-save-hook 'eglot-format-buffer)))

(setopt eglot-autoshutdown t)

(with-eval-after-load 'eglot
  (eval-when-compile (require 'eglot))
  (keymap-set eglot-mode-map "C-c l f" 'eglot-format)
  (keymap-set eglot-mode-map "C-c l r" 'eglot-rename)
  (keymap-set eglot-mode-map "C-c l c" 'eglot-code-actions))

(add-hook 'python-base-mode-hook 'eglot-ensure)
(add-hook 'eglot-managed-mode-hook 'toggle-eglot-format-hook)

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" 'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" 'flymake-goto-prev-error))

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; Single line doc string:
(setopt eldoc-echo-area-use-multiline-p nil)

;;;
;;; Writing
;;;

(setenv "LANG" "en_US.UTF-8")
(setopt ispell-program-name "hunspell"
  ispell-silently-savep t
  ispell-dictionary "en_US,nb_NO")

(with-eval-after-load 'ispell
  (if (fboundp 'ispell-set-spellchecker-params)
      (ispell-set-spellchecker-params))
  (if (fboundp 'ispell-hunspell-add-multi-dic)
      (ispell-hunspell-add-multi-dic "en_US,nb_NO")))

(add-hook 'text-mode-hook 'flyspell-mode)
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(ensure-package 'flyspell-correct)
(with-eval-after-load 'flyspell
  (eval-when-compile (require 'flyspell))
  (keymap-set flyspell-mode-map "C-;" 'flyspell-correct-wrapper))

(setopt calendar-week-start-day 1)

(setopt org-hide-emphasis-markers t
        org-startup-indented t
        org-startup-folded t
        org-blank-before-new-entry '((heading . t)
                                    (plain-list-item . nil))
        org-todo-keywords '((sequence "TODO" "NEXT" "DOING" "|" "DONE"))
        org-startup-with-inline-images t
        org-ellipsis " …"
        org-image-actual-width nil
        org-special-ctrl-a/e t
        org-directory "~/src/notes"
        org-agenda-files '("work.org" "personal.org" "tech.org"))
(with-eval-after-load 'org
  (eval-when-compile (require 'org))
  (keymap-set org-mode-map "C-M-<up>" 'org-up-element))

(setopt org-todo-keyword-faces '(("NEXT" . '(org-level-3 org-todo))
                                 ("DOING" . '(org-level-4 org-todo))))

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

(ensure-package 'org-appear)
(add-hook 'org-mode-hook 'org-appear-mode)

;;;
;;; Programming modes
;;;

(defun enable-indent-tabs-mode ()
  "Enable tab indent."
  (setq-local indent-tabs-mode t))

(setopt sh-basic-offset 8)
(add-hook 'sh-mode-hook 'enable-indent-tabs-mode)

(ensure-package 'markdown-mode)
(setopt markdown-fontify-code-blocks-natively t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(ensure-package 'terraform-mode)

;;;
;;; Misc
;;;

(setopt help-window-select t)

(with-eval-after-load 'info
  (defvar Info-directory-list)
  (add-to-list 'Info-directory-list "/opt/homebrew/share/info"))

(ensure-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)

;; Make flymake know about our load path (ELPA):
(setq elisp-flymake-byte-compile-load-path load-path)

(provide 'init)
;;; init.el ends here
