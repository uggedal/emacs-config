;; Minimal GUI:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; No startup screen or echo message:
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; Disable native compilation warnings:
(setq native-comp-async-report-warnings-errors 'silent)

;; Move native compilation cache to no-littering dir:
(startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory)))
