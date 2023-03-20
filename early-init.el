;; Minimal GUI:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; No startup screen or echo message:
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; Disable native compilation warnings:
(setq native-comp-async-report-warnings-errors 'silent)
