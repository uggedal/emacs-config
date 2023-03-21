;; Minimal GUI:
(custom-set-variables '(tool-bar-mode . nil)
                      '(scroll-bar-mode nil))

(modify-all-frames-parameters '((ns-appearance . dark)
                                  (ns-transparent-titlebar . t)))

;; No startup screen or echo message:
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; Disable native compilation warnings:
(setq native-comp-async-report-warnings-errors 'silent)
