;; Minimal GUI:
(tool-bar-mode -1)
(scroll-bar-mode -1)
(modify-all-frames-parameters '((ns-apperance . light)
                                (ns-transparent-titlebar . t)
                                (width . 200)
                                (height . 60)))

;; Do not restrict resize to columns and lines:
(setopt frame-resize-pixelwise t)

;; No startup screen or echo message:
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)

;; No frame resizing when setting font:
(setopt frame-inhibit-implied-resize t)

;; Disable native compilation warnings:
(setopt native-comp-async-report-warnings-errors 'silent)

(setopt load-prefer-newer t)
