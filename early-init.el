;; Minimal GUI:
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Do not restrict resize to columns and lines:
(setopt frame-resize-pixelwise t)

;; No startup screen or echo message:
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
