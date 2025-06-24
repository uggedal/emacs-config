;; Minimal GUI:
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Nice initial window size:
(modify-all-frames-parameters '((width . 200)
				(height . 60)))

;; Do not restrict resize to columns and lines:
(setopt frame-resize-pixelwise t
	frame-inhibit-implied-resize t)

;; No startup screen or echo message:
(setopt inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
