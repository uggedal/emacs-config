;; Minimal GUI:
(custom-set-variables '(tool-bar-mode . nil)
                      '(scroll-bar-mode nil))

(modify-all-frames-parameters '((ns-apperance . light)
                                (ns-transparent-titlebar . t)
                                (width . 80)
                                (height . 40)))

;; Do not restrict resize to columns and lines:
(setq frame-resize-pixelwise t)

;; No startup screen or echo message:
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)

;; Disable native compilation warnings:
(setq native-comp-async-report-warnings-errors 'silent)

;; Extend PATH:
(let ((local_bin (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat local_bin ":" (getenv "PATH")))
  (setq exec-path (push local_bin exec-path)))

;; Extend MANPATH:
(setenv "MANPATH" (concat "/opt/local/share/man:" (getenv "MANPATH")))
