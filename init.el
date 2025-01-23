;;; init.el --- My emacs config for c/c++ javascript and golang.

(setq user-full-name "Joy Lobo")
(setq user-mail-address "joylobo0528@gmail.com")

(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(require 'startup)
(require 'init-gui)
(require 'init-basic)
(require 'init-devtools)
(require 'init-org)

(provide 'init)
