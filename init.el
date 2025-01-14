;;; init.el --- My emacs config for c/c++ javascript and golang.

;;; Commentary:

;;; Code:
(setq user-full-name "Joy Lobo")
(setq user-mail-address "joylobo0528@gmail.com")

(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(require 'init-basic)
(require 'init-gui)
(require 'init-devtools)
(require 'init-org)

(provide 'init)
