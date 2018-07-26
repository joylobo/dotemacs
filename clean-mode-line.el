;;; Package --- Hiding and replacing modeline strings
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl))

(defvar mode-line-cleaner-alist
  `((company-mode . " c")
		(flycheck-mode . " f")
		(yas-minor-mode . " s")
    (which-key-mode . "")
    (eldoc-mode . "")
		(helm-mode . "")
    (abbrev-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (js2-mode . "js")
    (go-mode . "go")
    (emacs-lisp-mode . "el"))
  "Alist for `clean-mode-line'.")

(defun clean-mode-line ()
  "Hiding and replacing modeline strings."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
	do (let* ((mode (car cleaner))
		  (mode-str (cdr cleaner))
		  (old-mode-str (cdr (assq mode minor-mode-alist))))
	     (when old-mode-str
	       (setcar old-mode-str mode-str))
	     ;; major mode
	     (when (eq mode major-mode)
	       (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(provide 'clean-mode-line)
;;; clean-mode-line ends here
