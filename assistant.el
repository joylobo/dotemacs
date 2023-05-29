(require 'json)

(defgroup assistant nil
  "An assistant for develop"
  :group 'external)

(defcustom data-refresh-interval 0.2
  "The interval time to refresh the data")

(defun http-get (url callback)
  "Send an asynchronous HTTP GET request to the specified URL and call the
   specified CALLBACK function with the response body."
  (url-retrieve url callback))

(defun refresh-baidu-news () "refresh"
       (interactive)
       (http-get "https://www.fengche.tech/api/v1/news/baidu"
		 (lambda (status)
		   (goto-char (point-min))
		   (re-search-forward "^$")
		   (let* ((json-object-type 'hash-table)
			  (response (json-read-from-string (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
			  (items (gethash "items" response)))

		     (with-output-to-temp-buffer "*assistant*"
		       (pop-to-buffer "*assistant*")
		       (setf (buffer-string) "")
		       (dolist (item (append items nil))
			 (let ((keyword (gethash "keyword" item))
			       (url (gethash "url" item))
			       (summary (gethash "summary" item)))
			   (insert-button keyword 'action (lambda (x) (browse-url (button-get x 'url))) 'url url)
			   (insert "\n")
			   (insert (string-trim summary))
			   (insert "\n\n"))
			 ))))))

(define-minor-mode assistant-mode "" :lighter " assistant")

(provide 'assistant)
