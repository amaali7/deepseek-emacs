;;; deepseek-emacs.el --- DeepSeek integration for Emacs -*- lexical-binding: t; -*-

(defgroup deepseek-emacs nil
  "DeepSeek integration for Emacs."
  :group 'tools)

(defcustom deepseek-api-key nil
  "API key for DeepSeek."
  :type 'string
  :group 'deepseek-emacs)

(defcustom deepseek-api-url "https://api.deepseek.com/v1/refactor"
  "DeepSeek API endpoint."
  :type 'string
  :group 'deepseek-emacs)

(defun deepseek-api-request (input request)
  "Send INPUT and REQUEST to DeepSeek."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " deepseek-api-key))))
        (url-request-data
         (json-encode `(("input" . ,input) ("request" . ,request)))))
    (with-current-buffer (url-retrieve-synchronously deepseek-api-url)
      (goto-char url-http-end-of-headers)
      (json-read-from-string 
       (buffer-substring (point) (point-max))))))

(defun deepseek-select-refactored-code (options)
  "Select from OPTIONS using active completion system."
  (cond
   ((and (fboundp 'vertico-mode) (bound-and-true-p vertico-mode))
    (completing-read "Select refactored code: " options))
   ((and (fboundp 'ivy-read) (bound-and-true-p ivy-mode))
    (ivy-read "Select refactored code: " options))
   ((and (fboundp 'helm) (bound-and-true-p helm-mode))
    (helm :sources (helm-build-sync-source "Options"
                                           :candidates options
                                           :action 'identity)))
   (t (completing-read "Select refactored code: " options))))

(defun deepseek-refactor-region-or-buffer ()
  "Refactor region or buffer using DeepSeek."
  (interactive)
  (let* ((input (if (use-region-p)
                    (buffer-substring-no-properties 
                     (region-beginning) (region-end))
                  (buffer-string)))
         (request (read-string "Refactoring request: "))
         (response (deepseek-api-request input request))
         (options (split-string (alist-get 'text response) "\n" t)))
    (when options
      (let ((choice (deepseek-select-refactored-code options)))
        (if (use-region-p)
            (progn
              (delete-region (region-beginning) (region-end))
              (insert choice))
          (progn
            (erase-buffer)
            (insert choice)))))))

(provide 'deepseek-emacs)
;;; deepseek-emacs.el ends here
