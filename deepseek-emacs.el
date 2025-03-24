;;; deepseek-emacs.el --- A DeepSeek-powered Emacs package for code refactoring and interactive dialog

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (ivy "0.13.0") (helm "3.0"))
;; Keywords: refactoring, ai, deepseek

;;; Commentary:
;; This package provides an interface to DeepSeek for code refactoring and interactive dialog.

;;; Code:

(defgroup deepseek-emacs nil
  "Customize DeepSeek Emacs integration."
  :group 'tools)

(defcustom deepseek-command "deepseek"
  "The command to run DeepSeek."
  :type 'string
  :group 'deepseek-emacs)

(defcustom deepseek-api-key nil
  "API key for DeepSeek (if required)."
  :type 'string
  :group 'deepseek-emacs)

(defcustom deepseek-completion-framework 'ivy
  "The completion framework to use (ivy or helm)."
  :type '(choice (const ivy) (const helm))
  :group 'deepseek-emacs)

(defun deepseek-api-request (input request)
  "Send INPUT and REQUEST to DeepSeek and return the response."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " deepseek-api-key))))
        (with-temp-buffer
          (insert (json-encode `((input . ,input) (request . ,request))))
          (let ((response (url-retrieve-synchronously "https://api.deepseek.com/v1/refactor")))
            (with-current-buffer response
              (goto-char (point-min))
              (re-search-forward "\n\n")
              (json-read-from-string (buffer-substring (point) (point-max))))))

        (defun deepseek-refactor-region-or-buffer ()
          "Refactor the selected region or buffer using DeepSeek."
          (interactive)
          (let* ((input (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (buffer-string))) ; Use region or entire buffer
                 (request (read-string "Enter your refactoring request (e.g., 'add comments', 'optimize performance'): "))
                 (refactored-content (deepseek-api-request input request)))

            ;; Check if refactoring produced output
            (if (string-empty-p refactored-content)
                (message "Refactoring failed or produced no output.")
              (progn
                ;; Split refactored output into lines for selection
                (let ((refactored-options (split-string refactored-content "\n" t)))
                  (cond
                   ((eq deepseek-completion-framework 'ivy)
                    (ivy-read "Select the best refactored code: "
                              refactored-options
                              :action (lambda (candidate)
                                        (if (use-region-p)
                                            (progn
                                              (delete-region (region-beginning) (region-end))
                                              (insert candidate))
                                          (progn
                                            (erase-buffer)
                                            (insert candidate)))
                                        (message "Refactored code replaced successfully!"))))
                   ((eq deepseek-completion-framework 'helm)
                    (helm :sources (helm-build-sync-source "Refactored Options"
                                                           :candidates refactored-options
                                                           :action (lambda (candidate)
                                                                     (if (use-region-p)
                                                                         (progn
                                                                           (delete-region (region-beginning) (region-end))
                                                                           (insert candidate))
                                                                       (progn
                                                                         (erase-buffer)
                                                                         (insert candidate)))))))))))))

        (defun deepseek-refactor-with-dialog ()
          "Refactor the selected region or buffer using an interactive dialog with DeepSeek."
          (interactive)
          (let* ((input (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (buffer-string))) ; Use region or entire buffer
                 (request (read-string "Enter your refactoring request (e.g., 'add comments', 'optimize performance'): "))
                 (dialog-buffer "*DeepSeek Dialog*")
                 (refactored-content nil)
                 (final-code nil))

            ;; Start the dialog with DeepSeek
            (with-current-buffer (get-buffer-create dialog-buffer)
              (erase-buffer)
              (insert "=== DeepSeek Refactor Dialog ===\n\n")
              (insert "Your request: " request "\n\n")
              (insert "Original code:\n" input "\n\n")
              (insert "Waiting for DeepSeek response...\n")
              (display-buffer dialog-buffer))

            ;; Send the initial request to DeepSeek
            (setq refactored-content (deepseek-api-request input request))
            (with-current-buffer dialog-buffer
              (insert "DeepSeek's initial response:\n" refactored-content "\n\n")
              (insert "Type your follow-up questions or requests below:\n"))

            ;; Interactive dialog loop
            (while (not final-code)
              (let ((user-input (read-string "Your follow-up (or press Enter to apply the code): ")))
                (if (string-empty-p user-input)
                    (setq final-code refactored-content) ; User is satisfied, apply the code
                  (progn
                    ;; Send follow-up to DeepSeek
                    (setq refactored-content (deepseek-api-request refactored-content user-input))
                    (with-current-buffer dialog-buffer
                      (insert "Your follow-up: " user-input "\n\n")
                      (insert "DeepSeek's response:\n" refactored-content "\n\n"))))))

            ;; Apply the final refactored code
            (if (use-region-p)
                (progn
                  (delete-region (region-beginning) (region-end))
                  (insert final-code))
              (progn
                (erase-buffer)
                (insert final-code)))
            (message "Refactored code applied successfully!")))

        (provide 'deepseek-emacs)

;;; deepseek-emacs.el ends here
