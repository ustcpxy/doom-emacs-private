;;; ../../github/doom-emacs-private/autoload/org-agenda.el -*- lexical-binding: t; -*-

(defvar +org-agenda-fn #'org-agenda
  "Command to use to initiate org-agenda.")

;;;###autoload
(defun +org-agenda/open-frame (&optional initial-input key)
  "Opens the org-agenda window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (make-frame +org-capture-frame-parameters))))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames
    (with-selected-frame frame
      (require 'org-agenda)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-agenda-fn))
            )
        ('error
         (message "org-agenda: %s" (error-message-string ex))
         (delete-frame frame))))))
