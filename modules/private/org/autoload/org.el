;;; org.el -*- lexical-binding: t; -*-

;;;###autoload
 (defun doom/org-brain-add-resource ()
   "Add a URL from the clipboard as an org-brain resource.
 Suggest the URL title as a description for resource."
   (interactive)
   (let* ((url (org-web-tools--get-first-url))
          (html (org-web-tools--get-url url))
          (title (org-web-tools--html-title html)))
     (org-brain-add-resource
      url
      title
      t)))

;;;###autoload
(defun org-agenda-show-daily (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

;;;###autoload
(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes too
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
          (org-insert-heading)
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading-respect-content)
                 (outline-show-entry))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return)))))

;;;###autoload
(defun export-diary-from-cal ()
  (interactive)
  (start-process-shell-command "export diary" nil "/Users/xfu/.emacs.d-backup/private/local/calendardiary 30 > /Users/xfu/Dropbox/org/cal.diary"))

;;;###autoload
(defun +org/open-brain-here ()
    (interactive)
    (let ((org-brain-path (projectile-project-root)))
      (call-interactively 'org-brain-visualize)))

;;;###autoload
(defun reflash-indentation ()
  "Fix org-indent issues, center line."
  (interactive)
  (org-indent-mode 1)
  (recenter-top-bottom))


;;;###autoload
(defun +org/work-on-heading ()
  (interactive)
  (org-clock-in)
  (org-tree-to-indirect-buffer)
  (map! :map org-mode-map
        :ni "<s-return>" #'+org/finish-work-on-heading))

;;;###autoload
(defun +org/finish-work-on-heading ()
  (interactive)
  (setq *org-git-notes (nth 4 (org-heading-components)))
  (org-clock-out)
  (save-buffer)
  (let ((file (buffer-file-name)))
    (magit-call-git "add" file)
    (magit-call-git "commit" "-m" *org-git-notes)
    (magit-refresh))
  (widen)
  (print "Work finished!")
  (map! :map org-mode-map
        :ni "<s-return>" #'+org/work-on-heading))

