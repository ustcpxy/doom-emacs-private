;;; +org.el -*- lexical-binding: t; -*-

;; helper function

(after! org
  (setq org-directory "~/pkms/gtd/")
  (setq org-log-done t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "READING"))))

  (setq org-agenda-files (quote ("~/pkms/gtd"
                                 "~/pkms/adva"
                                 "~/pkms/notes"
                                 )))
  (setq org-capture-templates
        `(
          ("t" "Tasks" entry (file,(expand-file-name "inbox.org" org-directory ) )
           "* TODO %?\n%U\n")
          ("b" "Books" entry (file,(expand-file-name "inbox.org" org-directory ) )
           "* Read %^{TITLE}\n\%U%^{AUTHOR}p\n%\\1\n%?"
           :empty-lines 1)
          ("n" "Notes" entry (file,(expand-file-name "inbox.org" org-directory ) )
           "* %^{heading}\n%U\n\n%?")
          ("c" "Calendar" entry (file+headline ,(expand-file-name "tasks.org" org-directory) "Tasks")
            "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
            :empty-lines 1)
          ("o" "Inbox" entry (file+headline ,(expand-file-name "inbox.org" org-directory ) "Others")
           "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
           :empty-lines 1)
          ;; ("s" "Code Snippet" entry (file (concat org-directory "snippets.org"))
          ;;  "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ;; ("w" "work" entry (file+headline (concat org-directory "gtd.org") "GE11X")
          ;;  "* TODO %?\n  %i\n %U"
          ;;  :empty-lines 1)
           ("w" "Weekly Review" entry (file+olp+datetree ,(concat org-directory "reviews.org"))
            (file ,(concat org-directory "templates/weekly_review.org")))
          ("l" "org-protocol-capture" entry (file,(expand-file-name "inbox.org" org-directory ) )
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          ("j" "Journal Entry" entry (file+datetree ,(expand-file-name "journal.org" org-directory ))
           "* %?"
           :empty-lines 1)
          ;; ("p" "Project" entry (file (concat org-directory "project.org"))
          ;;  "* %^{prompt} %^g\n %U\n %i\n %?"
          ;;  :empty-lines 1)
          ;; ("h" "Habit" entry (file (concat org-directory "gtd.org"))
          ;;  "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%U\n")
          )
  )
  )

(setq jethro/org-agenda-directory "~/pkms/gtd/")
(setq jethro/org-agenda-reading-view
      `("r" "Reading" todo ""
        ((org-agenda-files '(,(concat jethro/org-agenda-directory "reading.org"))))))
(defun org-current-is-todo ()
(string= "TODO" (org-get-todo-state)))
(defun jethro/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (or (org-current-is-todo)
                (not (org-get-scheduled-time (point))))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
(goto-char (point-max))))))
;; (add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-reading-view)
(setq jethro/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "inbox.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "Emails")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "emails.org")))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "someday.org")
                                    ,(concat jethro/org-agenda-directory "projects.org")
                                    ,(concat jethro/org-agenda-directory "next.org")))
                ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")))
                ;; (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first)
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda ""
                        ((org-agenda-span 'day)
                         (org-agenda-start-day "+0d")
                         ))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
                ((org-agenda-todo-ignore-deadlines (quote all)))
               ))))
