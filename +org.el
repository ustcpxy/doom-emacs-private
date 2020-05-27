;;; +org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/pkms/gtd/")

;; disable the default org-mode stuck projects agenda view
(setq org-stuck-projects (quote ("" nil nil "")))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;;; customize org agenda view
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("n" "Next Agenda"
               ((agenda ""
                        ((org-agenda-span 'day)
                         (org-agenda-start-day nil)
                         (org-deadline-warning-days 3)))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-with-date t)
                            (org-agenda-sorting-strategy
                             '(effort-up category-keep))))))
              (" " "Block Agenda"
               ((agenda "" nil)
                (tags-todo "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
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
               nil))))

(after! org
  (setq org-log-done t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "READING"))))

  ;; used for filtering block agenda view
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (setq org-agenda-files (quote ("~/pkms/gtd"
                                 "~/pkms/adva"
                                 "~/pkms/notes"
                                 )))
  (setq org-capture-templates
        `(
          ("t" "Tasks" entry (file "inbox.org")
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
           ("w" "Weekly Review" entry (file+olp+datetree ,(concat org-directory "reviews.org"))
            (file ,(concat org-directory "templates/weekly_review.org"))
            :tree-type week)
           ("l" "org-protocol-capture" entry (file "inbox.org")
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
          ("j" "Journal Entry" entry (file+olp+datetree "journal.org")
           "* %U %?")
          ("h" "Habit" entry (file "inbox.org")
           "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
          )
  )
  )
