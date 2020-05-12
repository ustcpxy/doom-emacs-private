;;; +org.el -*- lexical-binding: t; -*-


(after! org
  (set-popup-rule! "^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org \\(?:Links\\|Export Dispatcher\\|Select\\)\\)"
  :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right
  )
  (set-popup-rule! "^\\*Org Agenda" :size #'+popup-shrink-to-fit :side 'right :select t )
  (set-popup-rule! "^\\*Org Src"    '((size . +popup-shrink-to-fit) (side . right)) '((quit) (select . t)))
  (set-popup-rule! "^CAPTURE.*\\.org$" '((size . +popup-shrink-to-fit) (side . right)) '((quit) (select . t)))
  ;; (setq
  ;;  org-modules (quote (org-bibtex org-habit org-info org-protocol org-mac-link org-notmuch))
  ;;  org-ellipsis " ▼ "
  ;;  )
  ;; from https://emacs.stackexchange.com/questions/30520/org-mode-c-c-c-c-to-display-inline-image
  ;; TODO only redisplay affect source block
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (setq org-directory "~/pkms/gtd/")
  (setq org-log-done t)
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "READING"))))

  ;; (push `("c" "Calendar" entry (file+headline ,(expand-file-name "tasks.org" +org-dir) "Tasks")
  ;;                "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
  ;;                :empty-lines 1)
  ;;       org-capture-templates)
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


  (org-add-link-type
   "color"
   (lambda (path)
     (message (concat "color "
                      (progn (add-text-properties
                              0 (length path)
                              (list 'face `((t (:foreground ,path))))
                              path) path))))
   (lambda (path desc format)
     (cond
      ((eq format 'html)
       (format "<span style=\"color:%s;\">%s</span>" path desc))
      ((eq format 'latex)
       (format "{\\color{%s}%s}" path desc)))))

  (setq org-agenda-files (quote ("~/pkms/gtd"
                                 "~/pkms/adva"
                                 "~/pkms/notes"
                                 )))
  )
