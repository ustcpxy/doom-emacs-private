;;; org/+capture.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org-private|init-capture)

(after! org
  (setq
   org-default-journal-file (expand-file-name "journal.org.gpg" org-directory)
   org-default-notes-file (expand-file-name "inbox.org" org-directory)
   org-default-review-file (expand-file-name "review.org" org-directory)
   ledger-journal-file (expand-file-name "ledger.gpg" org-directory)
   org-default-works-file (expand-file-name "works.org" org-directory)
   )

  ;; The following %-escapes will be replaced with content and expanded:

  ;;   %[pathname] Insert the contents of the file given by
  ;;               pathname.  These placeholders are expanded at the very
  ;;               beginning of the process so they can be used to extend the
  ;;               current template.
  ;;   %(sexp)     Evaluate elisp (sexp) and replace it with the results.
  ;;               Only placeholders pre-existing within the template, or
  ;;               introduced with %[pathname] are expanded this way.  Since this
  ;;               happens after expanding non-interactive %-escapes, those can
  ;;               be used to fill the expression.
  ;;   %<...>      The result of format-time-string on the ... format specification.
  ;;   %t          Time stamp, date only.  The time stamp is the current time,
  ;;               except when called from agendas with M-x org-agenda-capture or
  ;;               with org-capture-use-agenda-date set.
  ;;   %T          Time stamp as above, with date and time.
  ;;   %u, %U      Like the above, but inactive time stamps.
  ;;   %i          Initial content, copied from the active region.  If %i is
  ;;               indented, the entire inserted text will be indented as well.
  ;;   %a          Annotation, normally the link created with org-store-link.
  ;;   %A          Like %a, but prompt for the description part.
  ;;   %l          Like %a, but only insert the literal link.
  ;;   %c          Current kill ring head.
  ;;   %x          Content of the X clipboard.
  ;;   %k          Title of currently clocked task.
  ;;   %K          Link to currently clocked task.
  ;;   %n          User name (taken from the variable user-full-name).
  ;;   %f          File visited by current buffer when org-capture was called.
  ;;   %F          Full path of the file or directory visited by current buffer.
  ;;   %:keyword   Specific information for certain link types, see below.
  ;;   %^g         Prompt for tags, with completion on tags in target file.
  ;;   %^G         Prompt for tags, with completion on all tags in all agenda files.
  ;;   %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
  ;;               You may define a prompt like: %^{Please specify birthday}t.
  ;;               The default date is that of %t, see above.
  ;;   %^C         Interactive selection of which kill or clip to use.
  ;;   %^L         Like %^C, but insert as link.
  ;;   %^{prop}p   Prompt the user for a value for property prop.
  ;;   %^{prompt}  Prompt the user for a string and replace this sequence with it.
  ;;               A default value and a completion table ca be specified like this:
  ;;               %^{prompt|default|completion2|completion3|...}.
  ;;   %?          After completing the template, position cursor here.
  ;;   %\1 ... %\N Insert the text entered at the nth %^{prompt}, where N
  ;;               is a number, starting from 1.
  (defvar personal-account
    "Account||Paypal-Personal|Alipay-Personal" "Personal accounts")

  (setq org-capture-templates
        `(
          ("t" "Todo" entry
           (file+headline org-default-notes-file "Todo")
           "* TODO %^{Logging for...}
:PROPERTIES:
:Created: %U
:END:
%i
%?" )
          ("tl" "Todo with link" entry
           (file org-default-notes-file)
           "* TODO %^{Logging for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" )

          ("wt" "Works Todo" entry
           (file org-default-works-file)
           "* TODO %^{Logging for...}
:PROPERTIES:
:Created: %U
:END:
%i
%?" )
          ("wl" "Works Log" entry
           (file+datetree+prompt org-default-works-file )
           "* %^{Logging for...} :logs:
%^{Effort}p
%^T

- Things to discuss:

%i
%?"   )

          ("M" "Meeting" entry
           (file+olp+datetree org-default-works-file)
           "* %^{Logging for...} :logs:communication:
%^{Effort}p
%^T

- Things to discuss:

%i
%?"  :clock-in t  )


          ("h" "Habit" entry
           (file ,(expand-file-name "habit.org" org-directory))
           "* %^{Habit for...}
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+5d/7d>>\")
:PROPERTIES:
:Created: %U
:STYLE: habit
:END:
%i
%?")

          ("j" "Journal" entry
           (file+datetree+prompt org-default-journal-file)
           "* %^{Logging for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?"   )
          ("i" "Idea" entry
           (file+headline org-default-notes-file "Ideas")
           "* %A :idea:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")


          ("dr" "Daily Review" entry
           (file+olp+datetree org-default-review-file)
           "* %^{Review} :review:daily:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%?"  )
          ("wr" "Week Review" entry
           (file+olp+datetree org-default-review-file)
           "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:week:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?"  )
          ("mr" "Month Review" entry
           (file+olp+datetree org-default-review-file)
           "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:month:%\\1:
:PROPERTIES:
:Created: %U
:END:
%?"  )

          ;; for ledger
          ("ig" "Income:Gifts" plain
           (file ledger-journal-file)
           "%(org-read-date) * 收 %^{Received From} %^{For why} 礼金
  Assets:%^{Account|Personal|Home}  %^{Amount} %^{Currency|CNY|USD|JPY}
  Income:Gifts
")
          ("eg" "Expense:Gifts" plain
           (file ledger-journal-file)
           "%(org-read-date) * 送 %^{Send to} %^{For why} 礼金
  Expense:Gifts  %^{Amount}  %^{Currency|CNY|USD|JPY}
  Assets:%^{Account||Personal|Home}
")
          ("x" "Transfer Ledger Entry" plain
           (file ledger-journal-file)
           "%(org-read-date) * %^{Description|Transfer}
  Assets:%^{To Account||Cash|PayPal-Personal|PayPal-ActualWebSpace|TDBank-Personal|TDBank-ActualWebSpace|DCU-Checking|DCU-Savings|ETRADE-Checking|Bitcoin|ETRADE-RothIRA-Personal|ETRADE-RothIRA-Ananda|ETRADE-Brokerage|Ameriprise-Life-Insurance} %^{Amount}
  Assets:%^{From Account||Cash|PayPal-Personal|PayPal-ActualWebSpace|TDBank-Personal|TDBank-ActualWebSpace|DCU-Checking|DCU-Savings|ETRADE-Checking|Bitcoin|ETRADE-RothIRA-Personal|ETRADE-RothIRA-Ananda|ETRADE-Brokerage|Ameriprise-Life-Insurance} -%\\3
")
          ("a" "Auto Loan Payment" plain
           (file ledger-journal-file)
           "%(org-read-date) * Auto Loan Payment
  Liabilities:Eastern-Bank-Subaru-Outback-Auto-Loan %^{Total Amount Paid (incl. fees)|459.93}
  Assets:PayPal-Personal -%^{Amount Applied to Loan|458.43}
  Expenses:Transaction-Fees -%^{Processing Fee|1.50}
")
          )
        +org-capture-window-params
        `((name . "org-capture")
          (fullscreen . fullwidth)
          (height . 40)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (left-fringe . 0)
          (right-fringe . 0)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (undecorated . t)
          (no-special-glyphs . t)
          (ns-appearance . nil)
          (window-system . ,(cond (IS-MAC 'ns)
                                  (IS-LINUX 'x)
                                  (t 'w32)))
          ,(if IS-LINUX '(display . ":0"))))

  (setq counsel-projectile-org-capture-templates
        '(("t" "TODO" entry (file+headline "${root}/TODO.org" "Tasks")
           "* TODO %^{Logging for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")
          ("f" "FIXME" entry (file+headline "${root}/TODO.org" "Tasks")
           "* FIXME %^{Logging for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")
          ("n" "NOTE" entry (file "${root}/note.org" )
           "* %^{Note for...}
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?")
          )))

(defun +org-private|init-capture ()
  (add-hook 'org-capture-prepare-finalize-hook
            #'(lambda()(if (or (equal "j" (org-capture-get :key))
                          (equal "t" (org-capture-get :key)))
                      (counsel-org-tag))))
  ;; (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync)
  )
