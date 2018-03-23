;;; +bindings.el -*- lexical-binding: t; -*-

(map!

 :gnvime "s-r" #'counsel-org-capture
 :gnvime "s-R" #'counsel-projectile-org-capture

 :gnvime "s-g" #'org-agenda-show-daily

 :gnvime "s-b" #'org-brain-visualize

 (:leader
   :nv    "O"  #'org-capture

   (:desc "Org"  :prefix "o"
     ;; org-agenda
     :nv   "#"    #'org-agenda-list-stuck-projects
     :nv   "/"    #'org-occur-in-agenda-files
     :nv   "a"    #'org-agenda-list
     :nv   "e"    #'org-store-agenda-views
     :nv   "ki"   #'org-clock-in-last
     :nv   "kj"   #'org-clock-jump-to-current-clock
     :nv   "ko"   #'org-clock-out
     :nv   "kr"   #'org-resolve-clocks
     :nv   "l"    #'org-store-link
     :nv   "m"    #'org-tags-view
     :nv   "o"    #'org-agenda
     :nv   "s"    #'org-search-view
     :nv   "t"    #'org-todo-list
     ;; SPC C- capture/colors
     :nv   "c"    #'org-capture)))

(add-hook 'org-load-hook #'+org-private|setup-keybinds t)

(defun +org-private|setup-keybinds ()
  (map! :map org-mode-map
        "M-o" #'org-open-at-point
        "M-i" #'org-insert-last-stored-link
        "M-I" #'org-insert-link
        "s-p" #'org-ref-ivy-insert-cite-link
        :n  "RET" #'+org/dwim-at-point
        ;; :n  [tab]     #'org-cycle
        :n  "t"       #'org-todo
        :n  "T"       #'org-insert-todo-heading-respect-content

        :ni "C-c l" #'org-web-tools-insert-link-for-url
        :ni "C-c i" #'org-web-tools-insert-web-page-as-entry
        :ni "C-c I" #'org-web-tools-convert-links-to-page-entrys

        :i "<"  (λ! (if (looking-back "^\\ *")(hydra-org-template/body) (self-insert-command 1)))

        (:localleader
          :desc "Math"              :n "m"                  #'+org-toggle-math
          :desc "Remove link"       :n "L"                  #'+org/remove-link
          :desc "Deadline"          :n "d"                  #'org-deadline
          :desc "C-c C-c"           :n doom-localleader-key #'org-ctrl-c-ctrl-c
          :desc "Edit Special"      :n "'"                  #'org-edit-special
          :desc "Effort"            :n "e"                  #'org-set-effort
          :desc "org-refile"        :n "r"                  #'org-refile
          :desc "Export"            :n [tab]                #'org-export-dispatch
          :desc "Clocking Effort"   :n "E"                  #'org-clock-modify-effort-estimate
          :desc "Property"          :n "p"                  #'org-set-property
          :desc "Get skim link"     :n "="                  (λ! (call-interactively #'evil-append) (insert (+reference/skim-get-annotation)))
          :desc "Narrow to Subtree" :n "n"                  #'org-narrow-to-subtree
          :desc "Narrow to Element" :n "N"                  #'org-narrow-to-element
          :desc "Widen"             :n "w"                  #'widen
          :desc "Lookup"            :n "$"                  #'wordnut-lookup-current-word
          :desc "Toggle heading"    :n "h"                  #'org-toggle-heading
          :desc "Archive Subtree"   :n "A"                  #'org-archive-subtree
          :desc "Toggle Archive"    :n "a"                  #'org-toggle-archive-tag


          (:desc "Clock & Time"
            :prefix "c"
            :nv "c"   #'org-clock-cancel
            :nv "i"   #'org-clock-in
            :nv "o"   #'org-clock-out
            :nv "r"   #'org-resolve-clocks

            :nv "d"   #'org-deadline
            :nv "s"   #'org-schedule
            :nv "t"   #'org-time-stamp
            :nv "T"   #'org-time-stamp-inactive)

          (:desc "Toggle"
            :prefix "T"
            :desc "Column View"       :n "c"    #'org-columns
            :desc "All Column View"   :n "C"    #'(lambda () (interactive)
                                                    (let ((current-prefix-arg 4))
                                                      (call-interactively #'org-columns)))
            :nv "e"   #'org-toggle-pretty-entities
            :nv "i"   #'org-toggle-inline-images
            :nv "l"   #'org-toggle-link-display
            :nv "t"   #'org-show-todo-tree
            :nv "T"   #'org-todo
            :nv "V"   #'space-doc-mode
            :nv "x"   #'org-toggle-latex-fragment)

          ;; Subtree editing
          (:desc "Subtree edit"
            :prefix "s"

            :nv  "a"     #'org-toggle-archive-tag
            :nv  "A"     #'org-archive-subtree
            :nv  "b"     #'org-tree-to-indirect-buffer
            :nv  "h"     #'org-promote-subtree
            :nv  "j"     #'org-move-subtree-down
            :nv  "k"     #'org-move-subtree-up
            :nv  "l"     #'org-demote-subtree
            :nv  "N"     #'org-narrow-to-subtree
            :nv  "W"     #'widen
            :nv  "r"     #'org-refile
            :nv  "K"     #'org-cut-subtree
            :nv  "s"     #'org-sparse-tree
            :nv  "S"     #'org-sort)

          ;; tables
          (:desc  "Table"
            :prefix "t"
            :nv  "a"     #'org-table-align
            :nv  "b"     #'org-table-blank-field
            :nv  "c"     #'org-table-convert
            :nv  "dc"    #'org-table-delete-column
            :nv  "dr"    #'org-table-kill-row
            :nv  "e"     #'org-table-eval-formula
            :nv  "E"     #'org-table-export
            :nv  "h"     #'org-table-previous-field
            :nv  "H"     #'org-table-move-column-left
            :nv  "ic"    #'org-table-insert-column
            :nv  "ih"    #'org-table-insert-hline
            :nv  "iH"    #'org-table-hline-and-move
            :nv  "ir"    #'org-table-insert-row
            :nv  "I"     #'org-table-import
            :nv  "j"     #'org-table-next-row
            :nv  "J"     #'org-table-move-row-down
            :nv  "K"     #'org-table-move-row-up
            :nv  "l"     #'org-table-next-field
            :nv  "L"     #'org-table-move-column-right
            :nv  "n"     #'org-table-create
            :nv  "N"     #'org-table-create-with-table.el
            :nv  "r"     #'org-table-recalculate
            :nv  "s"     #'org-table-sort-lines
            :nv  "tf"    #'org-table-toggle-formula-debugger
            :nv  "to"    #'org-table-toggle-coordinate-overlays
            :nv  "w"     #'org-table-wrap-region)

          ;; Source blocks / org-babel
          ;; :desc  "org-babel"  :prefix "b"
          (:desc  "Babel"
            :prefix "b"

            :nv  "p"    #'org-babel-previous-src-block
            :nv  "n"    #'org-babel-next-src-block
            :nv  "e"    #'org-babel-execute-maybe
            :nv  "o"    #'org-babel-open-src-block-result
            :nv  "v"    #'org-babel-expand-src-block
            :nv  "u"    #'org-babel-goto-src-block-head
            :nv  "g"    #'org-babel-goto-named-src-block
            :nv  "r"    #'org-babel-goto-named-result
            :nv  "b"    #'org-babel-execute-buffer
            :nv  "s"    #'org-babel-execute-subtree
            :nv  "d"    #'org-babel-demarcate-block
            :nv  "t"    #'org-babel-tangle
            :nv  "f"    #'org-babel-tangle-file
            :nv  "c"    #'org-babel-check-src-block
            :nv  "j"    #'org-babel-insert-header-arg
            :nv  "l"    #'org-babel-load-in-session
            :nv  "i"    #'org-babel-lob-ingest
            :nv  "I"    #'org-babel-view-src-block-info
            :nv  "z"    #'org-babel-switch-to-session
            :nv  "Z"    #'org-babel-switch-to-session-with-code
            :nv  "a"    #'org-babel-sha1-hash
            :nv  "x"    #'org-babel-do-key-sequence-in-edit-buffer
            :nv  "."    #'spacemacs/org-babel-transient-state/body)

          ;; insertion
          (:desc "Insert"
            :prefix "i"
            :nv  "d"    #'org-insert-drawer
            :nv  "e"    #'org-set-effort
            :nv  "f"    #'org-footnote-new
            :nv  "h"    #'org-insert-heading
            :nv  "H"    #'org-insert-heading-after-current
            :nv  "K"    #'spacemacs/insert-keybinding-org
            :nv  "l"    #'org-insert-link
            :nv  "p"    #'org-set-property
            :nv  "s"    #'org-insert-subheading
            :nv  "t"    #'org-set-tags)
          )

        (:after org-agenda
          (:map org-agenda-mode-map
            :nm "C-k"      #'evil-window-up
            :nm "C-j"      #'evil-window-down
            :nm "C-h"      #'evil-window-left
            :nm "C-l"      #'evil-window-right
            :nm "C-n"      #'org-agenda-next-item
            :nm "C-p"      #'org-agenda-previous-item

            :nm "<escape>" #'org-agenda-Quit
            :nm "q"        #'org-agenda-Quit
            :nm "J"        #'org-clock-convenience-timestamp-down
            :nm "K"        #'org-clock-convenience-timestamp-up
            :nm "M-j"      #'org-agenda-later
            :nm "M-k"      #'org-agenda-earlier
            :nm "s-o"      #'org-clock-convenience-fill-gap
            :nm "s-e"      #'org-clock-convenience-fill-gap-both
            :nm "\\"       #'ace-window
            :nm "t"        #'org-agenda-todo
            :nm "T"        #'+org-agend-todo-done
            :nm "C-t"      #'org-agenda-todo-yesterday
            :nm "C-S-t"    #'org-agenda-todo-yesterday-done
            :nm "p"        #'org-set-property
            :nm "r"        #'org-agenda-redo
            :nm "e"        #'org-agenda-set-effort
            :nm "H"        #'org-habit-toggle-habits
            :nm "L"        #'org-agenda-log-mode
            :nm "D"        #'org-agenda-toggle-diary
            :nm "G"        #'org-agenda-toggle-time-grid
            :nm ";"        #'counsel-org-tag-agenda
            :nm "s-j"      #'counsel-org-goto-all
            :nm "i"        #'org-agenda-clock-in
            :nm "o"        #'org-agenda-clock-out
            :nm "<tab>"    #'org-agenda-goto
            :nm "C"        #'org-agenda-capture
            :nm "m"        #'org-agenda-bulk-mark
            :nm "u"        #'org-agenda-bulk-unmark
            :nm "U"        #'org-agenda-bulk-unmark-all
            :nm "f"        #'+org@org-agenda-filter/body
            :nm "C-."      #'hydra-org-agenda/body
            :nm "-"        #'org-agenda-manipulate-query-subtract
            :nm "="        #'org-agenda-manipulate-query-add
            :nm "_"        #'org-agenda-manipulate-query-subtract-re
            :nm "$"        #'org-agenda-manipulate-query-add-re
            :nm "d"        #'org-agenda-deadline
            :nm "s"        #'org-agenda-schedule
            :nm "z"        #'org-agenda-view-mode-dispatch
            :nm "S"        #'org-save-all-org-buffers))
        (:after org-src
          (:map org-src-mode-map
            ;; "C-c C-c" nil
            ;; "C-c C-k" nil
            (:localleader
              :desc "Finish" :nm ","  #'org-edit-src-exit
              :desc "Abort"  :nm "k"  #'org-edit-src-abort
              )))
        (:after org-capture
          (:map org-capture-mode-map
            ;; "C-c C-c" nil
            ;; "C-c C-k" nil
            ;; "C-c C-w" nil
            (:localleader
              :desc "Finish" :nm "," #'org-capture-finalize
              :desc "Refile" :nm "r" #'org-capture-refile
              :desc "Abort"  :nm "k" #'org-capture-kill
              )))))

