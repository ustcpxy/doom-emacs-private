;;; +org.el -*- lexical-binding: t; -*-


;; keybinding settings

(add-hook 'org-load-hook #'+org-private|setup-keybinds t)

(defun +org-private|setup-keybinds ()
(map!
      (:after evil-org
        (:map evil-org-mode-map
          :nm "t" #'org-todo
          :nm "r" #'org-refile
          :nm "s" #'org-schedule
          (:localleader
            :nm "SPC" #'org-set-tags
            )))
      (:after org-agenda
        (:map org-agenda-mode-map
          :nm "t"        #'org-agenda-todo
          :nm "d"        #'org-agenda-deadline
          :nm "s"        #'org-agenda-schedule
          ))
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
            ))))
)

(after! org-agenda
  (remove-hook 'org-agenda-finalize-hook #'+org|exclude-agenda-buffers-from-workspace)
  )
