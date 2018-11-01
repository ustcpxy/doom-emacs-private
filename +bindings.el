;;; +bindings.el -*- lexical-binding: t; -*-


;;
(map!

 :gnime "<f12>" #'org-agenda
 :gnime "<f9>" #'org-capture
 :gnime "<f8>" #'org-capture-finalize
 :gnime "<f5>" #'org-refile
 :gnime "M-<f9>" #'org-capture-refile
 :gnime "M-<f8>" #'org-capture-kill

 :v "M-;" #'comment-dwim
 :gnime "M-;" #'comment-line
 :gnime "M-'" #'comment-dwim

 ;; --- Personal vim-esque bindings ------------------
 :nm "gd" #'+lookup/definition
 :nm "gr" #'+lookup/references

 :ne "M-`"   #'swiper
 :nv "M-i"   #'imenu
 :gnime "M-/"   #'helm-grep-rg

 (:leader
   ;; Most commonly used
   :desc "Find file in system"     :n "l" #'helm-for-files
   :desc "Switch buffer"           :n ","   #'switch-to-buffer
   :desc "Switch last buffer"      :n "TAB"   #'evil-switch-to-windows-last-buffer
   :desc "Jump char"               :n "SPC"   #'avy-goto-word-or-subword-1

   (:desc "project" :prefix "p"
     :desc "Search in project"        :n  "s" #'helm-projectile-ag)
   )
 )


(map! [remap org-capture] nil)
