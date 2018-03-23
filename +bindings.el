;;; +bindings.el -*- lexical-binding: t; -*-


;;
(map!

 :v "M-;" #'comment-dwim
 :gnime "M-;" #'comment-line
 :gnime "M-'" #'comment-dwim

 ;; --- Personal vim-esque bindings ------------------
 :nm "gd" #'+lookup/definition
 :nm "gr" #'+lookup/references

 :ne "M-`"   #'swiper
 :nv "M-i"   #'imenu

 (:leader
   ;; Most commonly used
   :desc "Find file in system"     :n "l" #'counsel-locate
   :desc "Switch buffer"           :n ","   #'switch-to-buffer
   :desc "Switch last buffer"      :n "TAB"   #'evil-switch-to-windows-last-buffer
   :desc "Jump char"               :n "SPC"   #'avy-goto-word-or-subword-1
   )
 )

