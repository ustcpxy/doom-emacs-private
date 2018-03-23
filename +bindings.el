;;; +bindings.el -*- lexical-binding: t; -*-


;;
(map!

 :v "M-;" #'comment-dwim
 :gnime "M-;" #'comment-line

 ;; --- Personal vim-esque bindings ------------------
 :nm "gd" #'+lookup/definition
 :nm "gr" #'+lookup/references
 )

