;;; +bindings.el -*- lexical-binding: t; -*-


;;
(map!

 :gnvime "M-;" #'comment-dwim

 ;; --- Personal vim-esque bindings ------------------
 :nm "gd" #'+lookup/definition
 :nm "gr" #'+lookup/references
 )

