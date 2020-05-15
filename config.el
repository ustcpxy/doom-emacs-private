;;; config.el --- description -*- lexical-binding: t; -*-

(setq doom-theme `doom-molokai)
(setq doom-font (font-spec :family "monospace" :size 16))

; Specify font for Chinese characters Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
         when (font-installed-p font)
         return (set-fontset-font t '(#x4e00 . #x9fff) font))

;; disable quit confirmation
(setq confirm-kill-emacs nil)

(setq doom-localleader-key ",")

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
 :n "M-s" #'save-buffer
 :n "M-w" #'delete-window
 :n "M-q" (if (daemonp) #'delete-frame #'evil-quit-all)

 (:leader
   ;; Most commonly used
   :desc "Find file in system"     :n "l" #'helm-for-files
   :desc "Switch buffer"           :n ","   #'switch-to-buffer
   :desc "Switch last buffer"      :n "TAB"   #'evil-switch-to-windows-last-buffer
   :desc "Jump char"               :n "SPC"   #'avy-goto-word-or-subword-1
   )

 ;; use "TAB" instead of "C-SPC" to bind preview function
 (:when (featurep! :completion ivy)
       ;; (:after ivy
       ;;  :map ivy-minibuffer-map
       ;;  "TAB" #'ivy-call-and-recenter  ; preview file
       ;;  )
       (:after counsel
        :map counsel-ag-map
        "TAB"    #'ivy-call-and-recenter ; preview
        ))
 )

(setq frame-title-format
      '("" " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; gtags support
(use-package! gxref
  :when (featurep! :feature lookup)
  :commands (gxref-xref-backend
             gxref-create-db
             gxref-update-db
             gxref-single-update-db
             gxref-set-project-dir)
  )

;; *** Company
(after! company
  (setq company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-tooltip-minimum-width 60
        company-tooltip-margin 0
        company-show-numbers t
        company-tooltip-offset-display nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-childframe-child-frame nil))
(set-company-backend! '(emacs-lisp-mode) '(company-elisp company-files company-yasnippet company-dabbrev-code))
(set-company-backend! '(python-mode) '(company-anaconda company-files company-yasnippet company-dabbrev-code))
(set-company-backend! '(inferior-python-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set-company-backend! '(inferior-ess-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set-company-backend! '(org-mode) '(company-capf company-files company-yasnippet company-dabbrev))
(set-lookup-handlers! 'emacs-lisp-mode
  :documentation #'helpful-at-point)
(global-company-mode +1)

(after! cc-mode
  (set-lookup-handlers! '(c-mode c++-mode)
    :xref-backend #'gxref-xref-backend)
  (setq c-basic-offset 4)
  (c-set-offset 'inclass '+)
  (set-company-backend! '(c-mode c++-mode) '(company-gtags company-dabbrev-code))
  )

 ;; plantuml and dot
(setq plantuml-jar-path (concat (expand-file-name "local/" doom-private-dir) "plantuml.jar"))
(setq org-plantuml-jar-path plantuml-jar-path)

(use-package! winum
  :config
  (progn
    (defun spacemacs//winum-assign-func ()
      "Custom number assignment for neotree."
      (when (and (boundp 'neo-buffer-name)
                 (string= (buffer-name) neo-buffer-name)
                 ;; in case there are two neotree windows. Example: when
                 ;; invoking a transient state from neotree window, the new
                 ;; window will show neotree briefly before displaying the TS,
                 ;; causing an error message. the error is eliminated by
                 ;; assigning 0 only to the top-left window
                 (eq (selected-window) (frame-first-window)))
        0))
    (add-to-list 'winum-assign-functions #'spacemacs//winum-assign-func)
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line nil
          winum-ignored-buffers '(" *which-key*"))
    (map!
     :gnime "M-0" #'winum-select-window-0-or-10
     :gnime "M-1" #'winum-select-window-1
     :gnime "M-2" #'winum-select-window-2
     :gnime "M-3" #'winum-select-window-3
     :gnime "M-4" #'winum-select-window-4
     )
    (winum-mode))
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(after! counsel
  :config
  (progn

    (defun my-counsel-imenu (&optional force-rescan jump-immediately)
      "Jump to a buffer position indexed by imenu.

With one \\[universal-argument] prefix, imenu will rescan the
entire buffer regardless of its size.

With two \\[universal-argument] prefixes, we'll immediately jump
to the definition of the thing at point (assuming that thing is
found by imenu)."
      (interactive "P")
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (let* ((imenu-auto-rescan t)
             (imenu-auto-rescan-maxout (buffer-size))
             (items (imenu--make-index-alist t))
             (items (delete (assoc "*Rescan*" items) items))
             (tap (thing-at-point 'symbol))
             (tap-candidate (assoc tap items)))
        (if tap-candidate
            (imenu (car tap-candidate))
          (ivy-read "imenu items:" (counsel-imenu-get-candidates-from items)
                    :preselect tap
                    :require-match t
                    :action (lambda (candidate)
                              (with-ivy-window
                                ;; In org-mode, (imenu candidate) will expand child node
                                ;; after jump to the candidate position
                                (imenu (cdr candidate))))
                    :caller 'counsel-imenu))))
    )
  )


(setq +doom-modeline-buffer-file-name-style 'file-name)

(setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")
(setq helm-ag-insert-at-point 'symbol)
;;; Default ag arguments
;; https://github.com/ggreer/the_silver_searcher
(defconst modi/ag-arguments
  '("--nogroup" ;mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
    "--skip-vcs-ignores"                ;Ignore files/dirs ONLY from `.ignore'
    "--numbers"                         ;Line numbers
    "--smart-case"
    ;; "--one-device"                      ;Do not cross mounts when searching
    "--follow"                          ;Follow symlinks
    "--ignore" "#*#") ;Adding "*#*#" or "#*#" to .ignore does not work for ag (works for rg)
  "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
packages.")

;;; Default rg arguments
;; https://github.com/BurntSushi/ripgrep
(defconst modi/rg-arguments
  `("--no-ignore-vcs"                   ;Ignore files/dirs ONLY from `.ignore'
    "--line-number"                     ;Line numbers
    "--smart-case"
    "--follow"                 ;Follow symlinks
    "--max-columns" "150"      ;Emacs doesn't handle long line lengths very well
    "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
  "Default rg arguments used in the functions in `counsel' and `projectile'
packages.")

(after! projectile
  :config
  (progn
    (defun modi/advice-projectile-use-ag (&rest _args)
      "Always use `ag' for getting a list of all files in the project."
      (mapconcat #'shell-quote-argument
                 (append '("ag")
                         modi/ag-arguments
                         '("-0"         ;Output null separated results
                           "-g" ""))    ;Get file names matching "" (all files)
" "))
   (defun modi/advice-projectile-use-rg (&rest _args)
      "Always use `rg' for getting a list of all files in the project."
      (let* ((prj-user-ignore-name (expand-file-name
                                    (concat ".ignore." user-login-name)
                                    (projectile-project-root)))
             (prj-user-ignore (when (file-exists-p prj-user-ignore-name)
                                (concat "--ignore-file " prj-user-ignore-name))))
        (mapconcat #'shell-quote-argument
                   (if prj-user-ignore
                       (append '("rg")
                               modi/rg-arguments
                               `(,prj-user-ignore)
                               '("--null" ;Output null separated results
                                 ;; Get names of all the to-be-searched files,
                                 ;; same as the "-g ''" argument in ag.
                                 "--files"))
                     (append '("rg")
                             modi/rg-arguments
                             '("--null"
                               "--files")))
                   " ")))

    ;; Use `rg' all the time if available
    (if (executable-find "rg")
        (progn
          (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-ag)
          (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg))
      ;; Else use `ag' if available
      (when (executable-find "ag")
        (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-rg)
        (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-ag)))
    ))

(setq select-enable-primary t)

; https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
; display ansi color in emacs
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t)))
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(load! "+org")
