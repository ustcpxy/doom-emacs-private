;;; config.el --- description -*- lexical-binding: t; -*-

(load! +bindings)

(setq +org-dir "~/pkms/gtd/")

(setq doom-theme `doom-molokai)

;; disable quit confirmation
(setq confirm-kill-emacs nil)

;; gtags support
(def-package! gxref
  :when (featurep! :feature lookup)
  :commands (gxref-xref-backend
             gxref-create-db
             gxref-update-db
             gxref-single-update-db
             gxref-set-project-dir)
  )

(after! rtags
  (set! :lookup '(c-mode c++-mode)
    :definition #'rtags-find-symbol-at-point
    :references #'rtags-find-references-at-point
    :xref-backend #'gxref-xref-backend
    )
  ;; rtags use its own location stack, it should bewrapped
  (defun +lookup*xref-pop-marker-stack (orig-fn)
    (let
      (funcall orig-fn)
      (ignore-errors (rtags-location-stack-back))
      )
    )
  (advice-add #'xref-pop-marker-stack :around #'+lookup*xref-pop-marker-stack))

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
(set! :company-backend '(emacs-lisp-mode) '(company-elisp company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(python-mode) '(company-anaconda company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(inferior-python-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(inferior-ess-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(org-mode) '(company-capf company-files company-yasnippet company-dabbrev))
(set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point)

 ;; plantuml and dot
(setq plantuml-jar-path (concat (expand-file-name "local/" doom-private-dir) "plantuml.jar"))
(setq org-plantuml-jar-path plantuml-jar-path)

(def-package! winum
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
    (message "load winum")
    (map!
     :gnime "M-0" #'winum-select-window-0-or-10
     :gnime "M-1" #'winum-select-window-1
     :gnime "M-2" #'winum-select-window-2
     :gnime "M-3" #'winum-select-window-3
     :gnime "M-4" #'winum-select-window-4
     )
    (winum-mode))
  )

