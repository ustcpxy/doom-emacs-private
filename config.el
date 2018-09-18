;;; config.el --- description -*- lexical-binding: t; -*-

(load! "+bindings")

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
(after! cc-mode
  (set! :lookup '(c-mode c++-mode) :xref-backend #'gxref-xref-backend)
  (setq c-basic-offset 4)
  (c-set-offset 'inclass '+)
  (set! :company-backend '(c-mode c++-mode) '(company-gtags company-dabbrev-code))
  (add-hook! (c-mode c++-mode) #'doom|disable-line-numbers)
  )

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

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(after! counsel
  :config
  (progn

;; (defun counsel-imenu (&optional force-rescan jump-immediately)
;;   "Jump to a buffer position indexed by imenu.

;; With one \\[universal-argument] prefix, imenu will rescan the
;; entire buffer regardless of its size.

;; With two \\[universal-argument] prefixes, we'll immediately jump
;; to the definition of the thing at point (assuming that thing is
;; found by imenu)."
;;   (interactive "P")
;;   (setq jump-immediately (<= 16 (car force-rescan)))
;;   (unless (featurep 'imenu)
;;     (require 'imenu nil t))
;;   (let* ((imenu-auto-rescan t)
;;          (imenu-auto-rescan-maxout (if force-rescan
;;                                        (buffer-size)
;;                                      imenu-auto-rescan-maxout))
;;          (items (imenu--make-index-alist t))
;;          (items (delete (assoc "*Rescan*" items) items))
;;          (tap (thing-at-point 'symbol))
;;          (tap-candidate (assoc tap items)))
;;     (if tap-candidate
;;         (imenu (car tap-candidate))
;;       (ivy-read "imenu items:" (counsel-imenu-get-candidates-from items)
;;                 :preselect tap
;;                 :require-match t
;;                 :action (lambda (candidate)
;;                           (with-ivy-window
;;                             ;; In org-mode, (imenu candidate) will expand child node
;;                             ;; after jump to the candidate position
;;                             (imenu (cdr candidate))))
;;                 :caller 'counsel-imenu))))
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

(setq frame-title-format
      '("" " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(setq +doom-modeline-buffer-file-name-style 'file-name)
