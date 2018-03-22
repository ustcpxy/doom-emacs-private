;;; config.el --- description -*- lexical-binding: t; -*-

(load! +bindings)

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
