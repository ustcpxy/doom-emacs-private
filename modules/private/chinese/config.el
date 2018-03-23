;; ;;; config.el -*- lexical-binding: t; -*-

(message "load chinese")
;; Fonts
(def-package! cnfonts
  :init
  (add-hook 'after-init-hook #'cnfonts-enable)
  :config
  (setq cnfonts-keep-frame-size nil)
  (setq cnfonts-use-cache t)
  (setq cnfonts-profiles
        '("program1" "program2" "program3" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program1" . 4)
                                  ("program2" . 5)
                                  ("program3" . 3)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

(def-package! fcitx
  :config
  ;; Make sure the following comes before `(fcitx-aggressive-setup)'
  ;; (fcitx-evil-turn-off)                                  ; turn on in chinese
  (setq fcitx-active-evil-states '(insert emacs))
  (fcitx-prefix-keys-add "M-m" "C-M-m") ; M-m is common in Spacemacs
  (fcitx-prefix-keys-add "C-x" "C-c" "C-h" "M-s" "M-o")
  ;; (fcitx-prefix-keys-turn-on)
  ;; (fcitx-default-setup)
  (fcitx-aggressive-setup)
  ;; (setq fcitx-use-dbus t) ; uncomment if you're using Linux
  )


(def-package! youdao-dictionary
  ;; :disabled
  :commands youdao-dictionary-search-at-point+
  :bind ("s-y" . youdao-dictionary-search-at-point+)
  :init
  (progn
    ;; Enable Cache
    (setq
     ;; conflic with magithub
     ;; url-automatic-caching t

     ;; Set file path for saving search history
     youdao-dictionary-search-history-file
     (concat doom-cache-dir ".youdao")
     ;; Enable Chinese word segmentation support
     youdao-dictionary-use-chinese-word-segmentation t))
  )

(def-package! cal-china-x
  :commands cal-china-x-setup
  :init (add-hook 'calendar-load-hook #'cal-china-x-setup)
  :config
  ;; `S' can show the time of sunrise and sunset on Calendar
  (setq calendar-latitude +39.9
        calendar-longitude +116.4

        calendar-location-name "Huai-hua"

        )

  ;; Holidays
  (setq calendar-mark-holidays-flag t)

  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays
        '((holiday-lunar 1 15 "元宵节")
          (holiday-lunar 7 7 "七夕节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 12 "植树节")
          (holiday-fixed 5 4 "青年节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-fixed 9 10 "教师节")))
  (setq holiday-other-holidays
        '((holiday-fixed 2 14 "情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 12 25 "圣诞节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-float 11 4 4 "感恩节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))


;; Support pinyin in Ivy
;; Input prefix '!' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(def-package! pinyinlib
  :commands pinyinlib-build-regexp-string
  :after ivy
  :init
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order str)))

  (setq ivy-re-builders-alist
        '((t . re-builder-pinyin)))

  (defun my-pinyinlib-build-regexp-string (str)
    (cond ((equal str ".*")
           ".*")
          (t
           (pinyinlib-build-regexp-string str t))))

  (defun my-pinyin-regexp-helper (str)
    (cond ((equal str " ")
           ".*")
          ((equal str "")
           nil)
          (t
           str)))

  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str))
           nil)
          ((equal (substring str 0 1) "!")
           (mapconcat 'my-pinyinlib-build-regexp-string
                      (remove nil (mapcar 'my-pinyin-regexp-helper
                                          (split-string
                                           (replace-regexp-in-string "!" "" str ) "")))
                      ""))
          (t
           nil))))
