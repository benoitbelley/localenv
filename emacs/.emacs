;; Main Program
;; ============
;;
(setq-default debug-on-error nil)
(setq-default debug-on-quit nil)

(setq debug-on-error t)

(setq load-path
      (append
       (mapcar 'expand-file-name '("~/.emacs.d" "~/.emacs.d/haskell-mode-2.8.0"))
       load-path))

(load "align")
(load "dlStyleGuide")
(load "dlAbbrevs")
(load "dlUtils")
(load "dlUtilsExtras")

;; (load "clearcase")

(load "myFontLock")
(load "myUtilities")
(load "myKeyBindings")

(setq debug-on-error nil)
(message "Ready!")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(blink-cursor-mode t)
 '(c++-font-lock-keywords my-c++-font-lock-keywords t)
 '(c++-font-lock-keywords-1 my-c++-font-lock-keywords t)
 '(c++-font-lock-keywords-2 my-c++-font-lock-keywords t)
 '(c++-font-lock-keywords-3 my-c++-font-lock-keywords t)
 '(c-font-lock-keywords my-c++-font-lock-keywords t)
 '(c-font-lock-keywords-1 my-c++-font-lock-keywords t)
 '(c-font-lock-keywords-2 my-c++-font-lock-keywords t)
 '(c-font-lock-keywords-3 my-c++-font-lock-keywords t)
 '(case-fold-search t)
 '(clearcase-suppress-checkout-comments t)
 '(column-number-mode t)
 '(compilation-font-lock-keywords my-compilation-font-lock-keywords t)
 '(compilation-skip-threshold 2)
 '(current-language-environment "UTF-8")
 '(dabbrev-case-distinction t)
 '(dabbrev-case-fold-search nil)
 '(default-input-method "rfc1345")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(gnuserv-frame (quote gnuserv-special-frame-function))
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(ispell-program-name "~/bin/aspell")
 '(locate-command "~/bin/locate")
 '(locate-make-command-line (quote mac-locate-make-command-line))
 '(py-python-command "/usr/bin/python24")
 '(safe-local-variable-values (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)))))
 '(scala-interpreter "~/scala/bin/scala -deprecation")
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(vc-follow-symlinks t)
 '(visible-cursor t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "#BBBBBB"))))
 '(compilation-info ((((class color) (min-colors 16) (background light)) (:foreground "Green4" :weight bold))))
 '(compilation-warning ((((class color) (min-colors 16)) (:foreground "Yellow" :weight bold))))
 '(cperl-nonoverridable-face ((((class color) (background light)) (:foreground "blue"))))
 '(cursor ((t (:background "#40609F"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "DarkOliveGreen"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "blue3"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "red3" :weight semi-light))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "#950055"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "black" :weight semi-light))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "purple4"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "DeepSkyBlue4"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "blue4"))))
 '(font-lock-warning-face ((t (:foreground "Red" :weight bold)))))

