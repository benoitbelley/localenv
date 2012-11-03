;;;
;;;; Keys binding
;;;
(global-set-key [M-C-down]                  'end-of-defun)
(global-set-key [M-C-left]                  'backward-sexp)
(global-set-key [M-C-right]                 'forward-sexp)
(global-set-key "\C-x\C-r"                  'revert-buffer)
(global-set-key "\M-G"                      'goto-line)

(global-set-key [kp-left]                   'scroll-left)
(global-set-key [kp-right]                  'scroll-right)
(global-set-key [kp-up]                     'scroll-up)
(global-set-key [kp-down]                   'scroll-down)

(global-set-key [f2]                        'call-last-kbd-macro)
(global-set-key [f5]                        'next-error)
(global-set-key [f7]                        'my-manual-entry)
(global-set-key [f8]                        'ispell-region)
(global-set-key [f9]                        'repeat-complex-command)
(global-set-key [f10]                       'grep)
(global-set-key [f11]                       'compile)
(global-set-key [C-f11]                     'compile-current-buffer)
(global-set-key [f12]                       'locate)
;; (global-set-key [f12]                       'dl-find-other-file-bis)

(add-hook 
 'c-mode-common-hook 
 '(lambda()

    ;; Set TAB key to indent at beginning of line, insert TAB
    ;; elsewhere.
    (setq c-tab-always-indent nil)

    ;; Enter key reindents
    (define-key c-mode-map [(control m)]
      'reindent-then-newline-and-indent)
    (define-key c-mode-map [(return)]
      'reindent-then-newline-and-indent)
    (define-key c++-mode-map [(control m)]
      'reindent-then-newline-and-indent)
    (define-key c++-mode-map [(return)]
      'reindent-then-newline-and-indent)
    (define-key c++-mode-map [(control meta N)]
      'c++-align-parameters)
    (define-key c++-mode-map [(control meta S)]
      'c++-align-data-members)
    )
 )
	  
