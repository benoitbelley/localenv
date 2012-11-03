;; Miscellaneous Utilities
;; =======================

;;
;;  Chose right mode for each file extension
;;
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
		("\\.c?$" . c++-mode)
		("\\.C?$" . c++-mode)
		("\\.cpp$" . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.hh$" . c++-mode)
		("\\.mk$" . makefile-mode)
		("\\.options$" . makefile-mode)
		("\\.h@@/main/.*$" . c++-mode)
		("\\.c@@/main/.*$" . c++-mode)
		("\\.C@@/main/.*$" . c++-mode)
		("\\.cpp@@/main/.*$" . c++-mode)
		("\\.cc@@/main/.*$" . c++-mode)
		("\\.hh@@/main/.*$" . c++-mode)
		("\\.mk@@/main/.*$" . makefile-mode)
		("\\.options@@/main/.*$" . makefile-mode)
		("\\<Makefile.gen$" . makefile-mode)
		("\\<SConstruct$" . python-mode)
		("\\<SConscript$" . python-mode)
		("\\.jam$" . jam-mode)
		)
	      auto-mode-alist ))

;;
;; Auto-saving
;;
(setq auto-save-interval 7500)

;;
;; Various settings
;;

;; Emacs knows nothing about terms
(setenv "TERM" "dumb")

;; Always have line and column numbers
(tool-bar-mode 0)
(line-number-mode 1)
(column-number-mode 1)

(add-hook 'text-mode-hook
          '(lambda () (auto-fill-mode 1)))

;; MacOS X specific stuff
(if (equal system-type `darwin)
    (progn
      (setq mac-option-modifier 'hyper)
      (setq mac-command-modifier 'meta))
)

;; Always add finale newline
;; Always inser spaces instead of tabs 
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
(pending-delete-mode 1)
(setq default-buffer-file-coding-system `undecided-unix)
(setq colon-double-space t)
(setq-default tab-width 4)

(setq find-file-compare-truenames t)
(setq find-file-use-truenames t)

;; Disable the stupid bell!
(defun nobell ()
  ())
(setq ring-bell-function `nobell)

;; Use iswitchb
(iswitchb-mode)
(iswitchb-default-keybindings)

;; Configure psgml
(setq-default sgml-indent-data t)
(setq-default sgml-indent-step 3)

;; Configure jam-mode
(require `jam-mode)
(setq-default jam-indent-size 4)

;;
;; Cool automatic current buffer compilo command
;;
(require `compile)
(require 'dlUtilsExtras)

(defvar compile-current-buffer-command ""
  "*Last shell command used to do a buffer compilation; default for next compilation.")

(defvar compile-current-buffer-command-suffix ""
  "*Last shell command used to do a buffer compilation; default for next compilation.")

(defun compile-current-buffer ()
  "Calls a compilation with the current buffer's object file filename as
         an argument"
  (interactive)
  (let* ((obj-extension
	  (if (equal system-type `windows-nt)
	      ".obj"
	    ".o"))
	 (compile-command 
	  (concat 
	   (default-value 'compile-current-buffer-command ) " " 
	   (file-name-nondirectory  
	    (dl-file-name-sans-extension buffer-file-name)) obj-extension
	    (default-value 'compile-current-buffer-command-suffix))))
    (call-interactively 'compile)))

;; Compile settings
(if (equal system-type `windows-nt)
    (progn
     (setq compile-command "sh -c 'cd ..; scons -Dk'")
     (setq compile-current-buffer-command "sh -c 'scons -Dk")
     (setq compile-current-buffer-command-suffix "'")
     )
  (progn
     (setq compile-command "~/bin/buildmaya mainline 64 debug -j2")
     ;; (setq compile-current-buffer-command "scons -Dk")
     ;; (setq compile-current-buffer-command-suffix "")
   ))


;; Start-up with a 80x60 frame
(setq initial-frame-plist '(width 81 height 60))

;; add source code directory at DL
(load "find-file")
(require 'find-file)
(setq cc-search-directories 
      (append cc-search-directories
	      (list 
	       (concat "../../..")
	       )))

;; here i will define a function that you can invoke to have the current buffer
;; be reverted to the file you would obtain by following symbolic link.  This
;; works normally under UNIX, but with Clearcase under Windows, the filesystem
;; doesn't show symbolic links, so it normally doesn't work.  I just added a
;; parameter to take the symlink chasing function as input (file-truename-func).

(defun revert-file-follow-symlink (&optional file-truename-func silent)
  "find-alternate file by following symlink."
  (interactive)
  (let ((real-name (funcall (or file-truename-func 'file-truename)
			    buffer-file-name)))
    (if (not (equal real-name buffer-file-name))
	(progn
	  (if (not silent)
	      (message (concat "Following link(s) to " real-name)))
	  (kill-buffer (current-buffer))
	  (find-file real-name))
      (if (not silent)
	  (message "Nothing to revert, file is not a symlink.")))
    ))

;; here i define a simple function to chase the symlink of a path inside a
;; clearcase vob and return the truename of the file.

(load "clearcase")

(defun clearcase-file-truename (path)
  "Follows the clearcase link of a path until it's not a link anymore."
  (let ((p path))
    (while (clearcase-fprop-file-is-vob-slink-p p)
      (setq p (clearcase-path-follow-if-vob-slink p)))
    p))


;; here i define a simple intertive command that can be invoked to revert the
;; current buffer with the file as its true filename.

(defun clearcase-follow ()
  (interactive)
  (if (equal system-type `windows-nt)
      (revert-file-follow-symlink 'clearcase-file-truename t)
      (revert-file-follow-symlink 'file-truename t)))

;; i bind it under 'C-x v f', fits in well with the other emacs clearcase
;; bindings
(define-key clearcase-prefix-map "f" 'clearcase-follow)

;;
;; Customize C/C++ mode
;;

(require 'dlIncludeGuard)
(add-hook 
 'c-mode-common-hook 
 '(lambda()
    
    ;; Maximum decoration
    (setq font-lock-maximum-decoration t)

    ;; Turn on auto-fill and hungry-delete minor modes
    (auto-fill-mode 1)
    (c-toggle-auto-hungry-state 1)

    ;; here i setup a hook so that after invking ff-find-file (bound
    ;; to 'C-c o' for me), the buffer is automatically chased thru to
    ;; the truename of the file. Unfortunately this opens the buffer
    ;; twice but i couldn't find a simpler way to do this rather than
    ;; to fiddle in the code of find-file itself.
    (add-hook 'ff-post-load-hooks 'clearcase-follow)

    ;; include guards updates.
    (local-set-key "\C-ci" 'dl-update-include-guard)
    )
 )

;;
;; Customize haskell-mode
;;

(load "haskell-site-file")

(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;;
;; Customize reStructuredText mode
;;

(require 'rst) 

(add-hook 'text-mode-hook 'rst-text-mode-bindings)
(add-hook 'text-mode-hook 'rst-set-paragraph-separation)


;;
;; Scala
;;

(add-to-list 'load-path "~/.emacs.d/scala")
(require 'scala-mode-auto)


;;
;; locate
;;
(defun mac-locate-make-command-line (arg)
  (list "~/bin/locate" "-i" "--database=/Volumes/repos/mainline/locatedb" arg))

;; (defun mac-locate-make-command-line (arg)
;;   (list "~/bin/locate" "-i" "--database=/Volumes/repos/Maya2013ap/locatedb" arg))
