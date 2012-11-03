;;
;; Perforce setting
;;

(setenv "P4CONFIG" "p4config.txt")
(setenv "P4USER" "belleyb")
(setenv "P4PORT" "eserver:1666")

(setq-default p4-executable "/Users/benoit/bin/p4")

(require `p4)
