;;
;; ~/projects/games/0x10c/dcpu/programs/test-hw.el ---
;;
;; $Id: dcpu.el,v 1.8 2012/04/13 05:50:47 harley Exp $
;;

;; (progn (setq jhg-cload-enabled nil) (add-to-list 'load-path ".") (eval-buffer))

(require 'dcpu-defs)
(require 'dcpu-util)
(require 'dcpu-cpu)
(require 'dcpu-ui)
(require 'dcpu-display)

(require 'dasm-parse)

(provide 'dcpu)
