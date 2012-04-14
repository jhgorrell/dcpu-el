;;
;; ~/projects/games/0x10c/dcpu/programs/test-hw.el ---
;;
;; $Id: dcpu.el,v 1.9 2012/04/13 23:55:59 harley Exp $
;;

;; (eval-buffer)
;; Add our dir name to load-path so '(load "path/to/dcpu-el/dcpu")' works
(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path dir)
  nil)

;; these are in dependency order

;; this is standalone
(require 'dasm-mode)

;; the emulator
(require 'dcpu-defs)
(require 'dcpu-util)
(require 'dcpu-cpu)
(require 'dcpu-display)
(require 'dcpu-ui)

;; the assembler
(require 'dasm-parse)

;; ok!
(provide 'dcpu)
