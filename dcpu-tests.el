;;
;; ~/0x10c/dcpu-el/dcpu-tests.el ---
;;
;; $Id: dcpu-tests.el,v 1.7 2012/04/21 21:54:31 harley Exp $
;;

;; (progn (setq jhg-cload-enabled nil) (add-to-list 'load-path "."))

(require 'dcpu)

(defun dcpu-test-1 ()
  (interactive)
  (dcpu:activate-cpu (dcpu:new-cpu))
  (dcpu:set-mem-bulk
   0
   #x7c01 #x0030 #x7de1 #x1000 #x0020 #x7803 #x1000 #xc00d ;; 0000:
   #x7dc1 #x001a #xa861 #x7c01 #x2000 #x2161 #x2000 #x8463 ;; 0008: 
   #x806d #x7dc1 #x000d #x9031 #x7c10 #x0018 #x7dc1 #x001a ;; 0010:
   #x9037 #x61c1 #x7dc1 #x001a #x0000 #x0000 #x0000 #x0000 ;; 0018:
   )
  ;;
  (dcpu:trace-buffer-clear)
  (setq dcpu:display-mem-list '((0 #x30) 
                                (#x1000 8)
                                (#xfff0 16)))
  ;;
  (dcpu:clear-all-breakpoint-addr)
  (dcpu:set-breakpoint-addrs #x0002)
  ;;
  (dcpu:ui-enter)
  nil)
;; (progn (eval-buffer) (dcpu-test-1))

(defun dcpu-test-2 ()
  (dcpu:activate-cpu (dcpu:new-cpu))
  (dcpu:load-from-file "./fib.dbin")
  (dcpu:ui-enter))
;; (dcpu-test-2)

;; (global-set-key [f11] 'dcpu-test-2)
;; (global-set-key [f12] 'dcpu:ui-run)
;; (progn (eval-buffer) (dcpu-test-1))
;; (window-tree)

;; (require 'elp)
;; (elp-instrument-package "dcpu:")
;; (elp-results)

