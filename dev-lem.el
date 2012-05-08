;;
;; ~/0x10c/dcpu-el/dev-lem.el ---
;;
;; $Id: dev-lem.el,v 1.1 2012/05/08 05:44:50 harley Exp $
;;

(require 'dcpu-dev-defs)
(require 'dcpu-aregion)

;;;;;

(defun dev:make-lem ()
  (let ((dev (dcpu:make-dev)))
    (setf (dcpu:dev-hwq-func dev) 'dev:lem-hwq-func
          (dcpu:dev-hwi-func dev) 'dev:lem-hwi-func
          ;;(dcpu:dev-aregion1 dev) (dcpu:make-aregion)
          ;;(dcpu:dev-aregion2 dev) (dcpu:make-aregion)
          )
    dev))
;; (setq dcpu:cur-dev (dev:make-lem))

(defun dev:lem-init (dev)
  (let ((dcpu:a 0)
        (dcpu:b #x8000))
    (dcpu:dev-hwi dev)
    dev))

(defun dev:lem-hwq-func (dev)
  ;; 0x30cf7406
  (setf dcpu:b #x7349
        dcpu:a #xf615
        dcpu:c #x1802
        dcpu:y #x1c6c
        dcpu:x #x8b36)
  dev)
;; (dev:lem-hwq-func dcpu:cur-dev)

(defun dev:lem-hwi-func (dev)
  (cond
   ;;     0: MEM_MAP_SCREEN
   ;;        Reads the B register, and maps the video ram to DCPU-16 ram starting
   ;;        at address B. See below for a description of video ram.
   ;;        If B is 0, the screen is disconnected.
   ;;        When the screen goes from 0 to any other value, the the LEM1802 takes
   ;;        about one second to start up. Other interrupts sent during this time
   ;;        are still processed.
   ((eq dcpu:a 0)
    (let ((areg1 (dcpu:dev-aregion1 dev)))
      ;; this would be the future
      ;;(setf (dcpu:aregion-start  areg1) (if (= dcpu:b 0) nil dcpu:b)
      ;;      (dcpu:aregion-length areg1) (* 32 12))
      (setf dcpu:screen-addr (if (= dcpu:b 0) nil dcpu:b))
      nil))
   ;;     1: MEM_MAP_FONT
   ;;        Reads the B register, and maps the font ram to DCPU-16 ram starting
   ;;        at address B. See below for a description of font ram.
   ;;        If B is 0, the default font is used instead.
   ((eq dcpu:a 1)
    nil)
   ;;     2: MEM_MAP_PALETTE
   ;;        Reads the B register, and maps the palette ram to DCPU-16 ram starting
   ;;        at address B. See below for a description of palette ram.
   ;;        If B is 0, the default palette is used instead.
   ((eq dcpu:a 2)
    nil)
   ;;     3: SET_BORDER_COLOR
   ;;        Reads the B register, and sets the border color to palette index B&0xF
   ((eq dcpu:a 3)
    nil)
   ;;     4: MEM_DUMP_FONT
   ;;        Reads the B register, and writes the default font data to DCPU-16 ram
   ;;        starting at address B.
   ;;        Halts the DCPU-16 for 256 cycles
   ((eq dcpu:a 4)
    (incf dcpu:state-cycles 256)
    nil)
   ;;     5: MEM_DUMP_PALETTE
   ;;        Reads the B register, and writes the default palette data to DCPU-16
   ;;        ram starting at address B.
   ;;        Halts the DCPU-16 for 16 cycles
   ((eq dcpu:a 5)
    (incf dcpu:state-cycles 16)
    nil)
   ;;
   (t
    (error "lem")))
  dev)

;; (setq dcpu:cur-dev (dev:lem-init (dev:make-lem)))
;; (dcpu:aregion-start (dcpu:dev-aregion1 dcpu:cur-dev))

;; (eval-buffer)
(provide 'dev-lem)
