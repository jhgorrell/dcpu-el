;;
;; ~/projects/games/0x10c/dcpu/programs/test-hw.el ---
;;
;; $Id: dcpu.el,v 1.18 2012/05/03 16:34:07 harley Exp $
;;

;; Homepage:   https://github.com/jhgorrell/dcpu-el
;; Author:     James Harley Gorrell <harley@panix.com>
;; Keywords:   dasm-16, emulator, assembler
;; Version:    0.1
;; License:    GPL v3 or later (same as emacs)

;; (eval-buffer)
;; (require 'dcpu)

(eval-when-compile
  (require 'cl))

;; Add our dir name to load-path so '(load "path/to/dcpu-el/dcpu")' works
(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path dir)
  nil)

;; setup emacs to handle the other files.
(require 'dcpu-elisp)

;; these are in dependency order
(require 'dcpu-defs)
(require 'dcpu-util)
(require 'dcpu-aregion)
(require 'dcpu-opcode-func)
(require 'dcpu-opcode-list)
(require 'dcpu-asm)
(require 'dcpu-disasm)
(require 'dcpu-cpu)
(require 'dcpu-display)
(require 'dcpu-ui)

;; ok!
(provide 'dcpu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; D-Lisp - assembler and 
;; (require 'dl-asm)

;; ;; this is standalone
;; ;; (require 'dasm-mode)

;; ;;
;; (defvar dcpu:el-files
;;   '("dasm-mode.el"
;;     "dcpu.el"
;;     "dcpu-cli.el" "dcpu-cpu.el" "dcpu-defs.el" "dcpu-display.el"
;;     "dcpu-tests.el" "dcpu-ui.el" "dcpu-util.el" "dcpu.el"
;;     "dl-defs.el" "dl-asm.el"
;;     ))
;; ;; (file-expand-wildcards "dcpu*.el")

;; ;;;###autoload
;; (defun dcpu:generate-autoloads ()
;;   (interactive)
;;   (require 'autoload)
;;   (let ((generated-autoload-file (concat default-directory "/dcpu-autoloads.el"))
;;         (autoload-modified-buffers nil))
;;     (if (file-exists-p generated-autoload-file)
;;       (delete-file generated-autoload-file))
;;     (autoload-ensure-default-file generated-autoload-file)
;;     ;;
;;     (dolist (f dcpu:el-files)
;;       (update-file-autoloads f t)
;;       ;;(byte-compile-file f)
;;       nil)
;;     ;;
;;     ;;(byte-compile-file generated-autoload-file)
;;     nil))
;; ;; (progn (eval-buffer) (dcpu:generate-autoloads))

