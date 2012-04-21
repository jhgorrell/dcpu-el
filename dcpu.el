;;
;; ~/projects/games/0x10c/dcpu/programs/test-hw.el ---
;;
;; $Id: dcpu.el,v 1.11 2012/04/21 21:54:31 harley Exp $
;;

;; Homepage:   https://github.com/jhgorrell/dcpu-el
;; Author:     James Harley Gorrell <harley@panix.com>
;; Keywords:   dasm-16, emulator, assembler
;; Version:    0.1
;; License:    GPL v3 or later (same as emacs)

;; (eval-buffer)
;; Add our dir name to load-path so '(load "path/to/dcpu-el/dcpu")' works
(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path dir)
  nil)

;; these are in dependency order

;; the emulator
(require 'dcpu-defs)
(require 'dcpu-util)
(require 'dcpu-cpu)
(require 'dcpu-display)
(require 'dcpu-ui)

;; the assembler
(require 'dasm-parse)

;; this is standalone
;; (require 'dasm-mode)

;;
(defvar dcpu:el-files
  '("dasm-mode.el"
    "dasm-parse.el"
    "dcpu.el"
    "dcpu-cli.el" "dcpu-cpu.el" "dcpu-defs.el" "dcpu-display.el"
    "dcpu-tests.el" "dcpu-ui.el" "dcpu-util.el" "dcpu.el"))
;; (file-expand-wildcards "dcpu*.el")

;;;###autoload
(defun dcpu:generate-autoloads ()
  (interactive)
  (require 'autoload)
  (let ((generated-autoload-file (concat default-directory "/dcpu-autoloads.el"))
        (autoload-modified-buffers nil))
    (if (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file))
    (autoload-ensure-default-file generated-autoload-file)
    ;;
    (dolist (f dcpu:el-files)
      (update-file-autoloads f nil)
      ;;(byte-compile-file f)
      nil)
    (autoload-save-buffers)
    ;;
    ;;(byte-compile-file generated-autoload-file)
    nil))
;; (progn (eval-buffer) (dcpu:generate-autoloads))

;; ok!
(provide 'dcpu)
