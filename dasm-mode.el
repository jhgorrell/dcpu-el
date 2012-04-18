;;
;; ~/0x10c/dcpu-el/dasm-mode.el ---
;;
;; $Id: dasm-mode.el,v 1.5 2012/04/18 07:41:00 harley Exp $
;;

;; in your ~/.emacs:
;;   (add-to-list 'load-path "~/path/to/dasm-el")
;;   (add-to-list 'auto-mode-alist '("\\.dasm\\'" . dasm-mode))

(defvar dasm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'dasm-assemble-and-run)
    ;; (define-key map "C-cB" 'dasm-toggle-breakpoint)
    ;; (define-key map "C-cB" 'dasm-toggle-breakpoint)
    map))

(defvar dasm-mode-hooks nil)

(defvar dasm-mode-label-regexp "[a-z0-9_]+")

(defvar dasm-font-lock-keywords
  `((
    ;; comment
    (";.*" . font-lock-comment-face)
    ;;
    ("\\\"[^\\\"\n]*\\\"" . font-lock-string-face)
    ;; "":label"" or "label:"
    (,(concat 
       "\\(:" dasm-mode-label-regexp "\\|"
       dasm-mode-label-regexp ":\\)") . font-lock-function-name-face)
    ;; assembler directives
    ("\\.\\sw+" . font-lock-variable-name-face)
    ;; dasm instructions
    (
     ,(concat 
       "\\<"
       (regexp-opt
        '(
          ;; base
          "add" "and" "bor" "div" "ifb" "ife" "ifg" "ifn" "mod"
          "mul" "set" "shl" "shr" "sub" "xor"
          ;; special
          "jsr"
          ;; pseudo-instrs
          "dat" "word"
          ;; extensions
          "break"
          "print"
          ;; dcpu keyboard reading. (nonstandard)
          "getc"
          ))
       "\\>")
     . font-lock-keyword-face)
     )
    t ;; keywords-only
    t ;; case fold (@todo: case in regexp?
    ))

(defvar dasm-asm-program "./dcpu16.git/a16")

(defun dasm-assemble-file (filename &optional out-file)
  (or out-file (setq out-file (concat filename ".hex ")))
  (let ((buf     (get-buffer-create "*dasm assemble*"))
        (cmd      (concat
                   dasm-asm-program " "
                   "-O hex "
                   "-o " out-file
                   filename)))
    (when (file-exists-p out-file)
      (delete-file out-file))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert ";; cmd" cmd "\n")
    (call-process-shell-command cmd nil buf t)))
;; (dasm-assemble-file "./test-hello-emacs.dasm")

(defun dasm-assemble-and-run ()
  (require 'dcpu)
  (interactive)
  (save-buffer)
  ;;
  (let ((fn (buffer-file-name))
        (buf (current-buffer)))
    ;; 
    (dcpu:activate-cpu (dcpu:new-cpu))
    (dcpu:ui-update)
    ;;
    (dasm-assemble-file fn)
    (dcpu:load-from-file (concat fn ".hex"))
    (when (not dcpu:display-mem-list)
      (dcpu:add-to-mem-list 0 (* 8 16)))
    ;;
    (dcpu:ui-enter)
    (dcpu:ui-update)
    (switch-to-buffer buf)))

(defun dasm-mode ()
  "Major mode for editing DCPU-16 assembly."
  (interactive)
  (kill-all-local-variables)
  ;;
  (setq major-mode 'dasm-mode)
  (setq mode-name "dasm")
  (use-local-map dasm-mode-map)
  ;;
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults dasm-font-lock-keywords)
  ;;
  (run-mode-hooks 'dasm-mode-hooks)
  nil)

;; (eval-buffer)

(provide 'dasm-mode)
