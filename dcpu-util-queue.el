;;
;; ~/0x10c/dcpu-el/dcpu-util-queue.el ---
;;
;; $Id: dcpu-util-queue.el,v 1.3 2012/05/08 23:02:28 harley Exp $
;;


(defun dcpu:make-queue ()
  (list 'dcpu:queue nil nil))

(defun dcpu:queue-p (queue)
  (and (listp queue) (eq (car queue) 'dcpu:queue) queue))

(defun dcpu:queue-head (queue)
  (assert (dcpu:queue-p queue))
  (cadr queue))

(defun dcpu:queue-tail (queue)
  (assert (dcpu:queue-p queue))
  (caddr queue))

(defun dcpu:queue-nth (n queue)
  (assert (dcpu:queue-p queue))
  (nth n (cadr queue)))

(defun dcpu:queue-push (val queue)
  (assert (dcpu:queue-p queue))
  (let ((val-cons (cons val nil)))
    (if (caddr queue)
      (setcdr (caddr queue) val-cons)
      (setcar (cdr  queue) val-cons))
    (setcar (cddr queue) val-cons)
    queue))

(defun dcpu:queue-pop (queue)
  (assert (dcpu:queue-p queue))
  (let ((val-cons (cadr queue)))
    (setcar (cdr queue) (cdr val-cons))
    (if (not (cadr queue))
      (setcar (cddr queue) nil))
    (car val-cons)))

(defun dcpu:queue-copy (queue)
  (assert (dcpu:queue-p queue))
  (let ((new-queue (dcpu:make-queue)))
    (setcar (cdr new-queue) (copy-sequence (dcpu:queue-head queue)) nil)
    (setcar (cddr new-queue) (last (cadr new-queue)))
    new-queue))

;; (eval-buffer)
;; (setq q (dcpu:make-queue))
;; (dcpu:queue-p q)
;; (dcpu:queue-push 'a q)
;; (dcpu:queue-push 'b q)
;; (dcpu:queue-copy q)
;; (progn (dotimes (i 5) (dcpu:queue-push i q)) q)
;; (dcpu:queue-nth 4 q)
;; (while (dcpu:queue-head q) (message "pop: %s" (dcpu:queue-pop q)))
;; (dcpu:queue-push 3 q)

(provide 'dcpu-util-queue)