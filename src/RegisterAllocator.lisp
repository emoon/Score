
;;;
;;; Register Allocator 
;;; 


; Current instruction - Keeps track of the operation and witch registers that is used


;(defpackage :score-reg-allocator;  :use :common-lisp)

; (in-package :score-reg-allocato;)

;defstruct instruction registers operand type label)
;defstruct tile subtiles)

;;;
;;; Configuration for the allocator
;;;
;;; The Allocator must support a whole range of features
;;;
;;; General:
;;;
;;; 1. Configurable on a per scope-level using (rlet..) to make it possible 
;;; 2. Possible to assign names to specific registers directly
;;; 3. Configure which registers that can be used at a given time
;;; 4. The allocator needs to know which registers that are volatile/non-volatile
;;; 5. Some registers may be fully reserved so (k1, k2 on mips) but should be good to have them in
;;;    the allocator to be able to track stats (in case we were able to use them, how much better would the code be etc)
;;;
;;; For 680x0:
;;;
;;; 1. Ability in user code to say if register is address or data (data is default)
;;; 2. It's up to the user to specifiy correct register type depending on the operation (address for mem read/write)
;;; 3. Some operations can be done with address registers (add, sub) allocator should be able to exose that
;;; 4. When running out of data registers spil to address registers (if availible) otherwise stack
;;; 5. in the rlet statement it sholud be nice if you don't need to specifiy address or data and let the
;;;    allocator deal with it (that way for example, add.l an, an can be used otherwise the allocator may be too
;;;    restricted given the room the user has let 
;;;

(defstruct cblock label instructions entry-state exit-state predecessors)


; (defun convert-to-ssa-form (code))

;;;
;;; First we need to build a control flow graph over the code
;;;


;;;
;;; This code expects a 'raw' list of assembly instructions with lables and instructions only like
;;;
;;; (
;;;  .test
;;;    (add test 0 2)
;;;    (bne .test)
;;;    (bra .foo)
;;;    (add test tst)
;;; .foo
;;;    (mul 0 1 2)
;;;    (bra.s .foo)
;;; )

(defun build-basic-blocks (code)
  (declare (optimize debug))
  (let ((block-list '())
  		(current-block (make-cblock :instructions '())))
    (loop for i in code do
      (if (symbolp i) 
      	; if i is a symbol we threat it like a label
      	(progn 
      	  (format t "there ~%")
      	  (setf block-list (append block-list (list current-block)))
      	  (setf current-block (make-cblock :label i)))
      	(progn 
      	  (format t "here ~%")
      	  (setf (cblock-instructions current-block) (append (cblock-instructions current-block) (list i)))))) 
 	(setf block-list (append block-list (list current-block))) block-list))


;;;
;;; Just some test-data for the code above
;;;

(defun test-code ()
  (build-basic-blocks '(.test
  						 (add 2 2 2)
  						 (bne.s .test)
  						 (add a b c)
  						 (bra.s foo)
  						 (nop)
  						.foo
  						 (adding 2 3 )
  						 (nop)
  						 (bne.s .foo)
  						 (rts))))

; (defun build-cfg (code) )



