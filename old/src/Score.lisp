

;

(defun read-score-file (filename)
  (with-open-file (stream filename)
    (let ((end (gensym "end")))
      (loop for i = (read stream nil end)
  		while (not (eq i end)) collect i)))) 

; test code

(defparameter *test-code* '(def-ee-fun test-ee-code (test1) (addi loop-counter test1 0)
   			    (:loop)
   			      (addi loop-counter loop-counter -1)
   			      (bne r0 loop-counter :loop)
   			      (addi r0 r0 0)
			    (jr r31)))

(defparameter *test-code-col* '(def-ee-fun set-col (temp)
						       (lui color-reg #x1200)
						       (ori color-reg color-reg #x00e0)
						       (ori color r0 #xff)
						       (sw color color-reg 0)  
						       (jr r31)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun send-data (buffer length)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (let ((socket (usocket:socket-connect "192.168.0.8" 1339 :element-type '(unsigned-byte 8))))
	(when socket
      (loop for i from 0 to (- length 1) do
	    (write-byte (aref buffer i) (usocket:socket-stream socket)))
	  (force-output (usocket:socket-stream socket))
	  (usocket:socket-close socket))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-score-code (input-code)
  (let ((code input-code))
     ; fix branches
     (setf code (fixup-local-branches code))
     ; allocate registers
     (setf code (allocate-registers code))
     ;(format t "~{~a~^, ~}~%" code)
     (with-open-file (stream "c:/temp/out.bin" 
                              :direction :output 
                              :if-exists :overwrite
                              :element-type '(unsigned-byte 8))
        (loop for i in (nthcdr 3 code) do
	  (format t "~{~a~^, ~}~%" i)
          (encode-ee-instruction stream i))))
  
  ; read back the file 
  (format t "test~%")
  (with-open-file (stream "c:/temp/out.bin" :direction :input :element-type '(unsigned-byte 8))
    (format t "test 2~%")
    (let* ((length (file-length stream))
	   (code (make-array length :element-type '(unsigned-byte 8) )))
	   (format t "file size ~d~%" length)
      (loop for i from 0 to (- length 1) do
      	(format t "reading ~d~%" i)
	    (setf (aref code i) (read-byte stream))) 
      ; send the shit!
	  (send-data code length))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile a score function and output a binary blob
;;

(defun build-score-file (filename)
  (let (code (read-score-file filename))
    (compile-score-code code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fixes up local branches on labels
;;;

(defun fixup-local-branches (code-temp)
  ; "Fixes up local branches on lables. assumes that the code is in correct format and has already been paresed/error checked"
  ; first pass is to find all labels and also to make sure that we don't have labels more than once
  ; we also skip the function defenition here
  (let ((label-table (make-hash-table :test #'eq))
	(offset 0)
	(code (copy-list code-temp))
        (new-code))
    (loop for i in (nthcdr 3 code) do 
       ; ok, this might not be the best approach but should be fine. If instruction wasn't found we assume that it's a label
       (if (gethash (car i) *mips-instructions*)
	  (incf offset)
	  (progn
	    (when (gethash (car i) label-table)
	      (error (format nil "Label ~a found more than once in ~a" (car i) (car code))))
	      (setf (gethash (car i) label-table) offset))))
    (setf offset 0)
    ; append the function def
    (setf new-code (append new-code (subseq code 0 3))) 
    (loop for i in (nthcdr 3 code) do
       ; ths may actually back fire later because we are looking for branch instructions in the func declaration as well
       (unless (gethash (car i) label-table)
	 (incf offset)
	  ; lets see if instruction is a branch
         (if (find (car i) '(beq beql bne bnel bgtz bgtzl blez blezl))
	   (progn
  	     (let ((code-offset (gethash (car (last i)) label-table)))
	       (unless code-offset 
	          (error (format nil "Unable to find ~a label" (car (last i)))))
	       (setf new-code (append new-code (list (append (nbutlast i) (list (- code-offset offset))))))))
	   (setf new-code (append new-code (list i))))))
    (values new-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementing the mips (r5900 assembler here)
;;;
;;; Loots of testing / hacking around currently
;;;


; info about how to encode the function

