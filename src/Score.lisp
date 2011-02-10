;;; This function will parse a whole score file to lists to be processed  
;;; We might want to track line/similar things for better error reporting later

(defun read-score-file (filename)
	(with-open-file (stream filename)
		(let ((end (gensym "end")))
			(loop for i = (read stream nil end)
				while (not (eq i end)) collect i)))) 

;;; Very rought testing right now

(defun test-ee-parse ()
	(let ((forms (read-score-file "../tests/ps2/test0.score")))
		;; forms has the lists of the 
		(when forms
			(let ((inner (car forms)))
				; first entry here should be a def-ee-fun
				; notice will be proper parsing here, this is just for test
				(when (eql (car inner) 'def-ee-fun)
					(loop for i in (nthcdr 3 inner) do	; skip the function name and assume no in-parameters
						; here we can start pushing out the instructions but just printing now
						(print i)))))))

;;;
;;;
;;; Test code
;;;

(defun test-write-instructions (instructions)
	(with-open-file (stream "c:/temp/out.bin" 
													 :direction :output 
													 :if-exists :overwrite
													 :element-type '(unsigned-byte 8))
		(loop for i in instructions do
			(encode-ee-instruction stream i))))
;;
;;
;;

(defun gen-rand-inst () 
	 (with-open-file (stream "c:/temp/code.txt" :direction :output :if-exists :overwrite)
		 (loop for i from 0 to 180 do
		 		(let* ((instruction (aref *instruction-vector* i))
							 (instruction-info (gethash instruction *mips-instructions*))
							 (type (encoding-type (second instruction-info))))
					(case type 
						(0 (format stream "(~(~s~) r2 r3 12)~%" instruction))   
						(1 (format stream "(~(~s~) r2 r3 r4)~%" instruction))   
						(2 (format stream "(~(~s~) r2 12)~%" instruction))   
						(3 (format stream "(~(~s~) 12)~%" instruction))   
						(4 (format stream "(~(~s~) r2 r3)~%" instruction))
						(5 (format stream "(~(~s~) r2)~%" instruction))   
						(6 (format stream "(~(~s)~)~%" instruction)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementing the mips (r5900 assembler here)
;;;
;;; Loots of testing / hacking around currently
;;;

(defmacro make-ee-registers-hash ()
  (let ((hash (make-hash-table :test #'eq)))
		(loop for i from 0 to 31 collecting (setf (gethash (intern (format nil "R~d" i)) hash) i)) hash))

(defparameter *ee-registers* (make-ee-registers-hash)) 
(defparameter *fpu-registers* nil) 
(defparameter *register-type-lut* '(rs *ee-registers* rt *ee-registers* rd *ee-registers*
																		fs *fpu-registers* ft *fpu-registers* fd *fpu-registers*))

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))

;;; write value to the stream

(defun write-value (stream value byte-count)
	(format t "Writing value #x~x" value)
  (loop for i from 0 to (1- byte-count)
	  do (write-byte (ldb (byte 8 (* 8 i)) value) stream)))

; info about how to encode the function

(defstruct encoding inst (spec 0) rd rs rt imm (fixed 0) fs ft fd (type 0) func)

;;
;; encode instruction
;;

(defun encode-ee-instruction (stream instruction)
	(let ((instruction-info (gethash (first instruction) *mips-instructions*)))
		(when (not instruction-info)
			(error (format nil "Invalid instruction name ~{~a~^, ~}" instruction)))
		(funcall (encoding-func (second instruction-info)) stream (first instruction-info) instruction (second instruction-info))))

;;
;; builds the diffrent instruction types based on the inst-layout data
;;

(defmacro def-inst-type (name inst-layout)
 `(progn
		(defun ,name (stream instruction arguments enc)
			(pprint enc)
			(pprint instruction)
			(let ((code 0))
				,(when (first inst-layout)
					;; Generate the let for all the registers
					`(let ,(loop for i in (first inst-layout) for c from 1 collecting 
						`(,i (gethash (nth ,c arguments) ,(getf *register-type-lut* i))))
						;; error checking for all the registers
					,@(loop for i in (first inst-layout) for c from 1 collect
						`(when (not ,i) 
							(error (format nil "Invalid register name ~s" (nth ,c arguments)))))
				;; encode the registers
				(setf code (logior ,@(loop for i in (first inst-layout) collecting 
					`(ash ,i ,(read-from-string (format nil "(ENCODING-~A ENC)" i))))))))
			;; if we have a length of 2 in the list then we have imm value also and it needs to be a number 
			,(when (eql (length inst-layout) 2)
				(let ((imm-arg (+ (length (first inst-layout)) 1)))
					;`(when (not (numberp (nth ,imm-arg arguments)))
					;	(error (format nil "Invalid immediate ~s" (nth ,imm-arg arguments))))
						;; encode the immediate value
					`(setf code (logior code (ash (logand (nth ,imm-arg arguments) ,(second inst-layout))  (encoding-imm enc))))))
			(write-value stream (logior code 
																	(ash instruction (encoding-inst enc)) 
																	(ash (encoding-spec enc) 26) 
																	(encoding-fixed enc)) 4)))))

;; diffrent type of ee instructions

(def-inst-type inst-type0 ((rs rt) #x0000ffff)) 
(def-inst-type inst-type1 ((rd rt rs))) 
(def-inst-type inst-type2 ((rs) #x0000ffff)) 
(def-inst-type inst-type3 (() #x0000ffff)) 
(def-inst-type inst-type4 ((rt rs))) 
(def-inst-type inst-type5 ((rt))) 
; (def-inst-type inst-type6 (())) 

;; fpu instructions

(def-inst-type fpu-inst0 ((ft fs))) 
(def-inst-type fpu-inst1 ((ft fs fd))) 
(def-inst-type fpu-inst2 ((rt rs))) 

;;; List over all ee - instructions

(defparameter *instruction-vector* (make-array 200))
(defparameter *current-instruction* 0)

(defun make-ee-instructions (enc opcode-list) 
	(loop for i in opcode-list do 
		(setf (gethash (first i) *mips-instructions*) (list (second i) enc))
		(setf (aref *instruction-vector* *current-instruction*) (first i))
		(incf *current-instruction*)))

;;; instruction encoding

(defparameter *mips-instructions* (make-hash-table :test #'eq))

;;
;; Registers all instructions 
;;

(make-ee-instructions (make-encoding :inst 26 :rs 16 :rt 21 :imm 0 :type 0 :func #'inst-type0)
											'((addi #b001000) (addiu #b001001) (andi #b001100) (daddi #b011000) (daddiu #b011001)
											  (ori   #b001101) (slti #b001010) (sltiu #b001011) (xori   #b001110)
												(lb  #b100000) (lbu #b100100) (ld  #b110111) (ldl #b011010) (ldr #b011011)
												(lwl #b100010) (lh  #b100001) (lhu #b100101) (lw  #b100011) (lq  #b011110)
												(lwl #b100010) (lwr #b100110) (lwu #b100111)
												(sb  #b101000) (sd  #b111111) (sdl #b101100) (sdr #b101101) (sh  #b101001)
												(sw  #b101011) (swl #b101010) (swr #b101110) (sq  #b011111)
											  (beq  #b000100) (beql #b000100) (bne  #b000101) (bnel #b010101)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 16 :rt 11 :imm 5 :type 0 :func #'inst-type0)
											'((dsll   #b111000) (dsll32 #b111100) (dsra   #b111011) 
												(dsra32 #b111111) (drsl   #b111010) (drsl32 #b111110)
										    (sll    #b000000) (sra    #b000011) (srl    #b000010)))

(make-ee-instructions (make-encoding :inst 0 :spec 0 :rs 16 :rt 11 :imm 6 :type 0 :func #'inst-type0)
											'((teq #b110100) (tge #b110000) (tlt #b110010) (tne #b110110)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 21 :rt 16 :rd 11 :type 1 :func #'inst-type1)
											'((add   #b100000) (addu  #b100001) (and   #b100100) (dadd  #b101100) (daddu #b101101) 
												(dsllv #b010100) (dsrav #b010111) (dsrlv #b010110) (dsub  #b101110) (dsubu #b101111)
												(movn  #b001011) (movz  #b001010) (nor   #b100111) (or    #b100101)
												(sllv  #b000100) (slt   #b101010) (sltu  #b101011) (srav  #b000111) (srlv  #b000110)
												(sub   #b100010) (subu  #b100011) (xor   #b100110)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001000 :inst 6 :rs 21 :rt 16 :rd 11 :type 1 :func #'inst-type1)
											'((paddb  #b01000) (paddh  #b00100) (paddsb #b11000) (paddsh #b10100) (paddsw #b10000)
												(paddw  #b00000) (pcgtb  #b01010) (pcgth  #b00110) (pcgtw  #b00010) 
												(pextlb #b11010) (pextlh #b10110) (pextlw #b10010) (pmaxh  #b00111) (pmaxw  #b00011)
												(ppacb  #b11011) (ppach  #b10111) (ppacw  #b10011) (psubb  #b01001)
												(psubh  #b00101) (psubsb #b11001) (psubsh #b10101) (psubsw #b10001) (psubw  #b00001)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b101000 :inst 6 :rs 21 :rt 16 :rd 11 :type 1 :func #'inst-type1)
											'((paddub #b11000) (padduh #b10100) (padduw #b10000)
												(padsbh #b00100) (pceqb  #b01010) (pceqh  #b00110) (pceqw  #b00010) (pextub #b11010)
												(pextuh #b10110) (pextuw #b10010) (pminh  #b00111) (pminw  #b00011) (psubub #b11001)
												(psubuh #b10101) (psubuw #b10001) (qfsrv  #b11011)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001001 :inst 6 :rs 21 :rt 16 :rd 11 :type 1 :func #'inst-type1)
											'((pand   #b10010) (pcpyld #b01110) 
												(phmadh #b10001) (phmsbh #b10101) (pinth  #b01010) (pmaddh #b10000) (pmaddw #b00000)
												(pmsubh #b10100) (pmsubw #b00100) (pmulth #b11100)
												(pmultw #b01100) (psllvw #b00010) (psrlvw #b00011)
												(pxor   #b10011)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b101001 :inst 6 :rs 21 :rt 16 :rd 11 :type 1 :func #'inst-type1)
											'((pcpyud #b01110) (pinteh #b01010) (pmadduw #b00000) 
												(psllh #b111100) (psllw #b111101) (psravw #b111101) (psrlh #b111101) (psrlvw #b111101)
												(psrlw #b111101)))

(make-ee-instructions (make-encoding :spec #b011100 :inst 0 :rs 16 :rt 11 :rd 6 :type 1 :func #'inst-type1)
											'((psllh #b111100) (psllw #b111101) (psrlh #b111101) (psrlw #b111101)))

(make-ee-instructions (make-encoding :spec 1 :inst 16 :rs 21 :imm 0 :type 2 :func #'inst-type2)
											'((bgez #b00001) (bgezal #b10001) (bgezall #b10011) (bgezl #b00011)
						 						(bltz #b00000) (bltzal #b10000) (bltzall #b10010) (bltzl #b00010)))

(make-ee-instructions (make-encoding :spec 1 :inst 16 :rs 21 :imm 0 :type 2 :func #'inst-type2)
											'((bgtz #b000111) (bgtzl #b010111) (blez #b000110) (blezl #b010110)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 21 :imm 0 :type 2 :func #'inst-type2)
											'((jr #b001000))) 

(make-ee-instructions (make-encoding :spec 0 :inst 16 :rs 21 :imm 0 :type 2 :func #'inst-type2)
											'((teqi  #b010000 :ti-type) (tgei  #b110001 :ti-type) (tgeiu #b110000 :ti-type)
						 						(tlti  #b111010 :ti-type) (tltiu #b111010 :ti-type) (tnei  #b110010 :ti-type)))

(make-ee-instructions (make-encoding :spec 0 :inst 26 :rs 21 :imm 0 :type 2 :func #'inst-type2)
											'((pref #b110011)))

(make-ee-instructions (make-encoding :spec 0 :inst 26 :rs 16 :imm 0 :type 2 :func #'inst-type2)
											'((lui #b001111)))

(make-ee-instructions (make-encoding :inst 26 :imm 0 :type 3 :func #'inst-type3)
											'((j #b00010) (jal #b00011)))

(make-ee-instructions (make-encoding :inst 6 :imm 6 :type 3 :func #'inst-type3)
											'((syscall #b001100))) 

(make-ee-instructions (make-encoding :inst 0 :rs 21 :rt 11 :type 4 :func #'inst-type4) 
											'((jalr #b001001)))

(make-ee-instructions (make-encoding :inst 0 :rs 21 :rt 16 :type 4 :func #'inst-type4) 
						 				  '((mult #b011000) (multu #b011001) 
												(div  #b011010) (divu #b011011)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001000 :inst 6 :rs 16 :rt 11 :type 4 :func #'inst-type4)
											'((pext5  #b11110) (ppac5 #b11111)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001001 :inst 6 :rs 21 :rt 16 :type 4 :func #'inst-type4)
											'((pdivbw #b11101) (pexeh  #b11010) (pabsh #b00101) (pabsw #b00001) (pexew  #b11110)
												(pdivw  #b01101) (pdivuw #b01001)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b101001 :inst 6 :rs 16 :rt 11 :type 4 :func #'inst-type4)
											'((pcpyh  #b11001) (pexch  #b11010) (pexcw  #b11110)))

(make-ee-instructions (make-encoding :inst 0 :rt 11 :type 5 :func #'inst-type5) 
											 '((mfhi #b010000) (mflo #b010010)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001001 :inst 6 :rt 16 :type 5 :func #'inst-type5)
											 '((pmfhi  #b01000) (pmflo  #b01001)))

(make-ee-instructions (make-encoding :inst 0 :rt 21 :type 5 :func #'inst-type5) 
										   '((pmfhl.lh  #b110011) (pmfhl.lw #b110000) (pmfhl.sh #b110100)
												 (pmfhl.slw #b110010) (pmfhi.uw #b110001) (pmthi		#b10000)
												 (pmthi.lw	#b100000) (pmtlo		#b10001)))

; (make-ee-instructions (make-encoding :inst 0 :type 6 :func #'inst-type6) 
;											 '((sync #b001111) (sync.l #b001111) (sync.p #b101111)))

;;
;; fpu instructions
;;

; (make-ee-instructions (make-encoding :spec #b010001 :fix #x200000 :inst 0 :fs 11 :rd 6 :func #'fpu-type0)
; 											 '(

(defparameter *mips-instructions-temp* 
				   '(

						 ;; trap

						 ;; misc

						 ;; MMI instructions

						 ;; Float ops

						 (abs.s   #b000101)
						 (add.s   #b000000)
						 (adda.s  #b000000)
						 (div.s   #b000011)
						 (lwc1    #b110001)
						 (madd.s  #b000000)
						 (madda.s #b000000)
						 (max.s   #b000000)
						 (min.s   #b000000)
						 (msub.s  #b000000)
						 (msuba.s #b000000)
						 (mul.s   #b000000)
						 (mula.s  #b000000)
						 (suba.s  #b000000)
						 (swc1    #b111001)

						 (c.eq.s  #b011010 :fcmp-type)
						 (c.f.s   #b011000 :fcmp-type)
						 (c.le.s  #b011011 :fcmp-type)
						 (c.lt.s  #b011010 :fcmp-type)

						 (cvt.s.w #b100000 :fcon-type)
						 (cvt.w.t #b100100 :fcon-type)

						 (bc1f    #b000000 :fbranch-type)
						 (bc1fl   #b000010 :fbranch-type)
						 (bc1t    #b000001 :fbranch-type)
						 (bc1tl   #b000011 :fbranch-type)

						 (mov.s   #b000000 :fmov-type)
						 (mfc1    #b000000 :fftc1-type)
						 (mtc1    #b000100 :fftc1-type)

						 (rsqrt.s #b000000 :frsqrt-type)
						 (sqrt.s  #b000000 :fsqrt-type)

						 ;; cop-0 instructions (section non-functional but here to be implemented)

						 (bc0f    #b000000 :b0-type)
						 (bc0fl   #b000000 :b0-type)
						 (bc0t    #b000000 :b0-type)
						 (bc0tl   #b000000 :b0-type)

						 (cache   #b0000000 :cache-type)
						 (di	  #b1111001 :i-type)
						 (ei	  #b1111000 :i-type)
						 (eret	  #b0111000 :i-type)

						 (mfbpc   #b0000000 :mf-type)
						 (mfc0    #b0000000 :mf-type)
						 (mfdab   #b0000000 :mf-type)
						 (mfdabm  #b0000000 :mf-type)
						 (mfdvb   #b0000000 :mf-type)
						 (mfdvbm  #b0000000 :mf-type)
						 (mfiab   #b0000000 :mf-type)
						 (mfiabm  #b0000000 :mf-type)
						 (mfpc    #b0000000 :mf-type)
						 (mfps    #b0000000 :mf-type)
						 (mtc0    #b0000000 :mf-type)
						 (mtdab   #b0000000 :mf-type)
						 (mtdabm  #b0000000 :mf-type)
						 (mtdvb   #b0000000 :mf-type)
						 (mtdvbm  #b0000000 :mf-type)
						 (mtiab   #b0000000 :mf-type)
						 (mtiabm  #b0000000 :mf-type)

						 (tbpl    #b0000000 :tlb-type)
						 (tlbr    #b0000000 :tlb-type)
						 (tlwi    #b0000000 :tlb-type)
						 (tlwr    #b0000000 :tlb-type)))









