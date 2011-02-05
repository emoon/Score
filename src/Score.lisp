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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementing the mips (r5900 assembler here)
;;;
;;; Loots of testing / hacking around currently
;;;

(defmacro make-ee-registers-hash ()
  (let ((hash (make-hash-table :test #'eq)))
		(loop for i from 0 to 31 collecting (setf (gethash (intern (format nil "R~d" i)) hash) i)) hash))

(defparameter *ee-registers* (make-ee-registers-hash)) 

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))

;;; write value to the stream

(defun write-value (stream value byte-count)
	(format t "Writing value #x~x" value)
  (loop for i from 0 to (1- byte-count)
	  do (write-byte (ldb (byte 8 (* 8 i)) value) stream)))

; info about how to encode the function

(defstruct encoding inst spec rd rs rt imm func)

;; binds and valdiats rt and rs

(defun validate-rt-rs (arguments)
	(let ((rt (gethash (nth 1 arguments) *ee-registers*)) 
			  (rs (gethash (nth 2 arguments) *ee-registers*)))
		(when (not rt)
			(error (format nil "Invalid register name for rt ~s" (nth 1 arguments))))
		(when (not rs)
			(error (format nil "Invalid register name for rs ~s" (nth 2 arguments))))
		(values rt rs)))

;; binds and valdiats rd, rs and rt 

(defun validate-rd-rt-rs (arguments)
	(let ((rd (gethash (nth 1 arguments) *ee-registers*)) 
			  (rs (gethash (nth 2 arguments) *ee-registers*))
			  (rt (gethash (nth 3 arguments) *ee-registers*)))
		(when (not rd)
			(error (format nil "Invalid register name for rd ~s" (nth 1 arguments))))
		(when (not rs)
			(error (format nil "Invalid register name for rs ~s" (nth 2 arguments))))
		(when (not rt)
			(error (format nil "Invalid register name for rt ~s" (nth 3 arguments))))
		(values rd rs rt)))


;;; instruction type 0
;;;
;;; <operand> <register>, <register>, <immediate>
;;;

(defun inst-type0 (stream instruction arguments enc)
	(when (not (equal (length arguments) 4))
		(error (format nil "Expected 4 arguments si-type <operand> rt, rs/base, <immediate> was given ~{~a~^, ~}" arguments)))
	(multiple-value-bind (rt rs) (validate-rt-rs arguments)
		(when (not (numberp (nth 3 arguments)))
			(error (format nil "Invalid immediate ~s" (nth 3 arguments))))
		(write-value stream (logior (ash instruction (encoding-inst enc)) 
																(ash rs (encoding-rs enc))
																(ash rt (encoding-rt enc))
																(ash (nth 3 arguments) (encoding-imm enc))) 4)))

;;; instruction type 1
;;; 
;;; <operand> <register>, <register>, <register> 
;;;

(defun inst-type1 (stream instruction special-value arguments enc) 
	(when (not (equal (length arguments) 4))
		(error (format nil "Expected 4 arguments si-type <operand> rd, rs, rt, was given ~{~a~^, ~}" arguments)))
	(multiple-value-bind (rd rt rs) (validate-rd-rt-rs arguments)
		(write-value stream (logior (ash special-value (encoding-spec enc)) 
																(ash rs (encoding-rs enc))
																(ash rt (encoding-rt enc))
																(ash rd (encoding-rd enc))
																(ash instruction (encoding-inst enc))) 4)))

;;; instruction type 2
;;; 
;;; <operand> <register>, <immediate> 
;;;

(defun inst-type2 (stream instruction arguments enc)
	(when (not (equal (length arguments) 3))
		(error (format nil "Expected 3 arguments si-type <operand> rt, rs/base, <immediate> was given ~{~a~^, ~}" arguments)))
	(let (rs (gethash (first arguments) *ee-registers*))
		(when (not rs)
			(error (format nil "Invalid register name for rt ~s" (first arguments))))
		(when (not (numberp (nth 2 arguments)))
			(error (format nil "Invalid immediate ~s" (nth 2 arguments))))
		(write-value stream (logior (ash instruction (encoding-inst enc)) 
																(ash rs (encoding-rs enc))
																(ash (nth 2 arguments) (encoding-imm enc))) 4)))

;;; instruction type 3
;;; 
;;; <operand> <immediate> 
;;;

(defun inst-type3 (stream instruction arguments enc)
	(when (not (equal (length arguments) 2))
		(error (format nil "Expected 2 arguments <operand> <immediate> was given ~{~a~^, ~}" arguments)))
	(when (not (numberp (nth 1 arguments)))
		(error (format nil "Invalid immediate ~s" (nth 1 arguments))))
	(write-value stream (logior (ash instruction (encoding-inst enc)) 
															(ash (nth 1 arguments) (encoding-imm enc))) 4))

;;; instruction type 4
;;;
;;; <operand> <register>, <register>
;;;

(defun inst-type4 (stream instruction arguments enc)
	(when (not (equal (length arguments) 3))
		(error (format nil "Expected 3 arguments si-type <operand> rt, rs/base, <immediate> was given ~{~a~^, ~}" arguments)))
	(multiple-value-bind (rt rs) (validate-rt-rs arguments)
		(write-value stream (logior (ash instruction (encoding-inst enc)) 
																(ash rs (encoding-rs enc))
																(ash rt (encoding-rt enc))) 4)))

;;; instruction type 5
;;; 
;;; <operand> <register>
;;;

(defun inst-type5 (stream instruction arguments enc)
	(when (not (equal (length arguments) 2))
		(error (format nil "Expected 2 arguments si-type <operand> rt, rs/base, <immediate> was given ~{~a~^, ~}" arguments)))
	(let (rt (gethash (first arguments) *ee-registers*))
		(when (not rt)
			(error (format nil "Invalid register name for rt ~s" (first arguments))))
		(write-value stream (logior (ash instruction (encoding-inst enc)) 
																(ash rt (encoding-rt enc))) 4)))
;;; instruction type 6
;;; 
;;; <operand> 
;;;

(defun inst-type6 (stream instruction arguments enc)
	(when (not (equal (length arguments) 1))
		(error (format nil "Expected 1 arguments si-type <operand> rt, rs/base, <immediate> was given ~{~a~^, ~}" arguments)))
		(write-value stream ((ash instruction (encoding-inst enc))) 4))

;;; List over all ee - instructions

(defun make-ee-instructions (enc opcode-list) 
	(loop for i in opcode-list do 
		(setf (gethash (first i) *mips-instructions*) (list (second i) enc))))

;;; instruction encoding

(defun encode-ee-instruction (stream instruction)
	(let ((instruction-info (gethash (first instruction) *mips-instructions*)))
		(when (not instruction-info)
			(error (format nil "Invalid instruction name ~{~a~^, ~}" instruction)))
		(funcall (encoding-func (second instruction-info)) stream (first instruction-info) instruction (second instruction-info))))

(defparameter *mips-instructions* (make-hash-table :test #'eq))

;;
;; Type 0 instructions 
;; <operand> <register>, <register>, <immediate>
;;

(make-ee-instructions (make-encoding :inst 26 :rs 21 :rt 16 :imm 0 :func #'inst-type0)
											'((addi #b001000) (addiu #b001001) (andi #b001100) (daddi #b011000) (daddiu #b011001)
											  (lui  #b001111) (ori   #b001101) (slti #b001010) (sltiu #b001011) (xori   #b001110)

												;; load and store instructions
 												
												(lb  #b100000) (lbu #b100100) (ld  #b110111) (ldl #b011010) (ldr #b011011)
												(lwl #b100010) (lh  #b100000) (lhu #b100001) (lw  #b100011) (lq  #b011110)
												(lwl #b100010) (lwr #b100110) (lwu #b100111)
												(sb  #b101000) (sd  #b111111) (sdl #b101100) (sdr #b101101) (sh  #b101001)
												(sw  #b101011) (swl #b101010) (swr #b101110) (sq  #b011111)

												;; some branches
												
											  (beq  #b000100) (beql #b000100) (bne  #b000101) (bnel #b010101)))

;;
;; Type 0 instructions (shift) (encoding actually uses rd, rt but we use rt, rs as as it looks the same at callsite
;;

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 16 :rt 11 :imm 5 :func #'inst-type1)
											'((dsll   #b111000) (dsll32 #b111100) (dsra   #b111011) 
												(dsra32 #b111111) (drsl   #b111010) (drsl32 #b111110)
										    (sll    #b000000) (sra    #b000011) (srl    #b000010)))

;;
;; Type 1 instructions 
;;; <operand> <register>, <register>, <register> 
;;

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 21 :rt 16 :rd 11 :func #'inst-type1)
											'((add   #b100000) (addu  #b100001) (and   #b100100) (dadd  #b101100) (daddu #b101101) 
												(dsllv #b010100) (dsrav #b010111) (dsrlv #b010110) (dsub  #b101110) (dsubu #b101111)
												(movn  #b001011) (movz  #b001010) (nor   #b100111) (or    #b100101)
												(sllv  #b000100) (slt   #b101010) (sltu  #b101011) (srav  #b000111) (srlv  #b000110)
												(sub   #b100010) (subu  #b100011) (xor   #b100110)))

;;
;; Type 2 instructions 
;; <operand> <register>, <immediate> 
;;

(make-ee-instructions (make-encoding :spec 1 :inst 16 :rs 21 :imm 0 :func #'inst-type2)
											'((bgez #b00001) (bgezal #b10001) (bgezall #b10011) (bgezl #b00011)
						 						(bltz #b00000) (bltzal #b10000) (bltzall #b10010) (bltzl #b00010)))

(make-ee-instructions (make-encoding :spec 1 :inst 16 :rs 21 :imm 0 :func #'inst-type2)
											'((bgtz #b000111) (bgtzl #b010111) (blez #b000110) (blezl #b010110)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 21 :func #'inst-type2)
											'((jr #b001000))) 

;;
;; Type 3 instructions 
;;; <operand> <immediate> 
;;

(make-ee-instructions (make-encoding :inst 26 :imm 0 :func #'inst-type3)
											'((j #b00010) (jal #b00011)))

;;
;; Type 4 instructions 
;; <operand> <register>, <register>
;;

(make-ee-instructions (make-encoding :rs 21 :rt 11 :func #'inst-type4) 
											'((jalr #b001001)))

(make-ee-instructions (make-encoding :rs 21 :rt 16 :func #'inst-type4) 
						 				  '((mult #b011000) (multu #b011001) 
												(div  #b011010) (divu #b011011)))

;;
;; Type 5 instructions 
;;; <operand> <register>
;;

(make-ee-instructions (make-encoding :rt 11 :func #'inst-type5) 
											 '((mfhi #b010000) (mflo #b010010)))

(make-ee-instructions (make-encoding :rt 21 :func #'inst-type5) 
											 '((mthi #b010000) (mtlo #b010010)))

;;
;; Type 6 instructions 
;;; <operand> 
;;

(make-ee-instructions (make-encoding :inst 0 :func #'inst-type6) 
											 '((sync #b001111) (sync.l #b001111) (sync.p #b101111)

(defparameter *mips-instructions-temp* 
				   '(

						 ;; trap

						 (teq #b110100 :t-type)
						 (tge #b110000 :t-type)
						 (tlt #b110010 :t-type)
						 (tne #b110110 :t-type)
						 (teqi  #b010000 :ti-type)
						 (tgei  #b110001 :ti-type)
						 (tgeiu #b110000 :ti-type)
						 (tlti  #b111010 :ti-type)
						 (tltiu #b111010 :ti-type)
						 (tnei  #b110010 :ti-type)

						 ;; misc

						 (pref #b110011 :pref-type)
						 (syscall #b001100 :syscall-type)

						 ;; MMI instructions

						 (paddb  #b01000 :mmi0-type)
						 (paddh  #b00100 :mmi0-type)
						 (paddsb #b11000 :mmi0-type)
						 (paddsh #b10100 :mmi0-type)
						 (paddsw #b11000 :mmi0-type)
						 (paddw  #b00100 :mmi0-type)
						 (pcgtb  #b01010 :mmi0-type)
						 (pcgth  #b00110 :mmi0-type)
						 (pcgtw  #b00100 :mmi0-type)
						 (pext5  #b11110 :mmi0-type)
						 (pextlb #b11010 :mmi0-type)
						 (pextlh #b10110 :mmi0-type)
						 (pextlw #b10010 :mmi0-type)
						 (pmaxh  #b00111 :mmi0-type)
						 (pmaxw  #b00011 :mmi0-type)
						 (ppac5  #b11111 :mmi0-type)
						 (ppacb  #b11011 :mmi0-type)
						 (ppach  #b10111 :mmi0-type)
						 (ppacw  #b10011 :mmi0-type)
						 (psubb  #b01001 :mmi0-type)
						 (psubh  #b00101 :mmi0-type)
						 (psubsb #b11001 :mmi0-type)
						 (psubsh #b10101 :mmi0-type)
						 (psubsw #b10001 :mmi0-type)
						 (psubw  #b00001 :mmi0-type)

						 (pabsh  #b00101 :mmi1-type)
						 (pabsw  #b00001 :mmi1-type)
						 (paddub #b11000 :mmi1-type)
						 (padduh #b10100 :mmi1-type)
						 (padduw #b10000 :mmi1-type)
						 (padsbh #b00100 :mmi1-type)
						 (pceqb  #b01010 :mmi1-type)
						 (pceqh  #b00110 :mmi1-type)
						 (pceqw  #b00010 :mmi1-type)
						 (pextub #b11010 :mmi1-type)
						 (pextuh #b10110 :mmi1-type)
						 (pextuw #b10010 :mmi1-type)
						 (pminh  #b00111 :mmi1-type)
						 (pminw  #b00011 :mmi1-type)
						 (psubub #b11001 :mmi1-type)
						 (psubuh #b10101 :mmi1-type)
						 (psubuw #b10001 :mmi1-type)
						 (qfsrv  #b11011 :mmi1-type)

						 (pand   #b10010 :mmi2-type)
						 (pcpyld #b01110 :mmi2-type)
						 (pdivbw #b11101 :mmi2-type)
						 (pexeh  #b11010 :mmi2-type)
						 (pexew  #b11110 :mmi2-type)
						 (phmadh #b10001 :mmi2-type)
						 (phmsbh #b10101 :mmi2-type)
						 (pinth  #b01010 :mmi2-type)
						 (pmaddh #b10000 :mmi2-type)
						 (pmaddw #b00000 :mmi2-type)
						 (pmfhi  #b01000 :mmi2mh-type)
						 (pmflo  #b01001 :mmi2mh-type)
						 (pmsubh #b10100 :mmi2-type)
						 (pmsubw #b00100 :mmi2-type)
						 (pmulth #b11100 :mmi2-type)
						 (pmultw #b01100 :mmi2-type)
						 (prevh  #b11011 :mmi2n-type)
						 (prot3w #b11111 :mmi2n-type)
						 (psllvw #b00010 :mmi2-type)
						 (psrlvw #b00011 :mmi2-type)
						 (pxor   #b01001 :mmi2-type)

						 (pmfhl.lh  #b110011 :pmfhl-type)
						 (pmfhl.lw  #b110000 :pmfhl-type)
						 (pmfhl.sh  #b110100 :pmfhl-type)
						 (pmfhl.slw #b110010 :pmfhl-type)
						 (pmfhi.uw  #b110001 :pmfhl-type)

						 (pmthi		#b10000 :pmthi-type)
						 (pmthi.lw	#b10000 :pmthi.lw-type)
						 (pmtlo		#b10001 :pmtlo-type)

						 (pcpyh  #b11001 :mmi3-type)
						 (pcpyud #b01110 :mmi3-type)
						 (pdivw  #b01101 :pdiv-type)
						 (pdivuw #b01001 :pdiv-type)
						 (pexch  #b11010 :mmi3n-type)
						 (pexcw  #b11110 :mmi3n-type)
						 (pinteh #b01010 :mmi3-type)
						 (pmadduw #b00000 :mmi3-type)

						 (psllh #b111100 :psll-type)
						 (psllw #b111101 :psll-type)
						 (psravw #b111101 :psll-type)
						 (psrlh #b111101 :psll-type)
						 (psrlvw #b111101 :psll-type)
						 (psrlw #b111101 :psll-type)
					
						 ;; Float ops

						 (abs.s   #b000101 :f0-type)
						 (add.s   #b000000 :f1-type)
						 (adda.s  #b000000 :f2-type)
						 (div.s   #b000011 :ft-type)
						 (lwc1    #b110001 :ls-type)
						 (madd.s  #b000000 :f1-type)
						 (madda.s #b000000 :f2-type)
						 (max.s   #b000000 :f1-type)
						 (min.s   #b000000 :f1-type)
						 (msub.s  #b000000 :f1-type)
						 (msuba.s #b000000 :f2-type)
						 (mul.s   #b000000 :f1-type)
						 (mula.s  #b000000 :f2-type)
						 (suba.s  #b000000 :f2-type)
						 (swc1    #b111001 :ls-type)

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









