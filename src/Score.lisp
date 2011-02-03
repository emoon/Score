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
	(loop for i from 0 to 31 collecting (setf (gethash (intern (format nil "R~d" i)) hash) t)) hash))

(defvar *ee-registers* (make-ee-registers-hash)) 

(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))

;;; si-type of opcode 
;;; expected input is 3 rt, rs, <immidiate> 

(defun :si-type (opcode arguments)
  (format t "~d~d" opcode arguments))

(defparameter *mips-instructions* 
   					   '((addi   #b001000 :si-type)
						 (addiu  #b001001 :lui-type)
						 (andi   #b001100 :si-type)
						 (daddi  #b011000 :si-type)
						 (daddiu #b011001 :lui-type)
						 (lui    #b001111 :lui-type)
						 (ori    #b001101 :ui-type)
						 (slti   #b001010 :si-type)
						 (sltiu  #b001011 :ui-type)
						 (xori   #b001110 :ui-type)

						 ;; Load / store instructions

						 (lb  #b100000 :ls-type)
						 (lbu #b100100 :ls-type)
						 (ld  #b110111 :ls-type)
						 (ldl #b011010 :ls-type)
						 (ldr #b011011 :ls-type)
						 (lwl #b100010 :ls-type)
						 (lh  #b100000 :ls-type)
						 (lhu #b100001 :ls-type)
						 (lw  #b100011 :ls-type)
						 (lq  #b011110 :ls-type)
						 (lwl #b100010 :ls-type)
						 (lwr #b100110 :ls-type)
						 (lwu #b100111 :ls-type)
						 (sb  #b101000 :ls-type)
						 (sd  #b111111 :ls-type)
						 (sdl #b101100 :ls-type)
						 (sdr #b101101 :ls-type)
						 (sh  #b101001 :ls-type)
						 (sw  #b101011 :ls-type)
						 (swl #b101010 :ls-type)
						 (swr #b101110 :ls-type)
						 (sq  #b011111 :ls-type)

						 ;; special 0

						 (add   #b100000 :s0-type)
						 (addu  #b100001 :s0-type)
						 (and   #b100100 :s0-type)
						 (dadd  #b101100 :s0-type)
						 (daddu #b101101 :s0-type)
						 (dsllv #b010100 :s0-type)
						 (dsrav #b010111 :s0-type)
						 (dsrlv #b010110 :s0-type)
						 (dsub  #b101110 :s0-type)
						 (dsubu #b101111 :s0-type)
						 (movn  #b001011 :s0-type)
						 (movz  #b001010 :s0-type)
						 (nor   #b100111 :s0-type)
						 (or    #b100101 :s0-type)
						 (sllv  #b000100 :s0-type)
						 (slt   #b101010 :s0-type)
						 (sltu  #b101011 :s0-type)
						 (srav  #b000111 :s0-type)
						 (srlv  #b000110 :s0-type)
						 (sub   #b100010 :s0-type)
						 (subu  #b100011 :s0-type)
						 (xor   #b100110 :s0-type)

						 ;; special 1
						
						 (dsll   #b111000 :s1-type)
						 (dsll32 #b111100 :s1-type)
						 (dsra   #b111011 :s1-type)
						 (dsra32 #b111111 :s1-type)
						 (drsl   #b111010 :s1-type)
						 (drsl32 #b111110 :s1-type)
						 (sll    #b000000 :s1-type)
						 (sra    #b000011 :s1-type)
						 (srl    #b000010 :s1-type)

						 ;; branch

						 (beq  #b000100 :b0-type)
						 (beql #b000100 :b0-type)
						 (bne  #b000101 :b0-type)
						 (bnel #b010101 :b0-type)
						 (bgez    #b00001 :b1-type)
						 (bgezal  #b10001 :b1-type)
						 (bgezall #b10011 :b1-type)
						 (bgezl   #b00011 :b1-type)
						 (bltz    #b00000 :b1-type)
						 (bltzal  #b10000 :b1-type)
						 (bltzall #b10010 :b1-type)
						 (bltzl	  #b00010 :b1-type)
						 (bgtz  #b000111 :b2-type)
						 (bgtzl #b010111 :b2-type)
						 (blez  #b000110 :b2-type)
						 (blezl #b010110 :b2-type)

						 ;; jumps

						 (j #b00010 :j0-type)
						 (jal #b00011 :j0-type)
						 (jalr #b001001 :j1-type)
						 (jr #b001000 :j2-type)

						 ;; move hi/lo registers

						 (mfhi #b010000 :mfhl-type)
						 (mflo #b010010 :mfhl-type)
						 (mthi #b010000 :mthl-type)
						 (mtlo #b010010 :mthl-type)

						 ;; multiply / divide

						 (mult  #b011000 :md-type)
						 (multu #b011001 :md-type)
						 (div  #b011010 :md-type)
						 (divu #b011011 :md-type)

						 ;; sync

						 (sync   #b001111 :sync-type)
						 (sync.l #b001111 :sync-type)
						 (sync.p #b101111 :sync-type)

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









