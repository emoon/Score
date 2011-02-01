;;; This function will parse a whole score file to lists to be processed  
;;; We might want to track line/similar things for better error reporting later

(defun read-score-file (filename)
	(with-open-file (stream filename)
		(let ((end (gensym end)))
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

(def-mips-instructions '((addi   #b001000 :si-type)
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

						 ;; multiply

						 (mult  #b011000 :mul-type)
						 (multu #b011000 :mul-type)

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


