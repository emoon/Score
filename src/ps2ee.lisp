(defparameter *mips-instructions* (make-hash-table :test #'eq))
(defmacro make-ee-registers-hash (prefix)
  (let ((hash (make-hash-table :test #'eq)))
    (loop for i from 0 to 31 collecting (setf (gethash (intern (format nil "~S~d" prefix i)) hash) i)) hash))

(defparameter *ee-registers* (make-ee-registers-hash R))
(defparameter *fpu-registers* (make-ee-registers-hash F))
(defparameter *register-type-lut* '(rs *ee-registers* rt *ee-registers* rd *ee-registers*
                                    fs *fpu-registers* ft *fpu-registers* fd *fpu-registers*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implements a very basic register allocator for given code.
;;; Currently doesn't track depth of loops/etc so it's extremly naive but a start
;;; 
;;; This code expects to get a function with (expanded assembly) 

(defun allocate-registers (code)
  ; local function to get the register
 (let ((register-table (make-hash-table :test #'eq))
       (current-reg 4)
       (new-code))
  (flet ((get-param (reg)
    (cond
      ((gethash reg *mips-instructions*) reg)
      ((gethash reg *ee-registers*) reg)
      ((numberp reg) reg)
      ((gethash reg register-table) (gethash reg register-table))
      ; if we didn't find the register/value above we need to handle it here
      (t (progn 
	(let ((register (gethash reg register-table))
              (reg-name))
	  ; not found, assign it a register and return
	  (unless register
	    (setf reg-name (intern (format nil "R~d" current-reg)))
	    (setf (gethash reg register-table) reg-name))
	    (incf current-reg)
	  (values reg-name)))))))
      ; only handle 4 arguments right now
      (when (> (length (third code)) 4)
        (error (format nil "More than 4 arguments~%")))
      ; first handle the input registers which are r4 - r7
      (loop for i in (third code) for r in '(r4 r5 r6 r7) do
         (incf current-reg)
         (setf (gethash i register-table) r))
      ; insert the function header (not actually used but here for info)
      (setf new-code (append new-code (subseq code 0 3))) 
      ; loop all code and allocate/insert registers
      (loop for i in (nthcdr 3 code) do
        (setf new-code (append new-code (list (loop for c in i collect (get-param c))))))
      (values new-code))))

;;; write value to the stream

(defun write-value (stream value byte-count)
  (loop for i from 0 to (1- byte-count)
    do (write-byte (ldb (byte 8 (* 8 i)) value) stream)))

(defstruct encoding inst (spec 0) rd rs rt (imm 0) (fixed 0) fs ft fd func)

;; encode instruction

(defun encode-ee-instruction (stream instruction)
  (let ((instruction-info (gethash (first instruction) *mips-instructions*)))
    (when (not instruction-info)
      (error (format nil "Invalid instruction name ~{~a~^, ~}" instruction)))
    (funcall (encoding-func (second instruction-info)) stream (first instruction-info) instruction (second instruction-info))))

(defun make-ee-instructions (enc opcode-list)
  (loop for i in opcode-list do 
    (setf (gethash (first i) *mips-instructions*) (list (second i) enc))))

;; builds the diffrent instruction types based on the inst-layout data

(defmacro def-inst-type (name inst-layout)
  `(progn
     (defun ,name (stream instruction arguments enc)
       (declare (optimize debug))
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
					;  (error (format nil "Invalid immediate ~s" (nth ,imm-arg arguments))))
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
(def-inst-type inst-type6 (())) 

;; fpu instructions

(def-inst-type fpu-type0 ((ft fs) #x0000ffff)) 
(def-inst-type fpu-type1 ((fd fs ft))) 
(def-inst-type fpu-type2 ((fd fs))) 
(def-inst-type fpu-type3 ((rt fs))) 


;; Registers all instructions 

(make-ee-instructions (make-encoding :inst 26 :rs 16 :rt 21 :imm 0 :func #'inst-type0)
                      '((addi #b001000) (addiu #b001001) (andi #b001100) (daddi #b011000) (daddiu #b011001)
                        (ori #b001101) (slti #b001010) (sltiu #b001011) (xori   #b001110)
                        (lb  #b100000) (lbu #b100100) (ld  #b110111) (ldl #b011010) (ldr #b011011)
                        (lwl #b100010) (lh  #b100001) (lhu #b100101) (lw  #b100011) (lq  #b011110)
                        (lwl #b100010) (lwr #b100110) (lwu #b100111) (sb  #b101000) (sd  #b111111) 
                        (sdl #b101100) (sdr #b101101) (sh  #b101001) (sw  #b101011) (swl #b101010) 
                        (swr #b101110) (sq  #b011111)))

(make-ee-instructions (make-encoding :inst 26 :rs 21 :rt 16 :imm 0 :func #'inst-type0)
                      '((beq  #b000100) (beql #b000100) (bne  #b000101) (bnel #b010101)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 11 :rt 16 :imm 5 :func #'inst-type0)
                      '((dsll   #b111000) (dsll32 #b111100) (dsra   #b111011) 
                        (dsra32 #b111111) (drsl   #b111010) (drsl32 #b111110)
                        (sll    #b000000) (sra    #b000011) (srl    #b000010)))

(make-ee-instructions (make-encoding :inst 0 :spec 0 :rs 21 :rt 16 :imm 6 :func #'inst-type0)
                      '((teq #b110100) (tge #b110000) (tlt #b110010) (tne #b110110)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 16 :rt 21 :rd 11 :func #'inst-type1)
                      '((add   #b100000) (addu  #b100001) (and   #b100100) (dadd  #b101100) (daddu #b101101) 
                        (dsub  #b101110) (dsubu #b101111) (movn  #b001011) (movz  #b001010) (nor   #b100111) 
                        (or    #b100101) (slt   #b101010) (sltu  #b101011)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rs 21 :rt 16 :rd 11 :func #'inst-type1)
                      '((dsllv #b010100) (dsrav #b010111) (dsrlv #b010110) (sllv  #b000100) 
                        (srav  #b000111) (srlv  #b000110)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001000 :inst 6 :rs 16 :rt 21 :rd 11 :func #'inst-type1)
                      '((paddb  #b01000) (paddh  #b00100) (paddsb #b11000) (paddsh #b10100) (paddsw #b10000)
                        (paddw  #b00000) (pcgtb  #b01010) (pcgth  #b00110) (pcgtw  #b00010) 
                        (pextlb #b11010) (pextlh #b10110) (pextlw #b10010) (pmaxh  #b00111) (pmaxw  #b00011)
                        (ppacb  #b11011) (ppach  #b10111) (ppacw  #b10011) (psubb  #b01001)
                        (psubh  #b00101) (psubsb #b11001) (psubsh #b10101) (psubsw #b10001) (psubw  #b00001)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b101000 :inst 6 :rs 16 :rt 21 :rd 11 :func #'inst-type1)
                      '((paddub #b11000) (padduh #b10100) (padduw #b10000)
                        (padsbh #b00100) (pceqb  #b01010) (pceqh  #b00110) (pceqw  #b00010) (pextub #b11010)
                        (pextuh #b10110) (pextuw #b10010) (pminh  #b00111) (pminw  #b00011) (psubub #b11001)
                        (psubuh #b10101) (psubuw #b10001) (qfsrv  #b11011)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001001 :inst 6 :rs 16 :rt 21 :rd 11 :func #'inst-type1)
                      '((pand   #b10010) (pcpyld #b01110) 
                        (phmadh #b10001) (phmsbh #b10101) (pinth  #b01010) (pmaddh #b10000) (pmaddw #b00000)
                        (pmsubh #b10100) (pmsubw #b00100) (pmulth #b11100)
                        (pmultw #b01100) (pxor   #b10011)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001001 :inst 6 :rs 21 :rt 16 :rd 11 :func #'inst-type1)
                      '((psllvw #b00010) (psrlvw #b00011)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b101001 :inst 6 :rs 21 :rt 16 :rd 11 :func #'inst-type1)
                      '((pcpyud #b01110) (pinteh #b01010) (pmadduw #b00000) 
                        (psravw #b00011)))

(make-ee-instructions (make-encoding :spec #b011100 :inst 0 :rs 6 :rt 16 :rd 11 :func #'inst-type1)
                      '((psllh #b110100) (psllw #b111100) (psrlh #b110110) (psrlw #b111110)))

(make-ee-instructions (make-encoding :spec 1 :inst 16 :rs 21 :imm 0 :func #'inst-type2)
                      '((bgez #b00001) (bgezal #b10001) (bgezall #b10011) (bgezl #b00011)
                         (bltz #b00000) (bltzal #b10000) (bltzall #b10010) (bltzl #b00010)))

 (make-ee-instructions (make-encoding :spec 0 :inst 26 :rs 21 :imm 0 :func #'inst-type2)
                      '((bgtz #b000111) (bgtzl #b010111) (blez #b000110) (blezl #b010110)))

(make-ee-instructions (make-encoding :spec 0 :inst 26 :rs 21 :imm 0 :func #'inst-type2)
                      '((pref #b110011)))

(make-ee-instructions (make-encoding :spec 0 :inst 26 :rs 16 :imm 0 :func #'inst-type2)
                      '((lui #b001111)))

(make-ee-instructions (make-encoding :inst 26 :imm 0 :func #'inst-type3)
                      '((j #b00010) (jal #b00011)))

(make-ee-instructions (make-encoding :inst 0 :imm 6 :func #'inst-type3)
                      '((syscall #b001100))) 

(make-ee-instructions (make-encoding :inst 0 :rs 21 :rt 11 :func #'inst-type4) 
                      '((jalr #b001001)))

(make-ee-instructions (make-encoding :inst 0 :rs 16 :rt 21 :func #'inst-type4) 
                      '((mult #b011000) (multu #b011001) 
                        (div  #b011010) (divu #b011011)))

(make-ee-instructions (make-encoding :spec 1 :inst 16 :rs 21 :imm 0 :func #'inst-type2)
                      '((teqi #b01100) (tgei  #b01000) (tgeiu #b01001)
                        (tlti #b01010) (tltiu #b01011) (tnei  #b01110)))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001000 :inst 6 :rs 16 :rt 11 :func #'inst-type4)
                      '((pext5  #b11110) (ppac5 #b11111)))

(make-ee-instructions (make-encoding :spec #b011100 :imm 0 :fixed 0 :inst 0 :rs 16 :rt 21 :func #'inst-type4)
                      '((pdivbw #b11101001001 ) (pdivw  #b01101001001 ) (pdivuw #b01101101001 )))

(make-ee-instructions (make-encoding :spec #b011100 :fixed 0 :inst 0 :rs 16 :rt 11 :func #'inst-type4)
                      '((pexeh  #b11010001001 ) (pabsh #b00101101000 ) (pabsw #b00001101000 ) (pexew  #b11110001001 )))

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b101001 :inst 6 :rs 16 :rt 11 :func #'inst-type4)
                      '((pcpyh  #b11011) (pexch  #b11010) (pexcw  #b11110)))

(make-ee-instructions (make-encoding :inst 0 :rt 11 :func #'inst-type5) 
                       '((mfhi #b010000) (mflo #b010010)))

(make-ee-instructions (make-encoding :spec 0 :inst 0 :rt 21 :imm 0 :func #'inst-type5)
                      '((jr #b001000))) 

(make-ee-instructions (make-encoding :spec #b011100 :fixed #b001001 :inst 6 :rt 11 :func #'inst-type5)
                       '((pmfhi  #b01000) (pmflo  #b01001)))

(make-ee-instructions (make-encoding :spec #b011100 :imm 0 :inst 0 :rt 11 :func #'inst-type5) 
                       '((pmfhl.lh  #b00011110000) (pmfhl.lw #b00000110000) (pmfhl.sh #b00100110000)
                         (pmfhl.slw #b00010110000) (pmfhl.uw #b00001110000))) 

(make-ee-instructions (make-encoding :spec #b011100 :imm 0 :inst 0 :rt 21 :func #'inst-type5) 
                       '((pmthi #b01000101001) (pmthl.lw  #b00000110001) (pmtlo #b01001101001)))

(make-ee-instructions (make-encoding :inst 0 :func #'inst-type6) 
                      '((sync #b00000001111) (sync.l #b00000001111) (sync.p #b10000001111)))

;; fpu instructions

(make-ee-instructions (make-encoding :imm 0 :inst 26 :fs 21 :ft 16 :func #'fpu-type0)
                      '((lwc1 #b110001) (swc1 #b111001)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed #x2000000 :inst 0 :fs 11 :fd 6 :func #'fpu-type2)
                      '((abs.s #b000101) (cvt.w.t #b100100) (mov.s #b000110) (neg.s #b000111)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed #x2000000 :inst 0 :fs 16 :fd 11 :func #'fpu-type2)
                      '((adda.s #b011000) (madda.s #b011110) (msuba.s #b011111) (mula.s #b011010) (suba.s #b011001) 
                        (c.eq.s #b110010) (c.f.s   #b110000) (c.le.s  #b110110) (c.lt.s #b110100)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed #x2000000 :inst 0 :fs 16 :fd 6 :func #'fpu-type2)
                      '((sqrt.s #b000100)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed #x2800000 :inst 0 :fs 11 :fd 6 :func #'fpu-type2)
                      '((ctv.s.w #b100000)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed #x2000000 :inst 0 :ft 16 :fs 11 :fd 6 :func #'fpu-type1)
                      '((add.s  #b000000) (div.s #b000011) (madd.s  #b011100) (max.s #b101000) (min.s #b101001)
                        (msub.s #b011101) (mul.s #b000010) (rsqrt.s #b010110) (sub.s #b000001)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed #x1000000 :inst 16 :func #'inst-type3) 
                      '((bc1f #b00000) (bc1fl #b00010) (bc1t #b00001) (bc1tl #b00011)))

(make-ee-instructions (make-encoding :spec #b010001 :fixed 0 :rt 16 :fs 11 :inst 21 :func #'fpu-type3) 
                      '((mfc1 #b00000) (mtc1 #b00100)))

;; cop-0 instructions

(make-ee-instructions (make-encoding :spec #b010000 :fixed #x1000000 :inst 16 :func #'inst-type3) 
                      '((bc0f #b00000) (bc0fl #b00010) (bc0t #b00001) (bc0tl #b00011)))

(make-ee-instructions (make-encoding :spec #b101111 :rs 21 :imm 0 :inst 16 :func #'inst-type2) 
                      '((cache.ixin #b00111)   (cache.ixltg #b00000) (cache.ixstg #b00100)  (cache.ihin #b01011)
                        (cache.ifl #b01110)    (cache.ixldt #b00001) (cache.ixsdt #b00101)  (cache.bxlbt #b00010)
                        (cache.bxsbt #b00110)  (cache.bfh #b01100)   (cache.bhinbt #b01010) (cache.dxwbin #b10100)
                        (cache.dxltg #b10000)  (cache.dxstg #b10010) (cache.dxin #b10110)   (cache.dhin #b11010)
                        (cache.dhwbin #b11000) (cache.dxldt #b10001) (cache.dxsdt #b10011)  (cache.dhwoin #b1110)))

(make-ee-instructions (make-encoding :spec #b010000 :fixed #x2000000 :inst 0 :func #'inst-type6) 
                      '((di #b111001) (ei #b111000) (eret #b011000))) 

(make-ee-instructions (make-encoding :spec #b010000 :fixed #xc000 :inst 0 :rt 16 :imm 0 :func #'inst-type5)
                      '((mfbpc #b00000) (mfdab  #b00100) (mfdabm  #b00101) (mfdvb   #b00110) (mfdvbm  #b00111)
                        (mfiab #b00010) (mfiabm #b00011)))

(make-ee-instructions (make-encoding :spec #b010000 :fixed #x80c000 :inst 0 :rt 16 :imm 0 :func #'inst-type5)
                      '((mtdab  #b0000100) (mtdabm  #b0000101) (mtdvb   #b0000110) (mtdvbm  #b0000111) 
                        (mtiab  #b0000010) (mtiabm  #b0000011)))
                        
(make-ee-instructions (make-encoding :spec #b010000 :inst 21 :rt 16 :rs 11 :func #'inst-type4)
                      '((mfc0 #b00000) (mtc0 #b00100)))

(make-ee-instructions (make-encoding :spec #b010000 :fixed #xc800 :rs 16 :imm 1 :inst 0 :func #'inst-type2) 
                      '((mfpc #b0000001) (mfps #b0000000) (mtpc #x800001) (mtps #x800000)))

(make-ee-instructions (make-encoding :spec #b010000 :fixed #x2000000 :inst 0 :func #'inst-type6) 
                      '((tbpl #b0001000) (tlbr #b0000001) (tlwi #b0000010) (tlwr  #b0000110)))


