
;
; Setup the gs and screen
;

(defun gs-init
  ; write a dummy gif-tag to just init the GS (dunno why this is needed but well)
  (gs-dma-write-wait '((0 gif:nop))

  ; screen-setup
  (gs-dma-write-wait '((gs:prmode-count 1)
  					   (gs:frame :fbw 10)
  					   (gs:zbuf :zbp 216 :zpf z24)
  					   (gs:scissor 0 639 0 359)
  					   (gs:xyoffset (ash 2048 4) (ash 2048 4))
  					   (gs:test :ate 0 :atst 1 :arf 255 :ztst 1) 
  					   (gs:color-clamp 1)
  					   (gs:dimx -4 2 -3 3 0 -2 1 1 -1 -3 3 -4 2 1 -1 0 -2)
  					   (gs:dthe 0)))

; Main function

(defun main-func
  (gs-init))
