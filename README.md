Score!, a programming language
=============================================================================

Score! is a new programming language which aims to continue where GOAL (http://en.wikipedia.org/wiki/Game_Oriented_Assembly_Lisp) 
left off (While GOAL is an acronym Score! is not but I figured it's better to have a name that 
resembles GOAL and builds on it instead of calling it GOAL++ or GOAL#) so what is the aim of Score!?

Here is a blogpost I did not long ago about Score! http://altdevblogaday.com/introducing-score

Productivity
------------

Make programmers productive, get away from the C/C++ cycle of compile/link/etc yet provide constructs 
to make things fast and to keep code in native. Ability to test the code you just added directly and to iterate on it.

Syntax
------

The syntax will (just like Lisp and GOAL) be based around S-Expressions. Some people don't 
like this but s-expressions allows for very powerful constructs that is close to impossible 
in other languages and it's still possible to implement DSL style constructs using macros. 
S-expressions is also very simple to parse and will allow you to reform/reconstruct the way you want to.

Assembler
---------

Like GOAL Score! aims to be an assembler but with high-level constructs. 
It will be possible to write high-level code in Score! but that is not the major aim. 
Using inline assembler is supposed to be easy and well defined.

Different targets, one tool-set
-------------------------------

Say that you want to run some vu0 code on you ps2, it needs to be compiled separately, 
linked and brought it as an "extern "C" g_myVu0Program" or something similar. 
The chain usually hinders productivity as it adds several steps the programmer needs 
to take to actually do something. What you actually want to do is 
(pesudo code, don't take this as final in any way)

	(def-vu0-fun vu0-fun (input1 :in vf01                      
   	                      input2 :in vf02                      
   	                      output :out vf03)  
  	  (add output input1 input2))

	(defun my-ee-fun ()  
  	; do something funky, set stuff etc and then call the code  
  	(vu0-fun))

Also the same way would be possible on PS3 when running a SPU program from the PPU:

	(def-spu-fun spu-fun (data :u32)  
  	  (spu-format "~d~%" data)

	(defun my-ppu-fun ()  
  	  (spu-fun 0)) ; scheduling, etc is up to the implementation

On PC your OpenCL programs can be checked at compile time instead 
of you getting some run-time error later.

Produce code to back-end/assembler
----------------------------------

Score! should be able to support different targets but yet can be tailored to 
the way you want it to behave. For example it would be possible to support LLVM 
as a back-end for supporting a various numbers of targets and LLVM has a very 
active community but still generate instructions directly (for PS2 for example) 
would also be possible.

Machine running all the time
----------------------------

Just like Lisp and GOAL. You will have a "machine" running all the time that 
you feed your data/functions to. That way you are always live and updated functions 
will be garbage collected at a safe point in the "frame" (all depends on application usage)

License and Copyright
---------------------

Score! is Copyright 2011 Daniel Collin All Rights Reserved.

Score! is made available under the GNU GPL. See the file COPYING for the
complete license text.

The license may change over time but right now I want people to contribute to less eventual fragmentation

