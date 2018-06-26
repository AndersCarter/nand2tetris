// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.

(START)
//sets store variable as KBD-1
	@KBD
	D=A 
	D=D-1 
	@store 
	M=D

//sets sum as a variable with the same value as SCREEN
	@SCREEN 
	D=A 
	@sum  
	M=D 
	
(BLACK)
//jumps to white section if a key is not pressed
	@KBD 
	D=M 
	@WHITE
	D;JLE //jumps if a key is not pressed
	
//jumps sets current sum as the address and then sets that address to -1	
	@sum 
	A=M 
	M=-1 


//sets carry as current address for changing later
	D=A 
	@carry 
	M=D 
	
//jumps if the address is at the max address which is 23575 
//fixes the running trying to store in a non-existent register. 
	@store 
	A=M-D //M-D=(current store-current address)
	D=A  
	@START
	D;JEQ 
	
//sets next address
	@carry
	D=M
	@sum
	M=D+1 //sets sum to next address
	
//loops back to the top
	@BLACK
	0;JMP
	
(WHITE)
//jumps to BLACK section if key is pressed
	@KBD
	D=M
	@BLACK
	D;JGT
	
//sets current address to 0
	@sum
	A=M 
	M=0 
	
//sets carry as current address for changing later
	D=A 
	@carry 
	M=D 
	
	
//jumps if the address is at the max address which is 23575 
//fixes the running trying to store in a non-existent register. 
	@store 
	A=M-D //M-D=(current store-current address)
	D=A  
	@START
	D;JEQ 
	
//sets next address
	@carry
	D=M
	@sum
	M=D+1 //sets sum to next address
	
//loops back to the top
	@WHITE
	0;JMP
	