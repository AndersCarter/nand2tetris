// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[3], respectively.)

@R1    // Loads R1 as A
D=M
@R3    // Load R3 as A
M=D    

@R2    // Load R2 as A
M=0    // blank slate on start


(REP)            //Flags this as the start of REP for later (repeat)
        @R3      // Loads R3 as A
        D=M      // D = Y
        @FIN     // Loads (FIN) to A
        D;JLE    // if Y < 0, jump to END
        @R3      // Loads R3 back to A
        M=D-1    // if Y (>=) 0, Y (-=) 1
        @R0      // Loads R0 to A
        D=M      // D=X
        @R2      // Loads the solution in A
        M=D+M    // R2 (+=) X
    @REP         // vv
    0;JMP        // Jump up to REP


(FIN)
    @FIN
    0;JMP    // Just keep looping to FIN, effectivly killing it