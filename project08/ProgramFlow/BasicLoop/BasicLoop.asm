@256
D=A
@SP
M=D
@300
D=A
@LCL
M=D
@400
D=A
@ARG
M=D
@500
D=A
@THIS
M=D
@600
D=A
@THAT
M=D
//push constant 0    
@0
D=A
@SP
A=M
M=D
@SP
M=M+1
//pop local 0 
@SP
A=M-1
D=M
@13
M=D
@0
D=A
@LCL
D=M+D
@14
M=D
@13
D=M
@14
A=M
M=D
@SP
M=M-1
//label LOOP_START
(LOOP_START-1)
//push argument 0    
@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
//push local 0
@0
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
//add
@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1
//pop local 0
@SP
A=M-1
D=M
@13
M=D
@0
D=A
@LCL
D=M+D
@14
M=D
@13
D=M
@14
A=M
M=D
@SP
M=M-1
//push argument 0
@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
//push constant 1
@1
D=A
@SP
A=M
M=D
@SP
M=M+1
//sub
@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1
//pop argument 0 
@SP
A=M-1
D=M
@13
M=D
@0
D=A
@ARG
D=M+D
@14
M=D
@13
D=M
@14
A=M
M=D
@SP
M=M-1
//push argument 0
@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1
//if-goto LOOP_START 
@SP
M=M-1
A=M
D=M
@LOOP_START-1
D;JNE
//push local 0
@0
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1