@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-2
D;JNE
@SP
M=M-1
A=M-1
M=-1
@END-3
0;JMP
(FALSE-2)
@SP
M=M-1
A=M-1
M=0
(END-3)
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-4
D;JNE
@SP
M=M-1
A=M-1
M=-1
@END-5
0;JMP
(FALSE-4)
@SP
M=M-1
A=M-1
M=0
(END-5)
@16
D=A
@SP
A=M
M=D
@SP
M=M+1
@17
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-6
D;JNE
@SP
M=M-1
A=M-1
M=-1
@END-7
0;JMP
(FALSE-6)
@SP
M=M-1
A=M-1
M=0
(END-7)
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-8
D;JGE
@SP
M=M-1
A=M-1
M=-1
@END-9
0;JMP
(FALSE-8)
@SP
M=M-1
A=M-1
M=0
(END-9)
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@892
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-10
D;JGE
@SP
M=M-1
A=M-1
M=-1
@END-11
0;JMP
(FALSE-10)
@SP
M=M-1
A=M-1
M=0
(END-11)
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@891
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-12
D;JGE
@SP
M=M-1
A=M-1
M=-1
@END-13
0;JMP
(FALSE-12)
@SP
M=M-1
A=M-1
M=0
(END-13)
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-14
D;JLE
@SP
M=M-1
A=M-1
M=-1
@END-15
0;JMP
(FALSE-14)
@SP
M=M-1
A=M-1
M=0
(END-15)
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32767
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-16
D;JLE
@SP
M=M-1
A=M-1
M=-1
@END-17
0;JMP
(FALSE-16)
@SP
M=M-1
A=M-1
M=0
(END-17)
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@32766
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=M-D
@FALSE-18
D;JLE
@SP
M=M-1
A=M-1
M=-1
@END-19
0;JMP
(FALSE-18)
@SP
M=M-1
A=M-1
M=0
(END-19)
@57
D=A
@SP
A=M
M=D
@SP
M=M+1
@31
D=A
@SP
A=M
M=D
@SP
M=M+1
@53
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1
@112
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1
@SP
A=M-1
M=!M
M=M+1
@SP
A=M-1
D=M
A=A-1
M=D&M
@SP
M=M-1
@82
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
M=D|M
@SP
M=M-1
@SP
A=M-1
M=!M