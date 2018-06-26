#lang Racket
(require 2htdp/batch-io)
(require test-engine/racket-tests)



;                                                                 
;                                                                 
;                                                                 
;                                                                 
;    ;;;;                                                         
;   ;   ;                        ;                    ;           
;   ;       ;;;  ;; ;;    ;;;;  ;;;;;   ;;;  ;; ;;   ;;;;;   ;;;; 
;   ;      ;   ;  ;;  ;  ;   ;   ;     ;   ;  ;;  ;   ;     ;   ; 
;   ;      ;   ;  ;   ;   ;;;    ;      ;;;;  ;   ;   ;      ;;;  
;   ;      ;   ;  ;   ;      ;   ;     ;   ;  ;   ;   ;         ; 
;   ;   ;  ;   ;  ;   ;  ;   ;   ;   ; ;   ;  ;   ;   ;   ; ;   ; 
;    ;;;    ;;;  ;;; ;;; ;;;;     ;;;   ;;;;;;;; ;;;   ;;;  ;;;;  
;                                                                 
;                                                                 
;                                                                 


(define ALU-TBL (hash "0" "0101010"
                      "1" "0111111"
                      "-1" "0111010"
                      "D" "0001100"
                      "A" "0110000"
                      "!D" "0001101"
                      "!A" "0110001"
                      "-D" "0001111"
                      "-A" "0110011"
                      "D+1" "0011111"
                      "A+1" "0110111"
                      "D-1" "0001110"
                      "A-1" "0110010"
                      "D+A" "0000010"
                      "A+D" "0000010"
                      "D-A" "0010011"
                      "A-D" "0000111"
                      "D&A" "0000000"
                      "D|A" "0010101"
                      "M" "1110000"
                      "!M" "1110001"
                      "-M" "1110011"
                      "M+1" "1110111"
                      "M-1" "1110010"   
                      "D+M" "1000010"
                      "A+M" "1000010"
                      "D-M" "0010011"
                      "M-D" "1000111"
                      "D&M" "1000000"
                      "D|M" "1010101"))

(define JMP-TBL (hash "JGT" "001"
                      "JEQ" "010"
                      "JGE" "011"
                      "JLT" "100"
                      "JNE" "101"
                      "JLE" "110"
                      "JMP" "111"
                      "null" "000"))

(define D-INST-TBL (hash "M" "001"
                         "D" "010"
                         "A" "100"
                         "AM" "101"
                         "AD" "110"
                         "MD" "011"
                         "AMD" "111"
                         "null" "000"))
(define SYM-TBL (make-hash))





;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;   ;;;;                                    ;;;;               ;;;     ;               ;               ;                           
;    ;  ;            ;                       ;  ;             ;                              ;                                     
;    ;   ;   ;;;;   ;;;;;    ;;;;            ;   ;   ;;;;   ;;;;;;   ;;;    ;; ;;    ;;;    ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;; 
;    ;   ;  ;    ;   ;      ;    ;           ;   ;  ;    ;    ;        ;     ;;  ;     ;     ;         ;    ;    ;   ;;  ;  ;    ; 
;    ;   ;   ;;;;;   ;       ;;;;;           ;   ;  ;;;;;;    ;        ;     ;   ;     ;     ;         ;    ;    ;   ;   ;   ;;;;  
;    ;   ;  ;    ;   ;      ;    ;           ;   ;  ;         ;        ;     ;   ;     ;     ;         ;    ;    ;   ;   ;       ; 
;    ;  ;   ;   ;;   ;   ;  ;   ;;           ;  ;   ;         ;        ;     ;   ;     ;     ;   ;     ;    ;    ;   ;   ;  ;    ; 
;   ;;;;     ;;; ;;   ;;;    ;;; ;;         ;;;;     ;;;;;  ;;;;;;   ;;;;;  ;;; ;;;  ;;;;;    ;;;    ;;;;;   ;;;;   ;;; ;;; ;;;;;  
;                                                                                                                                  
;                                                                                                                                  
;                                                                                                                                  
;

;; Hack Instruction
;; a Hack instruction is just a machine instruction (given as a string) in the Hack language
;;Examples
(define HACK1 "D=D+M")
(define HACK2 "A=M")
(define JUMP "0;JMP")
(define Ainst "@9")
(define Ainst2 "@1")


;; an intruction is either
;; an A instruction, or
;; a C instuction
;; a instruction input for the Nand2Tetris CPU

;;An A instruction is a 16 digit binary number that starts with a 0
;Examples
(define A1 "0000000000000001")
(define A9 "0000000000001001")


;;A C instruction is a 16 digit binary number that starts with three 1's in the form of
;;ixxaccccccdddjjj, where
;; - "i" determines what type of instruction
;; - "xx" are irrellivant in c instructions (if a C instruction then they will just be 1's)
;; - "a" whether the ALU functions are using the value in the A Register or the value in the selected Register
;; - "cccccc" determines what function the ALU does
;; - "ddd" determines the destination of the ALU  function
;; - "jjj" determines if there will be a jump or not.

;Example
(define C1 "1111000010010000") ;D=D+M
(define C2 "1111110000100000") ;A=M
(define C3 "1110101010000111") ;0;JMP


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;   ;;;;;;                                     ;                           
;    ;   ;                           ;                                     
;    ; ;    ;;  ;;  ;; ;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;; 
;    ;;;     ;   ;   ;;  ;  ;   ;;   ;         ;    ;    ;   ;;  ;  ;    ; 
;    ; ;     ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;   ;;;;  
;    ;       ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;       ; 
;    ;       ;  ;;   ;   ;  ;    ;   ;   ;     ;    ;    ;   ;   ;  ;    ; 
;   ;;;       ;; ;; ;;; ;;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;; ;;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          


;;Hack->ASM : Hack Instruction -> A instruction or, C instruction
;Translates a Hack Instruction into binary. This will either be an A instruction
;or a C instruction
(check-expect (Hack->ASM HACK1) C1)
(check-expect (Hack->ASM HACK2) C2)
(check-expect (Hack->ASM JUMP) C3)
(check-expect (Hack->ASM Ainst) A9)
(check-expect (Hack->ASM Ainst2) A1)


(define (Hack->ASM an-inst)
  (if (string-contains? an-inst "@")
      (A->Hack an-inst)
      (C->Hack an-inst)))






;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;     ;;             ;;;;;                                                             ;                   
;      ;               ;                     ;                               ;                             
;     ; ;              ;    ;; ;;    ;;;;;  ;;;;;   ;; ;;;  ;;  ;;   ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;  
;     ; ;              ;     ;;  ;  ;    ;   ;       ;;      ;   ;  ;   ;;   ;         ;    ;    ;   ;;  ; 
;     ; ;              ;     ;   ;   ;;;;    ;       ;       ;   ;  ;        ;         ;    ;    ;   ;   ; 
;     ;;;              ;     ;   ;       ;   ;       ;       ;   ;  ;        ;         ;    ;    ;   ;   ; 
;    ;   ;             ;     ;   ;  ;    ;   ;   ;   ;       ;  ;;  ;    ;   ;   ;     ;    ;    ;   ;   ; 
;   ;;; ;;;          ;;;;;  ;;; ;;; ;;;;;     ;;;   ;;;;;     ;; ;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;;
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          







;;A->Hack : Hack instruction -> A instruction
;takes the given A instruction and creates an A instruction

(check-expect (A->Hack Ainst) "0000000000001001")
(check-expect (A->Hack Ainst2) "0000000000000001")

(define (A->Hack a-ainst)
  (string-append "0" (to-binary (string->number (first (string-split a-ainst "@"))))))

;;to-binary : number -> string
; converts a number into string of 16 binary digits

(check-expect (to-binary 16) "000000000010000")
(check-expect (to-binary 12345) "011000000111001")

(define (to-binary a-num)
  (add-15 (to-binary/acc a-num empty)))

;;to-binary/acc : number list-of-string -> string
;;accumulator fuction for to-binary

(define (to-binary/acc a-num a-los)
  (if (< a-num 1)
      (foldr (λ (x y) (string-append x y)) "" (map (λ (x) (number->string x)) a-los))
      (to-binary/acc (if (even? a-num)
                         (/ a-num 2)
                         (/ (- a-num 1) 2))
                     (cons (remainder a-num 2) a-los)))) 


;;add-15 : string -> string
; adds 0's to the string until the string has a length of 16

(check-expect (add-15 "1111111111") "000001111111111")
(check-expect (add-15 "101") "000000000000101")

(define (add-15 a-str)
  (if (= 15 (string-length a-str))
      a-str
      (add-15 (string-append "0" a-str))))



;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;     ;;;;           ;;;;;                                                             ;                   
;    ;   ;             ;                     ;                               ;                             
;   ;                  ;    ;; ;;    ;;;;;  ;;;;;   ;; ;;;  ;;  ;;   ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;  
;   ;                  ;     ;;  ;  ;    ;   ;       ;;      ;   ;  ;   ;;   ;         ;    ;    ;   ;;  ; 
;   ;                  ;     ;   ;   ;;;;    ;       ;       ;   ;  ;        ;         ;    ;    ;   ;   ; 
;   ;                  ;     ;   ;       ;   ;       ;       ;   ;  ;        ;         ;    ;    ;   ;   ; 
;    ;   ;             ;     ;   ;  ;    ;   ;   ;   ;       ;  ;;  ;    ;   ;   ;     ;    ;    ;   ;   ; 
;     ;;;            ;;;;;  ;;; ;;; ;;;;;     ;;;   ;;;;;     ;; ;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;;
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          


;;C->Hack : Hack Instruction-> C-instruction
;; Produces the C binary instruction for the Hack machine instruction

(check-expect (C->Hack HACK1) C1)
(check-expect (C->Hack HACK2) C2)
(check-expect (C->Hack JUMP) C3)
(define T "D=A+M;JMP")
(define G "AM=D+1;JGE")

(define (C->Hack a-inst)
    (string-append "111"
                   (alu-instruction (get-alu-cmd a-inst))
                   (d-instruction (get-dest-cmd a-inst))
                   (jump-command (get-jmp-cmd a-inst))))


;; jump-command : string -> string
;; encodes the different jump commands

(check-expect (jump-command "JGT") "001")
(check-expect (jump-command "JEQ") "010")
(check-expect (jump-command "JGE") "011")
(check-expect (jump-command "JLT") "100")
(check-expect (jump-command "JNE") "101")
(check-expect (jump-command "JLE") "110")
(check-expect (jump-command "JMP") "111")


(define (jump-command a-str)
  (hash-ref JMP-TBL a-str))


;; alu-instruction : string -> string
; takes the Hack instrunction and turns it into the binary part. This function is for
; the acccccc part of the c instruction

(check-expect (alu-instruction "D+A") "0000010")
(check-expect (alu-instruction "A+D") "0000010")
(check-expect (alu-instruction "!D") "0001101")
(check-expect (alu-instruction "D|M") "1010101")



(define (alu-instruction a-str)
  (hash-ref ALU-TBL a-str))

;;d-instruction : string -> string
;turns the hack destination into binary. This is the "ddd" of the c-instruction

(check-expect (d-instruction "D") "010")
(check-expect (d-instruction "AMD") "111")
(check-expect (d-instruction "M") "001")




(define (d-instruction a-str)
  (hash-ref D-INST-TBL a-str))


;get-dest-cmd : string -> string
;breaks a Hack command into just the destination

(check-expect (get-dest-cmd "DM=A+1") "DM")
(check-expect (get-dest-cmd "0;JMP") "null")
(check-expect (get-dest-cmd "A=D+M") "A")
(check-expect (get-dest-cmd "D=A+1;JGT") "D")

(define (get-dest-cmd a-cmd)
  (define DEST-SPLIT (string-split a-cmd "="))
  (define DEST-EXIST (if (= 1 (length DEST-SPLIT))
                         "null"
                         (first DEST-SPLIT)))
  DEST-EXIST)

;get-jmp-cmd : string -> string
;breaks a Hack Command into just the jump command

(check-expect (get-jmp-cmd "A=D+M") "null")
(check-expect (get-jmp-cmd "0;JMP") "JMP")
(check-expect (get-jmp-cmd "D=A+M;JGE") "JGE")

(define (get-jmp-cmd a-cmd)
  (define JMP-SPLIT (string-split a-cmd ";"))
  (define JMP-EXIST (if (= 1 (length JMP-SPLIT))
                         "null"
                         (second JMP-SPLIT)))
  JMP-EXIST)

;get-alu-cmd : string -> string
; breaks the Hack command into just the ALU command

(check-expect (get-alu-cmd "A=D+M") "D+M")
(check-expect (get-alu-cmd "0;JMP") "0")
(check-expect (get-alu-cmd "D=A+M;JGE") "A+M")

(define (get-alu-cmd a-cmd)
  (define ALU-SPLIT-ONE (string-split a-cmd "="))
  (define ALU-SPLIT-TWO (if (= 1 (length ALU-SPLIT-ONE))
                            (string-split (first ALU-SPLIT-ONE) ";")
                            (string-split (second ALU-SPLIT-ONE) ";")))
  (first ALU-SPLIT-TWO))




;                                                                        
;                                                                        
;                                                                        
;                                                                        
;  ;;;;;  ;;;;;;   ;;   ;;;;        ; ;;; ;;;;;;;;   ;;;;; ;;;;;;;;;;;;; 
;   ;   ;  ;   ;    ;    ;  ;       ;  ;   ;  ;   ;    ;   ;  ;  ; ;   ; 
;   ;   ;  ; ;     ; ;   ;   ;     ;   ;   ;  ;   ;    ;      ;    ; ;   
;   ;   ;  ;;;     ; ;   ;   ;     ;   ; ; ;  ;   ;    ;      ;    ;;;   
;   ;;;;   ; ;     ; ;   ;   ;    ;    ; ; ;  ;;;;     ;      ;    ; ;   
;   ;  ;   ;      ;;;;;  ;   ;    ;    ; ; ;  ;  ;     ;      ;    ;     
;   ;   ;  ;   ;  ;   ;  ;  ;    ;     ; ; ;  ;   ;    ;      ;    ;   ; 
;  ;;;   ;;;;;;; ;;; ;;;;;;;     ;      ; ;  ;;;   ; ;;;;;   ;;;  ;;;;;; 
;                               ;                                        
;                                                                        
;





;;main :
; takes the given input file 

(define (main)
  (define name
    (command-line
     #:args ([filename "input.asm"])
     filename))


  (define lines (read-lines name))
  (define no-comments (eliminate-comments lines))
  (printf "Done ~a\n" name)

  (define hack-file (map (λ (x) (Hack->ASM x)) no-comments))
  (define output-file-name (file-namechange name ".hack"))
  (write-file output-file-name
              (string-join hack-file "\n")))


;;file-namechange : string string->string
;;changes a file extention

(check-expect (file-namechange "input.exe" ".txt") "input.txt")
(check-expect (file-namechange "input.hack" ".htdp") "input.htdp")

(define (file-namechange a-file a-ext)
  (define SPLIT (string-split a-file "."))
  (string-append (first SPLIT) a-ext))

;;eliminate-comments : list-of-strings -> string
; eliminate all the comments from the file

(check-expect (eliminate-comments (list "//something" "D+M" "D+M //something" )) (list "D+M" "D+M"))

(define Input (list "//Something" "D+2" "@1" "M=D"))

(define (eliminate-comments a-los)

  (define destroy-comment (filter (λ (x) (or (not (string-prefix? x "//"))
                                             (not (non-empty-string? x)))) a-los))
  (define no-in-line (map (λ (x) (remove-cmt x)) destroy-comment))
  (define no-empty (filter (λ (x) (non-empty-string? x)) no-in-line))
  no-empty)




;remove-cmt: string -> string
;same as above

(check-expect (remove-cmt "D+M   //something") "D+M")

(define (remove-cmt a-str)
  (foldr (λ (x y) (string-append x y)) "" (filter (λ (x) (or (not (string-prefix? x "//"))
                                                             (not (non-empty-string? x))))
                                                  (string-split a-str " "))))



;                                                   
;                                                   
;                                                   
;                                                   
;    ;; ; ;;; ;;;;;; ;;;       ;;;;;;;;;;;;   ;;;   
;   ;  ;;  ;   ;  ;; ;;        ;  ;  ; ;   ;   ;    
;   ;       ; ;   ;; ;;           ;    ;   ;   ;    
;    ;;;    ; ;   ; ; ;           ;    ;;;;    ;    
;       ;    ;    ; ; ;  ;;;;;    ;    ;   ;   ;    
;       ;    ;    ;   ;           ;    ;   ;   ;  ; 
;   ;;  ;    ;    ;   ;           ;    ;   ;   ;  ; 
;   ; ;;    ;;;  ;;; ;;;         ;;;  ;;;;;   ;;;;; 
;                                                   
;                                                   
;                                                   

(define L1 '("M=D+M" "@sum" "D=A+D" "(LOOP)" "@LOOP" "@sum" "D=D+M;JMP" "(NEXT)" "M=A+M" "@NEXT" "@hi"))

;create-final : hash -> hash
#;(define (create-final x)
  (define loop-cmds (map (λ (x) (substring x 1 (- (string-length x) 1)))
                         (filter (λ (x) (string-prefix? x "(")) (hash-keys SYM-TBL))))
  )


;create-tbl: list-of-strings -> hash
;takes the list of lines and creates a symbol table (SYM-TBL in constants) of all the jump destinations


(define (create-tbl a-los)
  (define line-num 0)
  (for ([line a-los])
    (cond
      [(is comment or blank)  (void)]
      [(is a label (...))   (hash-set! (extract-label line)  line-num)]
      [else (set! line-num (add1 line-num))]
      )))


    ]))

  
  (loop a-los)
  (define cmds (only-cmds (hash-keys SYM-TBL)))
  (define remove (filter (λ (x) (or (hash-has-key? ALU-TBL x)
                                    (hash-has-key? JMP-TBL x)
                                    (hash-has-key? D-INST-TBL x))) cmds))
  (remove-cmds-tbl remove)
  )

;remove-cmds-tbl : list-of-cmds -> hash
;removes all commands from the SYM-TBL. This leaves only the symbols

(define (remove-cmds-tbl a-loc)
  (map (λ (x) (hash-remove! SYM-TBL x)) (foldr (λ (x y) (append x y)) '() (map (λ (x) (hash-key-contains x)) a-loc))))

;hash-key-contains : string -> key
;takes a string and then searches to see if any keys contain that string

(define (hash-key-contains a-str)
  (filter (λ (x) (string-contains? x a-str)) (hash-keys SYM-TBL)))



; only-cmds : list-of-strings -> list-of-strings
; takes a list of commands and splits them into their parts

(define (only-cmds a-los)
  (foldr (λ (x y) (append x y)) '()
         (map (λ (x) (string-split x ";"))
              (foldr (λ (x y) (append x y)) '()
                     (map (λ (x) (string-split x "="))
                          a-los)))))



;; loop : (listof String) -> hash
;; formats all the lines in the given list with line numbers and lengths


(define (loop lines)
  (for/list ([n (range 0 (add1 (length lines)))]
             [line lines])
    (format-line line n)))


;; format-line : String Number -> String
;; produces line with its line number in front and length at end
;; produces comment lines unchanged


(define (format-line line n)
  (cond [(string-prefix? (string-trim line) "//") line]
        [else
         (hash-set! SYM-TBL line n)]))




(main)
(test)



















