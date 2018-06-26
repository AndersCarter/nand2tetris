#lang racket
(require 2htdp/batch-IO)
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

;tests
;various defined tests and resulting answers

;test-lines
(define T1 "push constant 2")
(define T2 "push constant 5")
(define T3 "add")
(define T4 "pop constant")
(define FT1 "call mult 3")
(define FT2 "function mult 8")
(define FT3 "return")

;answers-lines 
(define A1 '("@2" "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define A2 '("@5" "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define A3 '("@SP" "A=M-1" "D=M" "A=A-1" "M=D+M" "@SP" "M=M-1"))
(define A4 '("@SP" "M=M-1"))
(define FA1 '("push return-address" "push LCL" "push ARG" "push THIS" "push THAT" "ARG" "LCL" "goto mult" "(mult-ret)"))
(define FA2 '("(mult)" "@8" "D=A" "(START)" "D=D-1" "@LCL" "A=M+D" "M=0" "A=D-1" "@START" "D;JNE"))
(define FA3 '("FRAME" "RET" "RET-RESULT" "RET-SP" "RET-THAT" "RET-THIS" "RET-ARG" "RET-LCL" "goto-ret ~a"))


;;Translation lists
;the functions that produce the tranlated list
;Note: throughout these list ~a is used as placeholder that is changed depending on what is command is used.

;start
;starting values for everything
(define (start) '("@256" "D=A" "@SP" "M=D" "@300" "D=A" "@LCL" "M=D" "@400" "D=A" "@ARG" "M=D" "@500" "D=A" "@THIS" "M=D" "@600" "D=A" "@THAT" "M=D"))

;push hack cmds
(define (push-arg) '("@~a" "D=A" "@ARG" "A=M+D" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-lcl) '("@~a" "D=A" "@LCL" "A=M+D" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-static) '("@~a" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-constant) '("@~a" "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-this) '("@~a" "D=A" "@THIS" "A=M+D" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-that) '("@~a" "D=A" "@THAT" "A=M+D" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-pointer) '("@~a" "D=A" "@THIS" "A=A+D" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))
(define (push-temp) '("@~a" "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"))

;pop hack cmds
(define (pop-arg) '("@SP" "A=M-1" "D=M" "@13" "M=D" "@~a" "D=A" "@ARG" "D=M+D" "@14" "M=D" "@13" "D=M" "@14" "A=M" "M=D" "@SP" "M=M-1"))
(define (pop-lcl) '("@SP" "A=M-1" "D=M" "@13" "M=D" "@~a" "D=A" "@LCL" "D=M+D" "@14" "M=D" "@13" "D=M" "@14" "A=M" "M=D" "@SP" "M=M-1"))
(define (pop-static) '("@SP" "A=M-1" "D=M" "@~a" "M=D" "@SP" "M=M-1"))
(define (pop-constant) '("@SP" "M=M-1"))
(define (pop-this) '("@SP" "A=M-1" "D=M" "@13" "M=D" "@~a" "D=A" "@THIS" "D=M+D" "@14" "M=D" "@13" "D=M" "@14" "A=M" "M=D" "@SP" "M=M-1"))
(define (pop-that) '("@SP" "A=M-1" "D=M" "@13" "M=D" "@~a" "D=A" "@THAT" "D=M+D" "@14" "M=D" "@13" "D=M" "@14" "A=M" "M=D" "@SP" "M=M-1"))
(define (pop-pointer) '("@SP" "A=M-1" "D=M" "@13" "M=D" "@~a" "D=A" "@THIS" "D=A+D" "@14" "M=D" "@13" "D=M" "@14" "A=M" "M=D" "@SP" "M=M-1"))
(define (pop-temp) '("@SP" "A=M-1" "D=M" "@~a" "M=D" "@SP" "M=M-1"))

;aritmetic hack cmds
(define (add-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "M=D+M" "@SP" "M=M-1"))
(define (sub-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "M=M-D" "@SP" "M=M-1"))
(define (neg-cmd) '("@SP" "A=M-1" "M=!M" "M=M+1"))
(define (eq-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "D=M-D" "@FALSE-" "D;JNE" "@SP" "M=M-1" "A=M-1" "M=-1" "@END-" "0;JMP" "(FALSE-)" "@SP" "M=M-1" "A=M-1" "M=0" "(END-)"))
(define (gt-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "D=M-D" "@FALSE-" "D;JLE" "@SP" "M=M-1" "A=M-1" "M=-1" "@END-" "0;JMP" "(FALSE-)" "@SP" "M=M-1" "A=M-1" "M=0" "(END-)"))
(define (lt-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "D=M-D" "@FALSE-" "D;JGE" "@SP" "M=M-1" "A=M-1" "M=-1" "@END-" "0;JMP" "(FALSE-)" "@SP" "M=M-1" "A=M-1" "M=0" "(END-)"))
(define (and-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "M=D&M" "@SP" "M=M-1"))
(define (or-cmd) '("@SP" "A=M-1" "D=M" "A=A-1" "M=D|M" "@SP" "M=M-1"))
(define (not-cmd) '("@SP" "A=M-1" "M=!M"))

;function translate VM-VM
;these are the function VM cmds translated to intermediate VM cmds

(define (call-vm) '("push return-address" "push LCL" "push ARG" "push THIS" "push THAT" "ARG" "LCL" "goto ~a" "(~a-ret)")) ;where ~a is a label
(define (return-vm) '("FRAME" "RET" "RET-RESULT" "RET-SP" "RET-THAT" "RET-THIS" "RET-ARG" "RET-LCL"
                              "goto-ret ~a"
                              )) ;~a is a lable

;function translate VM-HACK
;the intermediet VM to Hack translation

;call
(define (push-sp-address) '("@SP" "D=M" "A=M-1" "M=D" "@SP" "M=M+1"))
(define (push-lcl-address) '("@LCL" "D=M" "@SP" "A=M-1" "M=D"))
(define (push-arg-address) '("@ARG" "D=M" "@SP" "A=M-1" "M=D" "@SP" "M=M+1"))
(define (push-this-address) '("@THIS" "D=M" "@SP" "A=M-1" "M=D" "@SP" "M=M+1"))
(define (push-that-address) '("@THAT" "D=M" "@SP" "A=M-1" "M=D" "@SP" "M=M+1"))
(define (arg) '("@~a" "D=A" "@SP" "D=M-D" "@5" "D=D-A" "@ARG" "M=D")) ;~a=n from call
(define (lcl) '("@SP" "D=M" "@LCL" "M=D" "@~a" "D=A" "@SP" "M=M+D")) ;~a=k from function

;function
(define (function) '("(~a)" "@~a" "D=A" "@END" "D;JEQ" "(START)" "@SP" "A=M" "M=0" "@SP" "M=M+1" "D=D-1" "@START" "D;JGT" "(END)" ))
;the first ~a creates a label,the second ~a=k, which is the number of lcl variables

;return
(define (FRAME) '("@LCL" "D=M" "@FRAME" "M=D")) ;@Frame is defined in the SYM-TBL below to be the same as @13
(define (RET) '("@5" "D=A" "@FRAME" "A=M-D" "D=M" "@RET" "M=D")) ;@RET is define in the SYM-TBL below to be @14
(define (RET-RESULT) '("@SP" "A=M-1" "D=M" "@ARG" "A=M" "M=D")) ;@HOLD is defined in the sym table as @15
(define (RET-SP) '("@ARG" "D=M" "@SP" "M=D+1"))
(define (RET-THAT) '("@1" "D=A" "@FRAME" "A=M-D" "D=M" "@THAT" "M=D"))
(define (RET-THIS) '("@2" "D=A" "@FRAME" "A=M-D" "D=M" "@THIS" "M=D"))
(define (RET-ARG) '("@3" "D=A" "@FRAME" "A=M-D" "D=M" "@ARG" "M=D"))
(define (RET-LCL) '("@4" "D=A" "@FRAME" "A=M-D" "D=M" "@LCL" "M=D"))

;other
(define (goto) '("@~a" "0;JMP")) ;~a=Label of the function or return address
(define (goto-ret) '("@~a-ret" "0;JMP"))
(define (if-goto) '("@SP" "M=M-1" "A=M" "D=M" "@~a" "D;JNE"))
(define (label) "(~a)")

;;Tables
;hash tables for all the VM->HACK translations

;Symbol Tabel
;these are just names given to register 13-15 to give an idea of their function
(define SYM-TBL (hash "@FRAME" "@13"
                      "@RET" "@14"
                      "@HOLD" "@15"))


;push-segment-table
(define PUSH-TBL (hash "argument" (push-arg)
                       "local" (push-lcl)
                       "static" (push-static)
                       "constant" (push-constant)
                       "this" (push-this)
                       "that" (push-that)
                       "pointer" (push-pointer)
                       "temp" (push-temp)))


;pop-segment-table
(define POP-TBL (hash "argument" (pop-arg)
                      "local" (pop-lcl)
                      "static" (pop-static)
                      "constant" (pop-constant)
                      "this" (pop-this)
                      "that" (pop-that)
                      "pointer" (pop-pointer)
                      "temp" (pop-temp)))

;arithmetic table
(define ARTH-TBL (hash "add" (add-cmd)
                       "sub" (sub-cmd)
                       "neg" (neg-cmd)
                       "eq" (eq-cmd)
                       "gt" (gt-cmd)
                       "lt" (lt-cmd)
                       "and" (and-cmd)
                       "or" (or-cmd)
                       "not" (not-cmd)))

;function table
(define FUNC-TBL (hash "ARG" (arg)
                       "LCL" (lcl)
                       "goto" (goto)
                       "function" (function)
                       "FRAME" (FRAME)
                       "RET" (RET)
                       "RET-RESULT" (RET-RESULT)
                       "RET-SP" (RET-SP)
                       "RET-THAT" (RET-THAT)
                       "RET-THIS" (RET-THIS)
                       "RET-ARG" (RET-ARG)
                       "RET-LCL" (RET-LCL)
                       "call" (call-vm)
                       "return" (return-vm)
                       "push return-address" (push-sp-address)
                       "push LCL" (push-lcl-address)
                       "push ARG" (push-arg-address)
                       "push THIS" (push-this-address)
                       "push THAT" (push-that-address)))




;                              
;                              
;                              
;                              
;  ;;; ;;;  ;;    ;;;;; ;;; ;;;
;   ;; ;;    ;      ;    ;;  ; 
;   ;; ;;   ; ;     ;    ;;  ; 
;   ; ; ;   ; ;     ;    ; ; ; 
;   ; ; ;   ; ;     ;    ; ; ; 
;   ;   ;  ;;;;;    ;    ; ; ; 
;   ;   ;  ;   ;    ;    ;  ;; 
;  ;;; ;;;;;; ;;; ;;;;; ;;; ;; 
;                              
;                              
;                              


;;main
; top of function heirarcy
; reads,translates and writes files

(define (main)
  (define name
    (command-line #:args ([filename "ProgramFlow\\BasicLoop"]) filename))
  (define files (filter (λ (x) (string-contains? x ".vm")) (map (λ (x) (path->string x)) (directory-list name))))


  (define lines (foldr (λ (x y) (append x y)) '() (map (λ (x) (read-lines (string-append name "\\" x))) files)))
  (define no-cmt (filter (λ (x) (and (not (void? x))
                                     (non-empty-string? x)))
                         (map (λ (x) (eliminate-comments x)) lines)))
  (define k-hold (map (λ (x) (k-hold-set x)) no-cmt))
  (define translated (foldr  (λ (x y)(append x y)) '()
                                           (map (λ (x) (VM-to-Hack x)) k-hold)))
  (define bootstrap (append (start) translated))

  (printf "Done")
 (define output-file-name (file-namechange name ".asm"))
 (write-file output-file-name (string-join bootstrap "\n")))

;;file-namechange : string string->string
;;changes a file extention

(check-expect (file-namechange "input.exe" ".txt") "input.txt")
(check-expect (file-namechange "input.hack" ".htdp") "input.htdp")

(define (file-namechange a-file a-ext)
  (define SPLIT (string-split a-file "."))
  (string-append (first SPLIT) a-ext))


;;eliminate-comments : string -> string
; eliminate all the comments from the file

(check-expect (eliminate-comments "//something") (void))
(check-expect (eliminate-comments "D+M //something") "D+M")
(check-expect (eliminate-comments "D+A") "D+A")

(define (eliminate-comments a-str)
  (cond [(string-prefix? a-str "//") (void)]
        [(string-prefix? a-str "/") (void)]
        [(string-contains? a-str "//")
         (define start (filter (λ (x) (non-empty-string? x)) (string-split (first (string-split a-str "//")) " ")))
         (if (< 1 (length start))
             (foldr (λ (x y) (string-append x " " y)) "" start)
             start)]
        [else a-str]))

(define test "if-goto IF_TRUE             //something")




;                                                                        
;                                                                        
;                                                                        
;                                                                        
;  ;;; ;;;;;; ;;;                            ;;; ;;;              ;;     
;   ;   ;  ;; ;;          ;                   ;   ;                ;     
;   ;   ;  ;; ;;         ;;;;;   ;;;          ;   ;   ;;;    ;;;;  ; ;;; 
;    ; ;   ; ; ;          ;     ;   ;         ;;;;;  ;   ;  ;   ;  ;  ;  
;    ; ;   ; ; ;  ;;;;;   ;     ;   ;  ;;;;;  ;   ;   ;;;;  ;      ;;;   
;    ; ;   ;   ;          ;     ;   ;         ;   ;  ;   ;  ;      ; ;   
;     ;    ;   ;          ;   ; ;   ;         ;   ;  ;   ;  ;   ;  ;  ;  
;     ;   ;;; ;;;          ;;;   ;;;         ;;; ;;;  ;;;;;  ;;;  ;; ;;; 
;                                                                        
;                                                                        
;                                                                        


;;VM-to-Hack : VM cmd -> list of hack cmds
; takes the list of vm commands and translates them to the Hack machine language
(check-expect (VM-to-Hack T1) A1)
(check-expect (VM-to-Hack T2) A2)
(check-expect (VM-to-Hack T3) A3)
(check-expect (VM-to-Hack T4) A4)


(define (VM-to-Hack a-line)
  (define translated (if (or (string-contains? a-line "call")
                             (string-contains? a-line "function")
                             (string-contains? a-line "return")
                             (string-contains? a-line "goto")
                             (string-contains? a-line "label"))
                         (add-cmd-label (translate-function-cmd a-line))
                         (add-label (non-function-cmd a-line))))
  (cons (string-append "//" a-line) translated))

;;VM-function-to-hack

;add-cmd-label : a-list-of-hack-cmds -> a-list-of-hack-cmds
;adds unique labels to all current labels in hack-command
(define cmd-global -1)
(define tbl (make-hash))

(define (add-cmd-label a-lol)
  (define start (map (λ (x) 
                       (cond [(string-contains? x "(" )
                              (define show (string-append (substring x 0 (- (string-length x) 1)) (number->string cmd-global) ")"))
                              (hash-set! tbl x cmd-global)
                              show]
                             [else x])) a-lol))
  
  
  (map (λ (x) (cond [(void? x) x]
                    [(and (string-contains? x "@")
                          (not (string-contains? x "ret"))
                          (hash-has-key? tbl (string-append "(" (substring x 1 (string-length x)) ")")))
                     (string-append x (number->string (hash-ref tbl (first (filter (λ (y) (string-contains? y (substring x 1 (string-length x)))) (hash-keys tbl))))))]
                    [(and (string-contains? x "@")
                          (string-contains? x "ret")
                          (hash-has-key? tbl (string-append "(" (substring x 1 (- (string-length x) 4)) ")")))
                     (define show2 (string-append x (number->string (hash-ref tbl (first (filter (λ (z) (string-contains? z (substring x 1 (- (string-length x) 4))))
                                                                                                 (hash-keys tbl)))))))
                     (set! cmd-global (sub1 cmd-global))
                     (set! tbl (make-hash))
                     show2]
                    [else x]))
       start))







;translate-function-cmd : a-vm-command -> a-list-of-hack-commands
;top level heirarchy: translates the function vm commands, call, function, and return

(check-expect (translate-function-cmd "goto mult") '("@mult" "0;JMP"))
(check-expect (translate-function-cmd "if-goto mult") '("@sp" "A=M-1" "D=M" "@mult" "D;JEQ"))
(check-expect (translate-function-cmd "label mult") '("(mult)"))

(define RET-HOLD '())
(define K-HOLD (make-hash))
(define (translate-function-cmd a-cmd)
  (define split (string-split a-cmd " "))
  (cond [(string=? (first split) "call") (translate-call-hack (translate-call-vm (second split) (third split)) (second split) (third split))]
        [(string=? (first split) "function") (translate-function-hack (second split) (third split))]
        [(string=? (first split) "return") (translate-return-hack (return-vm))]
        [(string=? (first split) "goto") (map (λ (x) (if (string-contains? x "~a")
                                                         (format x (second split))
                                                         x)) (goto))]
        [(string=? (first split) "if-goto") (map (λ (x) (if (string-contains? x "~a")
                                                            (format x (second split))
                                                            x))
                                                 (if-goto))]
        [(string=? (first split) "label") (list (format (label) (second split)))]))

;;translate-call-vm: function-name number-of-args -> list-of-vm-commands
; translates the call vm command into an intermediate vm command

(check-expect (translate-call-vm "mult" "2") '("push return-address" "push LCL" "push ARG" "push THIS" "push THAT" "ARG" "LCL" "goto mult" "(mult-ret)"))

(define (translate-call-vm f n)
  (set! RET-HOLD (cons f RET-HOLD))
  (map (λ (x) (if (string-contains? x "~a")
                  (format x f)
                  x)) (call-vm)))



;;translate-call-hack : list-of-intermediate-cmds -> list-of-hack-commands
; translates the intermediate vm commands to hack commands

(check-expect (translate-call-hack '("push return-address" "goto mult")) (append (push-sp-address) '("@mult" "0;JMP")))

(define (translate-call-hack a-intermediate f n)
  (foldr (λ (x y) (append x y)) '()
         (map (λ (x) (cond [(string-contains? x "goto")
                            (map (λ (y) (if (string-contains? y "~a")
                                            (format y (second (string-split x " ")))
                                            y))
                                 (goto))]
                           [(string-contains? x "(") (list x)]
                           [(string-contains? x "ARG")
                            (map (λ (z) (if (string-contains? z "~a")
                                            (format z n)
                                            z)) (arg))]
                           [(string-contains? x "LCL")
                            (map (λ (a) (if (string-contains? a "~a")
                                            (format a (hash-ref K-HOLD f))
                                            a)) (lcl))]
                           [else (hash-ref FUNC-TBL x)])) a-intermediate)))

;;translate-functon-hack : function-name number-of-lcl -> list of hack commands
; translates the function vm command into a list of hack commands

(check-expect (translate-function-hack "mult" "3") '("(mult)" "@3" "D=A" "(START)" "D=D-1" "@LCL" "A=M+D" "M=0" "A=D-1" "@START" "D;JNE"))


(define (translate-function-hack f k)
  (set! RET-HOLD (cons f RET-HOLD))
  (map (λ (x) (cond [(and (string-contains? x "~a")
                          (string-contains? x "(")) (format x f)]
                    [(and (string-contains? x "~a")
                          (string-contains? x "@")) (format x k)]
                    [else x])) (function)))

;k-hold-set : line -> line
(define (k-hold-set a-line)
  (define split (string-split a-line " "))
  (cond [(string-contains? a-line "function")
         (hash-set! K-HOLD (second split) (third split))
         a-line]
        [else a-line]))

;;translate-return-hack : list-of-intermediate-vm-cmds -> list-of-hack-commands
; translates return from intermediate-vm-cmds to hack-commands

(define (translate-return-hack a-intermediate)
  (define result (foldr (λ (x y) (append x y)) '()
                        (map (λ (x) (if (string-contains? x "goto")
                                        (map (λ (x) (if (string-contains? x "~a")
                                                        (format x (first RET-HOLD))
                                                        x))
                                             (goto-ret))
                                        (hash-ref FUNC-TBL x))) a-intermediate)))
  (if (<= 1 (length RET-HOLD))
      (set! RET-HOLD empty)
      (set! RET-HOLD (rest RET-HOLD)))
  (map (λ (x) (if (hash-has-key? SYM-TBL x)
                  (hash-ref SYM-TBL x)
                  x)) result))

;;non-function-cmd : vm-cmd -> list-of-hack-cmds
;translates all the commands that do not deal with functions

(check-expect (non-function-cmd T1) A1)
(check-expect (non-function-cmd T2) A2)
(check-expect (non-function-cmd T3) A3)
(check-expect (non-function-cmd T4) A4)

(define (non-function-cmd a-cmd)
  (define no-space (string-split a-cmd " "))
  (cond [(string=? "push" (first no-space)) (push (second no-space) (string->number (third no-space)))]
        [(string=? "pop" (first no-space)) (if (= (length no-space) 3)
                                               (pop (second no-space) (string->number (third no-space)))
                                               (hash-ref POP-TBL (second no-space)))]
        [else (hash-ref ARTH-TBL (first no-space))]))

;;push : segment index -> line
; takes the segment and index and translate it to Hack machine code
(check-expect (push "constant" "2") A1)

(define (push a-seg i)
  (format-hash PUSH-TBL a-seg i))

;;pop : segment index -> line
; takes the segment and index and translate it to Hack machine code
(check-expect (pop "local" 3) '("@SP" "A=M-1" "D=M" "@13" "M=D" "@3" "D=A" "@LCL" "D=M+D" "@14" "M=D" "@13" "D=M" "@14" "A=M" "M=D" "@SP" "M=M-1"))

(define (pop a-seg i)
  (format-hash POP-TBL a-seg i))

;format-hash : hash segment index ->
; performs push and pop

(define (format-hash a-tbl a-seg i)
  (define hack-code (hash-ref a-tbl a-seg))
  (define next-i (cond [(string=? "temp" a-seg) (+ i 5)]
                       [(string=? "static" a-seg) (+ i 16)]
                       [else i]))
  (map (λ (x) (if (string-contains? x "~a")
                  (format x next-i)
                  x))
       hack-code))

;;add-labels : list-of-lines -> list-of-lines
; takes a label and adds a number to it.

(check-expect (add-label '("@sp" "A=M-1" "D=M" "A=A-1" "D=M-D" "@sp" "M=M-1" "A=M-1" "@FALSE-" "D;JNE" "M=-1" "@END-" "0;JMP" "(FALSE-)" "M=0" "(END-)"))
              '("@sp" "A=M-1" "D=M" "A=A-1" "D=M-D" "@sp" "M=M-1" "A=M-1" "@FALSE-0" "D;JNE" "M=-1" "@END-1" "0;JMP" "(FALSE-0)" "M=0" "(END-1)"))

(define global 0)
(define (add-label a-lol)
  (define tbl (make-hash))
  
  (define start (map (λ (x) 
                       (cond [(string-contains? x "(" )
                              (define show (string-append (substring x 0 (- (string-length x) 1)) (number->string global) ")"))
                              (hash-set! tbl x global)
                              (set! global (add1 global))
                              show]
                             [else x])) a-lol))
  
  
  (map (λ (x) (cond [(void? x) x]
                    [(and (string-contains? x "@")
                          (hash-has-key? tbl (string-append "(" (substring x 1 (string-length x)) ")")))
                     (string-append x (number->string (hash-ref tbl (first (filter (λ (y) (string-contains? y (substring x 1 (string-length x)))) (hash-keys tbl))))))]
                    [else x])) start))



;(test)
(main)