#lang racket

(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require (only-in lang/htdp-advanced
         explode))

;; ---------------------------------------------------------
;; DATA DEFINITION FOR TOKENS


;; TOKType is one of the symbols:
;;  'KEYWORD 'SYMBOL 'INT 'STR 'IDENT 'COMMENT 'UNKNOWN

(struct token (type value)  #:transparent)
;; A Token is (token TOKType X)

; examples:
(define E1 (token 'KEYWORD "define"))
(define E2 (token 'INT-CONST 42))

;; ----------------------------------------------------------
;; Symbol Tables

(struct jackvar (name type kind number) #:transparent)
;; a jackvar is (jackvar string type kind number)
;; a variable in the jack language

(define J1 (jackvar "sum" "int" "static" 0))
(define J2 (jackvar "name" "string" "var" 1))

;; a type is one of the following strings
;; - int
;; - string
;; - array
;; - boolean
;; refers to the what type of variable is being declared

;; a kind is one of the following strings
;; - static
;; - field
;; - local
;; - argument
;; refers to what kind of variable is being defined

;; ----------------------------------------------------------
;; Tables

;; operator table
;; operators to be translated

(define OP-TBL (hash "+" "add"
                     "-" "sub"
                     "*" "call Math.multiply 2"
                     "/" "call Math.divide 2"
                     "&" "and"
                     "|" "or"
                     "<" "lt"
                     ">" "gt"
                     "=" "eq"))
(define UOP-TBL (hash "-" "neg"
                      "~" "not"))

;; mutable hash tables for the local and global symbol tables
(define GLB-TBL (make-hash))
(define LCL-TBL (make-hash))

;; number labels for the table by kind for use in the symbol tables
(define STAT 0)
(define FIELD 0)
(define ARG 0)
(define LCL 0)


;; a list containg the two tables
(define TBL-LIST '(LCL-TBL GLB-TBL))

;; ----------------------------------------------------------
;; HOLD variables

(define HOLD '())
(define CLASS-NAME "")
(define TRANSLATED '())




;; ----------------------------------------------------------
;; Top level Parsing Functions

;; class-dec : list-of-tokens -> list-of-tokens
;; parses the initial class declaration

(define (class-dec a-lot)
  (set! CLASS-NAME (token-value (second a-lot)))
  (var-maybe (list-tail a-lot 3))) ;ignores class and "{"

;; var-maybe : list-of-tokens -> list-of-tokens
;; determines if there are declared variables

(define (var-maybe a-lot)
  (if (is-class-var? (token-value (first a-lot)))
      (var-class a-lot)
      (subroutine a-lot)))

;; -----------------------------------------------------------
;; class level / global variable declarations

;; var-class : list-of-tokens -> list-of-tokens
;; adds variables to a symbol table

(define (var-class a-lot)
  (cond [(string=? "static" (token-value (first a-lot))) (static-class a-lot)]
        [(string=? "field" (token-value (first a-lot))) (field-class a-lot)]
        [else (subroutine a-lot)]))

;; static-class : list-of-tokens -> list-of-tokens
;; adds the static variable to the global table

(define (static-class a-lot)
  (define var (jackvar (token-value (third a-lot))
                           (token-value (second a-lot))
                           (token-value (first a-lot))
                           STAT)) ;; creates (jackvar name type kind number)
  (jackvar-set! var GLB-TBL)
  (update-kind! "static")
  (define recur (class-recur (list-tail a-lot 3 (token-value (second a-lot)) (token-value (first a-lot)))))
  (var-class (rest recur))) ;ignores ";"


;; field-class : list-of-tokens -> list-of-tokens
;; adds the field variable tot he global table

(define (field-class a-lot)
  (define var (jackvar (token-value (third a-lot))
                           (token-value (second a-lot))
                           (token-value (first a-lot))
                           FIELD))
  (jackvar-set! var GLB-TBL)
  (update-kind! "field")
  (define recur (class-recur (list-tail a-lot 3) (token-value (second a-lot)) (token-value (first a-lot))))
  (var-class (rest recur))) ;ingnores ";"

;; field-class-recur : list-of-tokens type kind -> list-of-tokens
;; creates entries in GLB-TBL when more than one variables are declared in one declaration

(define (class-recur a-lot type kind)
  (cond [(and (string=? "," (token-value (first a-lot)))
              (string=? "field" kind)) (define var (jackvar (token-value (second a-lot)) type kind FIELD))
                                       (jackvar-set! var GLB-TBL)
                                       (update-kind! "field")
                                       (class-recur (list-tail a-lot 2) type kind)]
        [(and (string=? "," (token-value (first a-lot)))
              (string=? "static" kind)) (define var2 (jackvar (token-value (second a-lot)) type kind STAT))
                                       (jackvar-set! var2 GLB-TBL)
                                       (update-kind! "static")
                                       (class-recur (list-tail a-lot 2) type kind)]
        [(string=? ";" (token-value (first a-lot))) a-lot]))


;; -----------------------------------------------------------
;; subroutine level parsing

;; subroutine : list-of-tokens -> list-of-tokens
;; top level subroutine parsing function

(define (subroutine a-lot)
  (cond [(and (string=? "}" (token-value (first a-lot)))
              (string=? "}" (token-value (second a-lot)))) (printf "Done")
                                                           TRANSLATED]
        [else (define function (functionDec a-lot))
              (bootstrap!)
              (parse-statements function)]))

;; bootstrap! : void -> void
;; translates commands for allocating memory for field variables

(define (bootstrap!)
  (TRANSLATE-set! (list "call Memory.alloc 1" (format "push constant ~a" FIELD))))

;; functionDec : list-of-tokens -> list-of-tokens
;; declares the function name and # of arguements

(define (functionDec a-lot)
  (cond [(string=? "constructor" (token-value (first a-lot))) (define constructor (create-constructor (token-value (second a-lot)) (token-value (third a-lot))))
                                                              (HOLD-set! constructor)
                                                              (TRANSLATE-set! (list constructor))
                                                              (list-tail a-lot 7)]
        [(string=? "method" (token-value (first a-lot))) (define method (create-method (token-value (second a-lot)) (token-value (third a-lot))))
                                                         (HOLD-set! method)
                                                         (TRANSLATE-set! (list method))
                                                         (set-args-maybe (list-tail a-lot 4))]
        [(string=? "function" (token-value (first a-lot))) (define function (create-function (token-value (second a-lot)) (token-value (third a-lot))))
                                                           (HOLD-set! function)
                                                           (TRANSLATE-set! (list function))
                                                           (set-args-maybe (list-tail a-lot 4))]
        [else a-lot]))

;; create-constructor : string string -> string
;; creates the function declaration for a constructor statement

(define (create-constructor class name)
  (define field-number (number->string (add1 FIELD))) ;can use because it counts as well as labels
  (string-append "function" " " class "." name " " field-number))

;; create-method : string string -> string
;; creates the function declaration for a method statement

(define (create-method class name)
  (define args (number->string (add1 FIELD)))
  (string-append "function" " " CLASS-NAME "." name " " args))

;; create-function : string string -> string
;; creates function declaration for function statement

(define (create-function class name)
  (define args (number->string FIELD))
  (string-append "function" " " CLASS-NAME "." name " " args))


;; set-args-maybe : list-of-tokens -> list-of-tokens
;; sets the arguements into the LCL table

(define (set-args-maybe a-lot)
  (cond [(string=? ")" (token-value (first a-lot))) (list-tail a-lot 2)]
        [else (set-args a-lot)]))

;; set-args : list-of-tokens -> list-of-tokens
;; recursive portion of set-args

(define (set-args a-lot)
  (define args (jackvar (token-value (second a-lot)) (token-value (first a-lot)) "arguement" ARG))
  (jackvar-set! args LCL-TBL)
  (update-kind! "arg")
  (cond [(string=? ")" (token-value (third a-lot))) (list-tail a-lot 3)]
        [(string=? "," (token-value (third a-lot))) (set-args-recur (list-tail a-lot 3))]
        [else "break"]))

;; set-args-recur

(define (set-args-recur a-lot) a-lot)



;; -----------------------------------------------------------
;; statment parsing functions

;; parse-statments : list-of-tokens -> list-of-tokens
;; top level parsing for statments within a method, function, or constructor

(define (parse-statements a-lot)
  (cond [(string=? "var" (token-value (first a-lot))) (varDec (rest a-lot))]
        [(is-statement? (token-value (first a-lot))) (statementDec a-lot)]
        [(string=? "}" (token-value (first a-lot))) a-lot]
        [else (printf "break parse-statements") a-lot]))

;; varDec : list-of-tokens -> list-of-tokens
;; adds local variables to the local sym table

(define (varDec a-lot)
  (define lcl (jackvar (token-value (second a-lot)) (token-value (first a-lot)) "local" LCL))
  (update-kind! "lcl")
  (jackvar-set! lcl LCL-TBL)
  (cond [(string=? "," (token-value (third a-lot))) (varDec-next (list-tail a-lot 3) (token-value (first a-lot)))]
        [(string=? ";" (token-value (third a-lot))) (parse-statements (list-tail a-lot 3))]
        [else (printf "varDec break") a-lot]))

;; varDec-next : list-of-tokens kind -> list-of-tokens
;; declares a second variable of the same given kind

(define (varDec-next a-lot kind)
  (define lcl (jackvar (token-value (first a-lot)) kind "local" LCL))
  (update-kind! "lcl")
  (jackvar-set! lcl LCL-TBL)
  (cond [(string=? "," (token-value (second a-lot))) (varDec-next (list-tail a-lot 2) kind)]
        [(string=? ";" (token-value (second a-lot))) (parse-statements (list-tail a-lot 2))]
        [else (printf "varDec-next break") (list-tail a-lot 2)]))

;; -----------------------------------------------------------
;; statementDec Functions

;; statementDec : list-of-tokens -> list-of-tokens
;; top level statementDec function

(define (statementDec a-lot)
  (printf (string-append (token-value (first a-lot)) " "))
  (cond [(string=? "let" (token-value (first a-lot))) (letDec (rest a-lot))]
        [(string=? "if" (token-value (first a-lot))) (ifDec (rest a-lot))]
        [(string=? "return" (token-value (first a-lot))) (returnDec (rest a-lot))]
        [(string=? "while" (token-value (first a-lot))) (whileDec (rest a-lot))]
        [(string=? "do" (token-value (first a-lot))) (doDec (rest a-lot))]
        [else (subroutine a-lot)]))

;; doDec : list-of-tokens -> list-of-tokens
;; trasnlates do statements

(define (doDec a-lot)
  (define sub (translate-subroutineCall a-lot))
  (TRANSLATE-set! OP-HOLD)
  (statementDec (rest sub))) ;;ignores ";"

;; whileDec : list-of-tokens -> list-of-tokens
;; translates while statements

(define WHILE 0)

(define (whileDec a-lot)
  (define expr (expressionDec (rest a-lot)))
  (TRANSLATE-set! (list (format "if goto WHILE-~a" WHILE)))
  (define stmt (statementDec (list-tail expr 2))) ;; ignores ")" "{"
  (TRANSLATE-set! (list (format "(WHILE-~a)" WHILE)))
  (WHILE-inc!)
  (statementDec (rest stmt))) ;; ignores "}"

;; WHILE-inc! : void -> void
;; increments WHILE variable

(define (WHILE-inc!)
  (set! WHILE (add1 WHILE)))

;; returnDec : list-of-tokens -> list-of-tokens
;; translates return statements

(define (returnDec a-lot)
  (cond [(string=? ";" (token-value (first a-lot))) (TRANSLATE-set! (list "return" "push constant 0"))
                                                    (statementDec (rest a-lot))]
        [else (define expr (expressionDec a-lot))
              (TRANSLATE-set! (list "return"))
              (rest expr)]))

;; letDec : list-of-tokens -> list-of-tokens
;; translates let statements

(define (letDec a-lot)  ;; finish!!!!!
  (define varName (format "pop ~a" (get-jackvar (first a-lot))))
  (define var-expr (cond [(string=? "[" (token-value (second a-lot))) (OP-HOLD-set! "add")
                                                                      (expressionDec (list-tail a-lot 2))]
                         [else a-lot])) 
  (define expr (expressionDec (list-tail var-expr 2))) ;; skips equal sign
  (TRANSLATE-set! (list varName))
  (statementDec expr)) ;; ignores ";"

;; ifDec : list-of-tokens -> list-of-tokens
;; translates if statements

(define (ifDec a-lot)
  (define expr (expressionDec (rest a-lot))) ;; ignores "("
  (TRANSLATE-set! (list (format "if-goto ELSE-~a" ELSE)))
  (define stmt (statementDec (list-tail expr 2))) ;; ignores ")" and "{"
  (TRANSLATE-set! (list (format "goto end-~a" END)))
  (TRANSLATE-set! (list (format "(else-~a)" ELSE)))
  (ELSE-inc!)
  (cond [(string=? "else" (token-value (second stmt))) (define fin (elseDec (list-tail stmt 2)))
                                                       (TRANSLATE-set! (list (format "(end-~a)" END)))
                                                       (END-inc!)
                                                       (rest fin)] ;; ignores ";"
        [else (TRANSLATE-set! (list (format "(end-~a)" END)))
              (END-inc!)
              (rest stmt)])) ;; ignores "}"

(define ELSE 0)
(define END 0)

;; ELSE-inc! : void -> void
;; increments ELSE by 1

(define (ELSE-inc!)
  (set! ELSE (add1 ELSE)))

;; END-inc! : void -> void
;; increments END by 1

(define (END-inc!)
  (set! END (add1 END)))

;; elseDec : list-of-tokens -> list-of-tokens
;; translates else statements

(define (elseDec a-lot)
  (define stmt (statementDec (rest a-lot))) ;; ignores "{"
  (rest stmt)) ;; ignores "}"



;; -----------------------------------------------------------
;; expression functions

(define EXPR-HOLD '())
(define OP-HOLD '())

;; expressionDec : list-of-tokens stop string -> list-of-tokens
;; translates expressions, where the stop is the character to look for when stopping

(define (expressionDec a-lot)
  (define translated (translate-term a-lot))
  (cond [(is-op? (token-value (first translated))) (set! OP-HOLD (cons (translate-op (first translated)) OP-HOLD))
                                                   (expressionDec (rest translated))]
        [else (expressionFin) translated]))

;; expressionFin : void -> void
;; creates the final translated express and adds it to HOLD

(define (expressionFin)
  (define push EXPR-HOLD)
  (define op (reverse OP-HOLD))
  (TRANSLATE-set! (append op push))
  (OP-reset!)
  (EXPR-reset!))

;; translate-term : list-of-tokens -> list-of-tokens
;; translates terms

(define (translate-term a-lot)
  (cond [(equal? 'INT-CONSTANT (token-type (first a-lot))) (EXPR-HOLD-set! (translate-int (first a-lot)))
                                                           (rest a-lot)]
        [(equal? 'STR (token-type (first a-lot))) (map EXPR-HOLD-set! (reverse (translate-str (first a-lot)))) ;;maybe reverse?
                                                  (rest a-lot)]
        [(is-keyword-const? (token-value (first a-lot))) (EXPR-HOLD-set! (translate-key (first a-lot)))
                                                         (rest a-lot)]
        [(and (equal? 'IDENT (token-type (first a-lot)))
              (not (or (string=? "." (token-value (second a-lot)))
                       (string=? "(" (token-value (second a-lot)))))) (translate-varname a-lot)]
        [(equal? 'SYMBOL (token-type (first a-lot))) (define expr (expressionDec (rest a-lot)))
                                                  (rest expr)]
        [(is-uo? (token-value (first a-lot))) (OP-HOLD-set! (translate-uo (first a-lot)))
                                              (translate-term (rest a-lot))]
        [(equal? 'IDENT (token-type (first a-lot))) (rest (translate-subroutineCall a-lot))]
        [else (printf "term break") (first a-lot)])) ;;ignores ";"

;; ---------------------------------------------------
;; Term Helper Functions

;; EXPR-HOLD-set! : string -> void
;; adds strings to EXPR-HOLD

(define (EXPR-HOLD-set! a-str)
  (set! EXPR-HOLD (cons a-str EXPR-HOLD)))

;; OP-HOLD-set! : string -> void
;; adds string to OP-HOLD

(define (OP-HOLD-set! a-str)
  (set! OP-HOLD (cons a-str OP-HOLD)))

;; translate-ident : token -> string
;; translates a identity/variable into VM command

(define (translate-ident a-tok)
  (format "push ~a" (get-jackvar a-tok)))

;; translate-int : token -> string
;; translates integer constants

(check-expect (translate-int (token 'INT-CONSANT "3")) "push constant 3")

(define (translate-int a-tok)
  (format "push constant ~a" (token-value a-tok)))

;; translate-str : token -> list-of-strings
;; translates string constants

(define STR (token 'STR-CONSTANT "SPACEBALLS THE TESTCASE"))

(define (translate-str a-tok)
  (define length (format "push constant ~a" (string-length (token-value a-tok))))
  (define firstcmd "call String.new 1")
  (define appendChar "call String.appendChar 2")
  (define char-explode (map number->string (map char->integer (string->list (token-value a-tok)))))
  (define push (map (λ (x) (format "push constant ~a" x)) char-explode))
  (define final (map (λ (x) (list x appendChar)) push))
  (define oneList (foldr (λ (x y) (append x y)) '() final))
  (cons length (cons firstcmd oneList)))

;; translate-key : token -> string
;; translates the key constants

(define (translate-key a-tok)
  (cond [(string=? "true" (token-value a-tok)) "push constant 1"]
        [(string=? "false" (token-value a-tok)) "push constant 0"]
        [(string=? "null" (token-value a-tok)) "push constant 0"] ;; does null = false?
        [(string=? "this" (token-value a-tok) (format "push ~a" (get-jackvar a-tok)))]))

;; translate-varname : list-of-tokens -> list-of-tokens
;; translates varnames and checks for varname[expression]

(define (translate-varname a-lot)
  (EXPR-HOLD-set! (translate-ident (first a-lot)))
  (cond [(string=? "[" (token-value (second a-lot))) (define expr (expressionDec (list-tail a-lot 2)))
                                                     (rest expr)]
        [(not (string=? "[" (token-value (second a-lot)))) (rest a-lot)]
        [else (printf "break translate-varname") a-lot]))

;; translate-subroutineCall : list-of-tokens -> list-of-tokens
;; translates subroutine calls

(define (translate-subroutineCall a-lot)
  (cond [(string=? "(" (token-value (second a-lot))) (define expr-lst (expression-list (list-tail a-lot 2)))
                                                     (OP-HOLD-set! (format "call ~a ~a" (token-value (first a-lot)) EXPR-LOOP))
                                                     (rest expr-lst)]
        [(string=? "." (token-value (second a-lot))) (define expr-lst2 (expression-list (list-tail a-lot 4)))
                                                     (OP-HOLD-set! (format "call ~a.~a ~a" (token-value (first a-lot)) (token-value (third a-lot)) EXPR-LOOP))
                                                     (rest expr-lst2)]))


;; expression-list : list-of-tokens -> list-of-tokens
;; decalares the expression list
(define EXPR-LOOP 0)

(define (expression-list a-lot)
  (EXPR-reset!)
  (OP-reset!)
  (cond [(string=? "," (token-value (first a-lot))) (EXPR-LOOP-inc!)
                                                    (expression-list a-lot)]
        [(string=? ")" (token-value (first a-lot))) a-lot]
        [else (EXPR-LOOP-inc!)
              (define expr (expressionDec a-lot))
              (expression-list expr)]))

;; EXPR-LOOP-inc! : void -> void
;; increments EXPR-LOOP

(define (EXPR-LOOP-inc!)
  (set! EXPR-LOOP (add1 EXPR-LOOP)))

;; EXPR-LOOP-reset! : void -> void
;; resets EXPR-LOOP

(define (EXPR-LOOP-reset!)
  (set! EXPR-LOOP 0))


;; ----------------------------------------------------
;; Expression Helper Functions

;; translate-uo : token -> void
;; translates an unary operator token into VM command 

(define (translate-uo a-tok)
  (hash-ref UOP-TBL (token-value a-tok)))

;; translate-op : token -> void
;; translates an operator token into a pop VM command

(define (translate-op a-tok)
  (hash-ref OP-TBL (token-value a-tok)))

;; get-jackvar : token -> string
;; retrieves information about the identifier for a variable in an expression

(define (get-jackvar a-tok)
  (define token (cond [(hash-has-key? LCL-TBL (token-value a-tok)) (hash-ref LCL-TBL (token-value a-tok))]
                      [(hash-has-key? GLB-TBL (token-value a-tok)) (hash-ref GLB-TBL (token-value a-tok))]
                      [else (printf "no hash key") a-tok]))
  (string-append (jackvar-kind token) " " (number->string (jackvar-number token))))


;; -----------------------------------------------------------
;; Low Level Helper Functions

;; is-class-var? : string -> boolean
;; determines if there is a class variable declatartion

(check-expect (is-class-var? "field") #t)
(check-expect (is-class-var? "static") #t)
(check-expect (is-class-var? "anything false") #f)

(define (is-class-var? a-str)
  (or (string=? "field" a-str)
      (string=? "static" a-str)))

;; reset-LCL! : hash -> hash
;; resets the LCL hash table to be emtpy

(define (reset-LCL!)
  (set! LCL-TBL (make-hash)))

;; jackvar-set! : jackvar hash -> void
;; places the jackvar into the given hash

(define (jackvar-set! a-jv a-tbl)
  (hash-set! a-tbl (jackvar-name a-jv) a-jv))

;; update-kind : kind -> void
;; adds one to the current value of the kind hold variable

(define (update-kind! a-str)
  (cond [(string=? "static" a-str) (set! STAT (add1 STAT))]
        [(string=? "field" a-str) (set! FIELD (add1 FIELD))]
        [(string=? "lcl" a-str) (set! LCL (add1 LCL))]
        [(string=? "arg" a-str) (set! ARG (add1 ARG))]))

;; HOLD-set! : string -> void
;; adds the string to list in HOLD

(define (HOLD-set! a-str)
  (set! HOLD (cons a-str HOLD)))

;; is-statement? : string -> boolean
;; determines if the string is a statement declaration

(check-expect (is-statement? "let") #t)
(check-expect (is-statement? "not") #f)

(define (is-statement? a-str)
  (define stmt-list '("let" "do" "return" "while" "if"))
  (ormap (λ (x) (string=? x a-str)) stmt-list))

;; is-uo? : string -> boolean
;; determines if the string is an unary operator

(check-expect (is-uo? "-") #t)
(check-expect (is-uo? "~") #t)
(check-expect (is-uo? "+") #f)

(define (is-uo? a-str)
  (hash-has-key? UOP-TBL a-str))

;; is-op? : string -> boolean
;; determines if the string is an operator

(check-expect (is-op? "+") #t)
(check-expect (is-op? "~") #f)

(define (is-op? a-str)
  (hash-has-key? OP-TBL a-str))

;; is-keyword-const? : string -> boolean
;; determines if the string is a keyword constant

(check-expect (is-keyword-const? "false") #t)
(check-expect (is-keyword-const? "that") #f)

(define (is-keyword-const? a-str)
  (define keyword-const '("null" "true" "false" "this"))
  (ormap (λ (x) (string=? a-str x)) keyword-const))

;; is-stop? : string token -> boolean
;; determines if the token value is the given string

(check-expect (is-stop? "]" (token 'SYM "]")) #t)
(check-expect (is-stop? "]" (token 'IDENT "a")) #f)

(define (is-stop? stop a-tok)
  (string=? stop (token-value a-tok)))

;; is-pushpop? : string -> boolean
;; determines if the string contains push or pop

(check-expect (is-pushpop? "something push") #t)
(check-expect (is-pushpop? "is poping") #t)
(check-expect (is-pushpop? "this is false") #f)

(define (is-pushpop? a-str)
  (or (string-contains? a-str "push")
      (string-contains? a-str "pop")))

;; TRANSLATE-HOLD-set! : list-of-strings -> void
;; adds the elements of the given list to TRANSLATED

(define (TRANSLATE-set! a-los)
  (set! TRANSLATED (append a-los TRANSLATED)))

;; OP-reset! : void -> void
;; resets the OP-HOLD variable

(define (OP-reset!)
  (set! OP-HOLD '()))

;; EXPR-reset! : void -> void
;; resets the EXPR-HOLD variable

(define (EXPR-reset!)
  (set! EXPR-HOLD '()))




;; ---------------------------------------------------------------


(define test-case (list
                   (token 'KEY "class")
                   (token 'IDENT "SquareGame")
                   (token 'SYMBOL "{")
                   (token 'KEY "field")
                   (token 'IDENT "Square")
                   (token 'IDENT "square")
                   (token 'SYMBOL ";")
                   (token 'KEY "field")
                   (token 'KEY "int")
                   (token 'IDENT "direction")
                   (token 'SYMBOL ";")
                   (token 'KEY "constructor")
                   (token 'IDENT "SquareGame")
                   (token 'IDENT "new")
                   (token 'SYMBOL "(")
                   (token 'SYMBOL ")")
                   (token 'SYMBOL "{")
                   (token 'KEY "let")))
(define method-case (list
                     (token 'KEY "method")
                     (token 'KEY "void")
                     (token 'IDENT "run")
                     (token 'SYMBOL "(")
                     (token 'KEY "int")
                     (token 'IDENT "a")
                     (token 'SYMBOL ",")
                     (token 'IDENT "Car")
                     (token 'IDENT "b")
                     (token 'SYMBOL ")")
                     (token 'SYMBOL "{")
                     (token 'KEY "var")
                     (token 'KEY "int")
                     (token 'IDENT "a")
                     (token 'SYMBOL ",")
                     (token 'IDENT "b")
                     (token 'SYMBOL ";")
                     (token 'KEY "var")
                     (token 'KEY "boolean")
                     (token 'IDENT "over?")
                     (token 'SYMBOL ";")
                     (token 'SYMBOL "}")))
(define expr-case (list
                   (token 'KEY "function")
                   (token 'KEY "int")
                   (token 'IDENT "multiply")
                   (token 'SYMBOL "(")
                   (token 'KEY "int")
                   (token 'IDENT "x")
                   (token 'SYMBOL ",")
                   (token 'KEY "int")
                   (token 'IDENT "y")
                   (token 'SYMBOL ")")
                   (token 'SYMBOL "{")
                   (token 'KEY "var")
                   (token 'KEY "int")
                   (token 'IDENT "sum")
                   (token 'SYMBOL ";")
                   (token 'KEY "var")
                   (token 'KEY "int")
                   (token 'IDENT "shifted")
                   (token 'SYMBOL ";")
                   (token 'KEY "var")
                   (token 'KEY "int")
                   (token 'IDENT "i")
                   (token 'SYMBOL ";")
                   (token 'KEY "let")
                   (token 'IDENT "i")
                   (token 'SYMBOL "=")
                   (token 'INT-CONSTANT "0")
                   (token 'SYMBOL ";")
                   (token 'KEY "let")
                   (token 'IDENT "sum")
                   (token 'SYMBOL "=")
                   (token 'INT-CONSTANT "0")
                   (token 'SYMBOL ";")
                   (token 'KEY "let")
                   (token 'IDENT "shifted")
                   (token 'SYMBOL "=")
                   (token 'IDENT "x")
                   (token 'SYMBOL ";")
                   (token 'KEY "do")))
(define if-case (list
                 (token 'KEY "function")
                 (token 'KEY "int")
                 (token 'IDENT "abs")
                 (token 'SYMBOL "(")
                 (token 'KEY "int")
                 (token 'IDENT "x")
                 (token 'SYMBOL ")")
                 (token 'SYMBOL "{")
                 (token 'KEY "if")
                 (token 'SYMBOL "(")
                 (token 'IDENT "x")
                 (token 'SYMBOL "<")
                 (token 'INT-CONSTANT "0")
                 (token 'SYMBOL ")")
                 (token 'SYMBOL "{")
                 (token 'KEY "let")
                 (token 'IDENT "x")
                 (token 'SYMBOL "=")
                 (token 'SYMBOL "-")
                 (token 'IDENT "x")
                 (token 'SYMBOL ";")
                 (token 'KEY "return")
                 (token 'IDENT "x")
                 (token 'SYMBOL ";")
                 (token 'SYMBOL "}")
                 (token 'KEY "else")
                 (token 'SYMBOL "{")
                 (token 'KEY "return")
                 (token 'IDENT "x")
                 (token 'SYMBOL ";")
                 (token 'SYMBOL "}")
                 (token 'KEY "function")))
(define while-case (list
                    (token 'KEY "class")
                    (token 'IDENT "Math")
                    (token 'SYMBOL "{")
                    (token 'KEY "static")
                    (token 'IDENT "Array")
                    (token 'IDENT "twoPowers")
                    (token 'SYMBOL ";")
                    (token 'KEY "function")
                    (token 'KEY "void")
                    (token 'IDENT "init")
                    (token 'SYMBOL "(")
                    (token 'SYMBOL ")")
                    (token 'SYMBOL "{")
                    (token 'KEY "var")
                    (token 'KEY "int")
                    (token 'IDENT "i")
                    (token 'SYMBOL ";")
                    (token 'KEY "var")
                    (token 'KEY "int")
                    (token 'IDENT "p")
                    (token 'SYMBOL ";")
                    (token 'KEY "let")
                    (token 'IDENT "i")
                    (token 'SYMBOL "=")
                    (token 'INT-CONSTANT "0")
                    (token 'SYMBOL ";")
                    (token 'KEY "let")
                    (token 'IDENT "p")
                    (token 'SYMBOL "=")
                    (token 'INT-CONSTANT "1")
                    (token 'SYMBOL ";")
                    (token 'KEY "let")
                    (token 'IDENT "twoPowers")
                    (token 'SYMBOL "=")
                    (token 'IDENT "Array")
                    (token 'SYMBOL ".")
                    (token 'IDENT "new")
                    (token 'SYMBOL "(")
                    (token 'INT-CONSTANT "16")
                    (token 'SYMBOL ")")
                    (token 'SYMBOL ";")
                    (token 'KEY "while")
                    (token 'SYMBOL "(")
                    (token 'IDENT "i")
                    (token 'SYMBOL "<")
                    (token 'INT-CONSTANT "16")
                    (token 'SYMBOL ")")
                    (token 'SYMBOL "{")
                    (token 'KEY "let")
                    (token 'IDENT "twoPowers")
                    (token 'SYMBOL "[")
                    (token 'IDENT "i")
                    (token 'SYMBOL "]")
                    (token 'SYMBOL "=")
                    (token 'IDENT "p")
                    (token 'SYMBOL ";")
                    (token 'KEY "let")
                    (token 'IDENT "p")
                    (token 'SYMBOL "=")
                    (token 'IDENT "p")
                    (token 'SYMBOL "+")
                    (token 'IDENT "p")
                    (token 'SYMBOL ";")
                    (token 'KEY "let")
                    (token 'IDENT "i")
                    (token 'SYMBOL "=")
                    (token 'IDENT "i")
                    (token 'SYMBOL "+")
                    (token 'INT-CONSTANT "1")
                    (token 'SYMBOL ";")
                    (token 'SYMBOL "}")
                    (token 'KEY "return")
                    (token 'SYMBOL ";")
                    (token 'SYMBOL "}")))

;; -----------------------------------------------------------
;; Top-level tokenizer functions

;; Tokenizer Mutable varibables
(define TOK-HOLD "")
(define TOK-LIST '())

;; A keyword is a one of the following strings:
(define KEY-LIST '("class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean"
                           "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"))

;; tokenFSM : tokenizer -> list-of-tokens
;; creates a list of tokens of a program written in Jack

(define (tokenFSM-start a-toker)
  (cond [(empty? a-toker) (reverse TOK-LIST)]
        [(is-whitespace? (first a-toker)) (tokenFSM-start (rest a-toker))]
        [(is-digit? (first a-toker)) (add-char! (first a-toker))
                                     (int-const (rest a-toker))]
        [(is-quote? (first a-toker)) (str-const (rest a-toker))] ;you don't need the quotes for token
        [(is-letter? (first a-toker)) (add-char! (first a-toker))
                                      (keyword (rest a-toker))]
        [(is-backslash? (first a-toker)) (maybe-comment a-toker)]
        [(is-symbol? (first a-toker)) (add-char! (first a-toker))
                                      (symbol (rest a-toker))]))

;; ---------------------------------------------------------
;; main state functions

;; int-const : tokenizer -> token
;; creates an interger constant token

(define (int-const a-toker)
  (cond [(empty? a-toker) (add-token! (token 'INT-CONSTANT TOK-HOLD))
                          (reset-hold!)
                          (tokenFSM-start a-toker)]
        [(is-digit? (first a-toker)) (add-char! (first a-toker))
                                     (int-const (rest a-toker))]
        [else (add-token! (token 'INT-CONSTANT TOK-HOLD))
              (reset-hold!)
              (tokenFSM-start a-toker)])) ;;push-back non-int string

;; str-const : tokenizer -> token
;; creates a string constant token

(define (str-const a-toker)
  (cond [(empty? a-toker) (add-token! (token 'STR-CONSTANT TOK-HOLD))
                          (set! TOK-HOLD "")
                          (tokenFSM-start a-toker)]
        [(not (is-quote? (first a-toker))) (set! TOK-HOLD (string-append TOK-HOLD (first a-toker)))
                                               (str-const (rest a-toker))]
        [(is-quote? (first a-toker)) (add-token! (token 'STR TOK-HOLD))
                                     (set! TOK-HOLD "")
                                     (tokenFSM-start (rest a-toker))]))

;; keyword : tokenizer -> token
;; creates a keyword token

(define (keyword a-toker)
  (cond [(empty? a-toker) (cond [(member? TOK-HOLD KEY-LIST)
                                 (add-token! (token 'KEY TOK-HOLD))
                                 (reset-hold!)
                                 (tokenFSM-start a-toker)]
                                [else (identity a-toker)])]
        [(is-letter? (first a-toker)) (set! TOK-HOLD (string-append TOK-HOLD (first a-toker)))
                                      (keyword (rest a-toker))]
        [else (cond [(member? TOK-HOLD KEY-LIST) (add-token! (token 'KEY TOK-HOLD))
                                                 (reset-hold!)
                                                 (tokenFSM-start a-toker)]
                    [else (identity a-toker)])]))

;; identity : tokenizer -> token
;; creates an identity token

(define (identity a-toker)
  (add-token! (token 'IDENT TOK-HOLD))
  (reset-hold!)
  (tokenFSM-start a-toker))

;; symbol : tokenizer -> token
;; creates a symbol token

(define (symbol a-toker)
  (add-token! (token 'SYMBOL TOK-HOLD))
  (reset-hold!)
  (tokenFSM-start a-toker))

;; maybe-comment : tokenizer -> token
;; checks if the "/" is for a comment or a symbol

(define (maybe-comment a-toker)
  (cond [(or (is-backslash? (second a-toker))
             (string=? "*" (second a-toker))) (comment (rest a-toker))]
        [else (add-char! (first a-toker))
              (symbol (rest a-toker))]))

;; comment : tokenizer -> token
;; creates a comment token

(define (comment a-toker)
  (cond [(empty? a-toker) (add-token! (token 'COMMENT TOK-HOLD))
                          (set! TOK-HOLD "")
                          (tokenFSM-start a-toker)]
        [(is-backslash? (first a-toker)) (line-cmt (rest a-toker))]
        [(string=? "*" (first a-toker)) (in-line-cmt (rest a-toker))]))

;; ---------------------------------------------------------
;; comment helper functions

;; line-cmt : tokenizer -> token
;; produces the comment token for a line comment

(define (line-cmt a-toker) (token 'COMMENT "")
  (cond [(empty? a-toker) (reset-hold!)
                          (tokenFSM-start a-toker)]
        [(is-newline? (first a-toker)) (reset-hold!)
                                         (tokenFSM-start (rest a-toker))]
        [else (add-char! (first a-toker))
              (line-cmt (rest a-toker))]))

;; in-line-cmt: tokenizer -> token
;; produces the comment token for an in line or multi-line comment

(define (in-line-cmt a-toker) (token 'COMMENT "")
  (cond [(empty? a-toker) (reset-hold!)
                          (tokenFSM-start a-toker)]
        [(is-backslash? (first a-toker)) (reset-hold!)
                                         (tokenFSM-start (rest a-toker))]
        [(string=? "*" (first a-toker)) (in-line-cmt (rest a-toker))]
        [else (in-line-cmt (rest a-toker))]))

;; ---------------------------------------------------------
;; low-level tokenizer helper functions

;; member? : str list-of-strings -> Boolean
;; Determines if the string is in the list of strings

(check-expect (member? "this" '("something" "this" "yet" "test")) #t)
(check-expect (member? "this" '("something" "This" "false")) #f)

(define (member? a-str a-los)
  (ormap (λ (x) (string=? a-str x)) a-los))

;; reset-hold! : void -> void
;; resets TOK-HOLD to an empty string

(define (reset-hold!)
  (set! TOK-HOLD ""))

;; add-token! : token -> void
;; adds a token to the token list

(define (add-token! a-tok)
  (set! TOK-LIST (cons a-tok TOK-LIST)))

;; add-char! : 1-char-string -> void
;; sets TOK-HOLD as TOK-HOLD with the char appened to the end - assume a one char strin

(define (add-char! a-char)
  (set! TOK-HOLD (string-append TOK-HOLD a-char)))



;; ---------------------------------------------------------
;; character class predicates i.e. functions that evaluate whether a 
;; character falls into one of the basic classifications of characters
;; ASSUME: the input is a one-letter string

;; - :  1String -> Boolean

(define (is-digit? c)
  (char-numeric? (string-ref c 0)))

(define (is-paren? c)
  (or (char=? #\( (string-ref c 0)) (char=? #\) (string-ref c 0))))

(define (is-semicolon? c)
  (char=? #\; (string-ref c 0)))

(define (is-newline? c)
  (char=? #\newline (string-ref c 0)))

(define (is-quote? c)
  (char=? #\" (string-ref c 0)))

(define (is-letter? c)
  (char-alphabetic? (string-ref c 0)))

(define (is-whitespace? c)
  (char-whitespace? (string-ref c 0)))

(define (is-symbol? c)
  (or (char-symbolic? (string-ref c 0)) (char-punctuation? (string-ref c 0))))

(define (is-backslash? c)
  (char=? #\/ (string-ref c 0)))



;; -----------------------------------------------------------

(define (main)
  (define name
    (command-line #:args ([filename "Seven"]) filename))
  (define files (filter (λ (x) (string-contains? x ".jack")) (map (λ (x) (path->string x)) (directory-list name))))
  (define lines (map (λ (x) (read-file (string-append name "\\" x))) files))
  (define tokens (map (λ (x) (tokenFSM-start (explode x))) lines))
  (define translation (reverse (first (map class-dec tokens))))
  (printf "done")
  (define output-file-names (map (λ (x) (path-replace-suffix x ".vm")) files))
  (map (λ (x) (write-file (path->string x) (string-join translation "\n"))) output-file-names))

