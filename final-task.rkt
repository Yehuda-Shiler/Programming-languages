#lang pl
;; question 1)
#| the BNF define the language rules.

;; intersect and union get <SOL> as parameters to let the BNF be recursive

<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>} 
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment
<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set
;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;;define constructors for SOL type: according to the BNF implementation:
;; Inter and Union constructors: get SOL according to my BNF .
;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; 
    [Set  SET]
    [Smult Number SOL]
    [Inter SOL SOL]
    [Union SOL SOL]
    [Id    Symbol]
;;  [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
    [Fun   Symbol Symbol SOL]
    [CallS SOL SOL SOL]
    [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; question 2)
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 


;; the function check if the list of numbers contain the specipic number
;;the function used be recursive and check any time the first number in list
;;if is equal is true, else we check the rest of the list.

  (: ismember? : Number SET  -> Boolean)
  (define (ismember? n l)
    (cond [(null? l) #f]
          [(= n (first l)) #t]
          [else (ismember? n (rest l))]))

  (test (not (ismember? 1 '(3 4 5))))
  (test (not (ismember? 1 '( 3 2 3 5 6))))
  (test (ismember? 1 '(3 4 5 1 3 4)))
  (test (ismember? 1 '(1)))

#|The function remove duplicate numbers. This is a recursive funtion. 
The first condition check if there are empty list or 1 element in list
The second condition check if the first number is exist in the list then we skip it and sent the rest of the list.
The third condition builds the new set.

|#
  (: remove-duplicates : SET  -> SET)
  (define (remove-duplicates l)
    (cond [(or (null? l) (null? (rest l))) l]
          [(and (ismember? (first l) (rest l)))(remove-duplicates (rest l))]
          [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5)) => '(3 4 5))
(test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(1 1 1 1 2 2 2 2)) => '(1 2))

#|the function acept a list remove the duplicate number and sort by size
the funcion uses a previos functions
|#

 (: create-sorted-set : SET -> SET)
  (define (create-sorted-set l)
    (sort (remove-duplicates l) <))

(test (create-sorted-set '(3 2 7 3)) => '(2 3 7))
(test (create-sorted-set '(1 2 3)) => '(1 2 3))
(test (create-sorted-set '(1)) => '(1))
(test (create-sorted-set '(9 8 9 8 1)) => '(1 8 9))
(test (create-sorted-set '(1 1 1 1)) => '(1))
(test (create-sorted-set '(2 3 4 2 3 4 )) => '(2 3 4))

 #|The function uses the racket built in append function to concatenate the lists.
Then I use the previous implementation to sort and remove duplicates.
|#
  (: set-union : SET SET -> SET)
  (define (set-union A B)
     (create-sorted-set (append A B)))

(test (set-union '(3 2 7 3)'(1 2 5 3)) => '(1 2 3 5 7))
(test (set-union '(1 2)'(2 1)) => '(1 2))
(test (set-union '(9 7 1 3)'(1 7)) => '(1 3 7 9))
(test (set-union '()'(1  5 3)) => '(1 3 5))
(test (set-union '(3 3 3)'(1 1 1 2)) => '(1 2 3))

#|
This function uses filter built in function and gives it two arguments: a function and a list.
It returns all the members that are exist in the A list.
|#
  (: set-intersection : SET SET -> SET)
  (define (set-intersection A B)
    (: mem-filter : Number -> Boolean)
    (define (mem-filter n)
      (ismember? n A))
    (sort (filter mem-filter B)<))

(test (set-intersection '(3 2 7 3)'(1 2 5 3)) => '(2 3))
(test (set-intersection '(1 2)'(2 1)) => '(1 2))
(test (set-intersection '(9 7 1 3)'(1 7)) => '(1 7))
(test (set-intersection '()'(1  5 3)) => '())
(test (set-intersection '(3 3 3)'(1 1 1 2 3)) => '(3))
;; ---------------------------------------------------------
;;question 3)
;; Parser

  (: parse-sexpr : Sexpr -> SOL)
  ;; to convert s-expressions into SOLs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; return list with function create-sorted-set which sort and remove-duplicates the list.
      [(symbol: name) (Id name)]
      [(cons 'with more);;'with is a syntatic sugar and we use in Call. Call is equals to 'with' in the way that both can take an expression and an ID and replace the ID within the expression with another expression
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (CallS (Fun name name (parse-sexpr body))(parse-sexpr named)(parse-sexpr named))] ;;; there is no With constructor replace with existing constructors
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (eq? name1 name2)
              (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice ;;We need to throw an exception from the test section
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
      [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
      ;; CallS and CallD in the parsing level: need to call to the right constructor: CallS/CallD
      ;; and then to pass the arguments after we call parse-sexpr because we want to convert them to SOL (from s-expression)
      [(list 'call-static fun arg1 arg2)(CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
      [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))];;
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


    


  (: parse : String -> SOL)
  ;; parses a string containing a SOL expression to a SOL AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{scalar-mult 2 {4 2 3}}") => (Smult 2 (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}") => (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
(test (parse "{call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}") => (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


;;-----------------------------------------------------
;;question 4)
;; Evaluation 
#|


;; call-static and call-dynamic are basicallt the same but to one difference. In call-static we save the local environment and in call-dynamic
;; we work with thw same environment. But in both we extend the environment by saving the key-value pairs.

Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below
    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env) ,(extend(x1, eval(E1,env),envf)))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env)
             = eval(Ef,(extend(x2,eval(E2,env),(extend(x1, eval(E1,env),env)))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
|#

;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 
;; The purpose of this function is to convert a special type SetV to a SOL - out language type 
  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))
  ;;Here we want to implement a function that will nultiply all elements in the list by scalar.
  ;; The easiet way is to convert to a SET because we want to use in Racket built in map function and the
  ;;convert it again to SetV
  (: smult-set : Number VAL -> VAL)
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))
    (SetV (map mult-op (SetV->set s))))

;;Here we apply a binary SET operation, but in order to use it need to do the conversion from SetV To set and vice versa.
(: set-op :(SET SET -> SET) VAL VAL -> VAL )
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV S)];; eval return VAL so need to convert to the matched type
      [(Smult n set) (smult-set n (eval set env))];; Here want to multiply by scalar - but set is an SOL need to convert it to VAL and call eval.
      [(Inter l r) (set-op set-intersection(eval l env)(eval r env))];; Need to convert to VAL but after calling eval (so will get the "final" value)
      [(Union l r) (set-op set-union(eval l env)(eval r env))];; the same as Inter
      [(Id name) (lookup name env)]
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)]
       ;; Here we must extend our environment becuse each time we call eval we want to look-up for the right instance
      [(CallS fun-expr arg-expr1 arg-expr2);; In Call we validate that the first argument is a function
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
            (eval bound-body (Extend bound-id2 (eval arg-expr2 env)(Extend bound-id1 (eval arg-expr1 env) f-env)))]
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      [(CallD fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
           (eval bound-body (Extend bound-id2 (eval arg-expr2 env)(Extend bound-id1 (eval arg-expr1 env) env)))]
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))


(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend 'second
          (FunV 'x 'y (CallS (Id 'x) (Fun 'x 'y (Id 'y)) (Set '())) (EmptyEnv))
          (Extend 'first
                  (FunV 'x 'y (CallS (Id 'y) (Fun 'x 'y (Id 'x)) (Set '())) (EmptyEnv))
                  (Extend 'cons
                          (FunV 'x 'y (Fun 'x 'y (CallS (Id 'x) (Id 'x) (Id 'y))) (EmptyEnv))
                                (EmptyEnv)))))


;; run wrap all together: we start from a string and then call all the functions: parsing and then eval. 
;; Run function must return a SET because - in the end we want to have an SOL
  (: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str) (createGlobalEnv))])
       (cases result
         [(SetV S) S]
         [else (error 'run "run expects a Set, got: ~s" result)])))




(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
