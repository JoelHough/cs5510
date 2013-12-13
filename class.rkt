#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [plusC (lhs : ExprC)
         (rhs : ExprC)]
  [multC (lhs : ExprC)
         (rhs : ExprC)]
  [argC]
  [thisC]
  [nullC] ; add null for #7
  [newC (class-name : symbol)
        (args : (listof ExprC))]
  [getC (obj-expr : ExprC)
        (field-name : symbol)]
  [sendC (obj-expr : ExprC)
         (method-name : symbol)
         (arg-expr : ExprC)]
  [ssendC (obj-expr : ExprC)
          (class-name : symbol)
          (method-name : symbol)
          (arg-expr : ExprC)]
  [if0C (if-expr : ExprC); adding if0 for #3
        (then-expr : ExprC)
        (else-expr : ExprC)]
  [instanceofC (obj-exp : ExprC) ; add instanceof for #2
               (instanceof? : (symbol -> boolean))]
  [castC (valid? : (symbol -> boolean)) ; add cast for #5
         (obj-exp : ExprC)])

(define-type ClassC
  [classC (name : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodC))])

(define-type MethodC
  [methodC (name : symbol)
           (body-expr : ExprC)])

(define-type Value
  [numV (n : number)]
  [objV (class-name : symbol)
        (field-values : (listof Value))]
  [nullV]) ; add null for #7

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (make-find [name-of : ('a -> symbol)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'a
    (cond
     [(empty? vals)
      (error 'find "not found")]
     [else (if (equal? name (name-of (first vals)))
               (first vals)
               ((make-find name-of) name (rest vals)))])))

(define find-class : (symbol (listof ClassC) -> ClassC)
  (make-find classC-name))

(define find-method : (symbol (listof MethodC) -> MethodC)
  (make-find methodC-name))

;; A non-list pair:
(define-type (Pair 'a 'b)
  [kons (first : 'a) (rest : 'b)])

(define (get-field [name : symbol] 
                   [field-names : (listof symbol)] 
                   [vals : (listof Value)])
  ;; Pair fields and values, find by field name,
  ;; then extract value from pair
  (kons-rest ((make-find kons-first)
              name
              (map2 kons field-names vals))))

(module+ test
  (test/exn (find-class 'a empty)
            "not found")
  (test (find-class 'a (list (classC 'a empty empty)))
        (classC 'a empty empty))
  (test (find-class 'b (list (classC 'a empty empty)
                             (classC 'b empty empty)))
        (classC 'b empty empty))
  (test (get-field 'a 
                   (list 'a 'b)
                   (list (numV 0) (numV 1)))
        (numV 0)))

;; ----------------------------------------

(define interp : (ExprC (listof ClassC) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case ExprC a
        [numC (n) (numV n)]
        [plusC (l r) (num+ (recur l) (recur r))]
        [multC (l r) (num* (recur l) (recur r))]
        [thisC () this-val]
        [argC () arg-val]
        [nullC () (nullV)]
        [if0C (i t e) (type-case Value (recur i) ; added if0 for #3
                        [numV (n) (if (= 0 n) (recur t) (recur e))]
                        [else (error 'interp "not a number")])]
        [instanceofC (obj-expr instanceof?) (type-case Value (recur obj-expr) ; add instanceof for #2
                                              [objV (obj-class-name field-vals)
                                                    (if (instanceof? obj-class-name)
                                                        (numV 1)
                                                        (numV 0))]
                                              [nullV () (numV 1)] ; add null for #7 (null is an instance of anything)
                                              [else (error 'interp "not an object")])]
        [castC (valid? obj-expr) (local [(define v (recur obj-expr))]
                                   (type-case Value (recur obj-expr) ; add cast for #5
                                     [objV (obj-class-name field-vals)
                                           (if (valid? obj-class-name)
                                               v
                                               (error 'interp "not a valid cast"))]
                                     [nullV () (nullV)] ; add null for #7 (null can be cast to anything)
                                     [else (error 'interp "not an object")]))]
        [newC (class-name field-exprs)
              (local [(define c (find-class class-name classes))
                      (define vals (map recur field-exprs))]
                (if (= (length vals) (length (classC-field-names c)))
                    (objV class-name vals)
                    (error 'interp "wrong field count")))]
        [getC (obj-expr field-name)
              (type-case Value (recur obj-expr)
                [objV (class-name field-vals)
                      (type-case ClassC (find-class class-name classes)
                        [classC (name field-names methods)
                                (get-field field-name field-names 
                                           field-vals)])]
                [nullV () (error 'interp "null reference")] ; add null for #7
                [else (error 'interp "not an object")])]
        [sendC (obj-expr method-name arg-expr)
               (local [(define obj (recur obj-expr))
                       (define arg-val (recur arg-expr))]
                 (type-case Value obj
                   [objV (class-name field-vals)
                         (call-method class-name method-name classes
                                      obj arg-val)]
                   [nullV () (error 'interp "null reference")] ; add null for #7
                   [else (error 'interp "not an object")]))]
        [ssendC (obj-expr class-name method-name arg-expr)
                (local [(define obj (recur obj-expr))
                        (define arg-val (recur arg-expr))]
                  (type-case Value obj ; add null for #7
                    [objV (n f) (call-method class-name method-name classes
                                             obj arg-val)]
                    [nullV () (error 'interp "null reference")]
                    [else (error 'interp "not an object")]))]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case ClassC (find-class class-name classes)
    [classC (name field-names methods)
            (type-case MethodC (find-method method-name methods)
              [methodC (name body-expr)
                       (interp body-expr
                               classes
                               obj
                               arg-val)])]))

(define (num-op [op : (number number -> number)]
                [op-name : symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (classC 
     'posn
     (list 'x 'y)
     (list (methodC 'mdist
                    (plusC (getC (thisC) 'x) (getC (thisC) 'y)))
           (methodC 'addDist
                    (plusC (sendC (thisC) 'mdist (numC 0))
                           (sendC (argC) 'mdist (numC 0))))
           (methodC 'addX
                    (plusC (getC (thisC) 'x) (argC)))
           (methodC 'multY (multC (argC) (getC (thisC) 'y)))
           (methodC 'factory12 (newC 'posn (list (numC 1) (numC 2)))))))

  (define posn3D-class
    (classC 
     'posn3D
     (list 'x 'y 'z)
     (list (methodC 'mdist (plusC (getC (thisC) 'z)
                                  (ssendC (thisC) 'posn 'mdist (argC))))
           (methodC 'addDist (ssendC (thisC) 'posn 'addDist (argC))))))

  (define posn27 (newC 'posn (list (numC 2) (numC 7))))
  (define posn531 (newC 'posn3D (list (numC 5) (numC 3) (numC 1))))

  (define (posn? name) ; added instanceof for #2
    (equal? name 'posn))
  
  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (if0C (numC 0) (numC 2) (numC 3)) ; added if0 for #3
                empty (numV -1) (numV -1))
        (numV 2))
  (test (interp (if0C (numC 1) (numC 2) (numC 3)) ; added if0 for #3
                empty (numV -1) (numV -1))
        (numV 3))
  (test (interp-posn (instanceofC posn27 posn?)) ; added instanceof for #2
        (numV 1))
  (test (interp-posn (instanceofC posn531 posn?)) ; added instanceof for #2
        (numV 0))
  (test/exn (interp-posn (instanceofC (numC 1) posn?))
        "not an object")

  (test (interp-posn (castC posn? posn27)) ; added cast for #2
        (objV 'posn (list (numV 2) (numV 7))))
  (test/exn (interp-posn (castC posn? (newC 'posn3D (list (numC 2) (numC 2) (numC 2)))))
            "not a valid cast")
  (test/exn (interp-posn (castC posn? (numC 1)))
            "not an object")
  
  ;; add null for #7
  (test (interp-posn (castC posn? (nullC)))
        (nullV))
  (test (interp-posn (instanceofC (nullC) posn?))
        (numV 1))
  (test (interp-posn (if0C (numC 0) (nullC) posn27))
        (nullV))
  (test/exn (interp-posn (getC (nullC) 'x))
            "null reference")
  (test/exn (interp-posn (sendC (nullC) 'mdist (numC 0)))
            "null reference")
  (test/exn (interp-posn (ssendC (nullC) 'posn 'mdist (numC 0)))
            "null reference")
  
  (test (interp (numC 10) 
                empty (numV -1) (numV -1))
        (numV 10))
  (test (interp (plusC (numC 10) (numC 17))
                empty (numV -1) (numV -1))
        (numV 27))
  (test (interp (multC (numC 10) (numC 7))
                empty (numV -1) (numV -1))
        (numV 70))

  (test (interp-posn (newC 'posn (list (numC 2) (numC 7))))
        (objV 'posn (list (numV 2) (numV 7))))

  (test (interp-posn (sendC posn27 'mdist (numC 0)))
        (numV 9))
  
  (test (interp-posn (sendC posn27 'addX (numC 10)))
        (numV 12))

  (test (interp-posn (sendC (ssendC posn27 'posn 'factory12 (numC 0))
                            'multY
                            (numC 15)))
        (numV 30))

  (test (interp-posn (sendC posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (if0C posn27 (numC 2) (numC 3))) ; added if0 for #3
            "not a number")
  (test/exn (interp-posn (plusC (numC 1) posn27))
            "not a number")
  (test/exn (interp-posn (getC (numC 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendC (numC 1) 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (ssendC (numC 1) 'posn 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (newC 'posn (list (numC 0))))
            "wrong field count"))
