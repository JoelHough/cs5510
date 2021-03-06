#lang plai-typed

;; Make all "class.rkt" definitions available here, where
;; the "class.rkt" file must be in the same directory
;; as this one:
(require "class.rkt")

(define-type ExprI
  [numI (n : number)]
  [plusI (lhs : ExprI)
         (rhs : ExprI)]
  [multI (lhs : ExprI)
         (rhs : ExprI)]
  [argI]
  [thisI]
  [nullI] ; add null for #7
  [newI (class-name : symbol)
        (args : (listof ExprI))]
  [getI (obj-expr : ExprI)
        (field-name : symbol)]
  [sendI (obj-expr : ExprI)
         (method-name : symbol)
         (arg-expr : ExprI)]
  [superI (method-name : symbol)
          (arg-expr : ExprI)]
  [if0I (i : ExprI) ; added if0 for #3
        (t : ExprI)
        (e : ExprI)]
  [instanceofI (obj-expr : ExprI) ; add instanceof for #2
               (class-name : symbol)]
  [castI (class-name : symbol) ; add cast for #5
         (obj-expr : ExprI)])

(define-type ClassI
  [classI (name : symbol)
          (super-name : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodI))])

(define-type MethodI
  [methodI (name : symbol)
           (body-expr : ExprI)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------
(define (instanceof? [instance-class-name : symbol] [class-name : symbol] [i-classes : (listof ClassI)]) ; add instanceof for #2
  (cond
    [(equal? instance-class-name class-name) true]
    [(equal? instance-class-name 'object) false]
    [else (type-case ClassI (find-i-class instance-class-name i-classes)
            [classI (name super-name field-names i-methods)
                    (instanceof? super-name class-name i-classes)])]))

(define (expr-i->c [a : ExprI] [super-name : symbol] [i-classes : (listof ClassI)]) : ExprC ; add instanceof for #2
  (local [(define (recur expr)
            (expr-i->c expr super-name i-classes))] ; add instanceof for #2
    (type-case ExprI a
      [numI (n) (numC n)]
      [plusI (l r) (plusC (recur l) (recur r))]
      [multI (l r) (multC (recur l) (recur r))]
      [argI () (argC)]
      [thisI () (thisC)]
      [nullI () (nullC)] ; add null for #7
      [if0I (i t e) (if0C (recur i) (recur t) (recur e))] ; added if0 for #3
      [instanceofI (obj-expr class-name) (instanceofC (recur obj-expr) (λ (obj-class-name) (instanceof? obj-class-name class-name i-classes)))] ; add instanceof for #2
      [castI (class-name obj-expr) (castC (λ (obj-class-name) (instanceof? obj-class-name class-name i-classes)) (recur obj-expr))] ; add cast for #5
      [newI (class-name field-exprs)
            (newC class-name (map recur field-exprs))]
      [getI (expr field-name)
            (getC (recur expr) field-name)]
      [sendI (expr method-name arg-expr)
             (sendC (recur expr)
                    method-name
                    (recur arg-expr))]
      [superI (method-name arg-expr)
              (ssendC (thisC)
                      super-name
                      method-name
                      (recur arg-expr))])))

(module+ test
  (test (expr-i->c (nullI) 'object empty) ; add null for #7
        (nullC))
  (test (expr-i->c (if0I (numI 0) (numI 2) (numI 3)) 'object empty) ; added if0 for #3
        (if0C (numC 0) (numC 2) (numC 3)))
  (test (expr-i->c (numI 10) 'object empty)
        (numC 10))
  (test (expr-i->c (plusI (numI 10) (numI 2)) 'object empty)
        (plusC (numC 10) (numC 2)))
  (test (expr-i->c (multI (numI 10) (numI 2)) 'object empty)
        (multC (numC 10) (numC 2)))
  (test (expr-i->c (argI) 'object empty)
        (argC))
  (test (expr-i->c (thisI) 'object empty)
        (thisC))
  (test (expr-i->c (newI 'object (list (numI 1))) 'object empty)
        (newC 'object (list (numC 1))))
  (test (expr-i->c (getI (numI 1) 'x) 'object empty)
        (getC (numC 1) 'x))
  (test (expr-i->c (sendI (numI 1) 'mdist (numI 2)) 'object empty)
        (sendC (numC 1) 'mdist (numC 2)))
  (test (expr-i->c (superI 'mdist (numI 2)) 'posn empty)
        (ssendC (thisC) 'posn 'mdist (numC 2))))

;; ----------------------------------------

(define (method-i->c [m : MethodI] [super-name : symbol] [i-classes : (listof ClassI)]) : MethodC
  (type-case MethodI m
    [methodI (name body-expr) 
             (methodC name 
                      (expr-i->c body-expr super-name i-classes))]))

(module+ test
  (define posn3d-mdist-i-method
    (methodI 'mdist
             (plusI (getI (thisI) 'z)
                    (superI 'mdist (argI)))))
  (define posn3d-mdist-c-method
    (methodC 'mdist
             (plusC (getC (thisC) 'z)
                    (ssendC (thisC) 'posn 'mdist (argC)))))

  (test (method-i->c posn3d-mdist-i-method 'posn empty)
        posn3d-mdist-c-method))

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI] [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassI c
    [classI (name super-name field-names methods)
            (classC
             name
             field-names
             (map (lambda (m) (method-i->c m super-name i-classes))
                  methods))]))

(module+ test
  (define posn3d-i-class 
    (classI 'posn3d
            'posn
            (list 'z)
            (list posn3d-mdist-i-method)))
  (define posn3d-c-class-not-flat
    (classC 'posn3d
            (list 'z)
            (list posn3d-mdist-c-method)))
  
  (test (class-i->c-not-flat posn3d-i-class empty)
        posn3d-c-class-not-flat))

;; ----------------------------------------

(define (flatten-class [c : ClassC] 
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassC c
    [classC (name field-names methods)
            (type-case ClassC (flatten-super name classes i-classes)
              [classC (super-name super-field-names super-methods)
                      (classC
                       name
                       (add-fields super-field-names field-names)
                       (add/replace-methods super-methods methods))])]))

(define (flatten-super [name : symbol]
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassI (find-i-class name i-classes)
    [classI (name super-name field-names i-methods)
            (if (equal? super-name 'object)
                (classC 'object empty empty)
                (flatten-class (find-class super-name classes)
                               classes
                               i-classes))]))

(module+ test
  (define posn-i-class 
    (classI 'posn
            'object
            (list 'x 'y)
            (list (methodI 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                  (methodI 'addDist
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))
  (define addDist-c-method
    (methodC 'addDist
             (plusC (sendC (thisC) 'mdist (numC 0))
                    (sendC (argC) 'mdist (numC 0)))))
  (define posn-c-class-not-flat
    (classC 'posn
            (list 'x 'y)
            (list (methodC 'mdist
                           (plusC (getC (thisC) 'x)
                                  (getC (thisC) 'y)))
                  addDist-c-method)))
  (define posn3d-c-class
    (classC 'posn3d
            (list 'x 'y 'z)
            (list posn3d-mdist-c-method
                  addDist-c-method)))

  (test (flatten-class posn3d-c-class-not-flat
                       (list posn-c-class-not-flat
                             posn3d-c-class-not-flat)
                       (list posn-i-class
                             posn3d-i-class))
        posn3d-c-class))

;; ----------------------------------------

(define add-fields append)

(define (add/replace-methods [methods : (listof MethodC)]
                             [new-methods : (listof MethodC)])
  : (listof MethodC)
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

(define (add/replace-method [methods : (listof MethodC)] 
                            [new-method : MethodC])
  : (listof MethodC)
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (methodC-name (first methods))
                 (methodC-name new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))

  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (methodC 'm (numC 0))))
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0))) empty)
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))))
  (test (add/replace-methods (list (methodC 'm (numC 0))
                                   (methodC 'n (numC 2)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))
                                   (methodC 'n (numC 2))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))

  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'm (numC 1)))
        (list (methodC 'm (numC 1))))
  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'n (numC 2)))
        (list (methodC 'm (numC 0))
              (methodC 'n (numC 2)))))

;; ----------------------------------------

(define find-i-class : (symbol (listof ClassI) -> ClassI)
  (make-find classI-name))

;; ----------------------------------------

(define (interp-i [i-a : ExprI] [i-classes : (listof ClassI)]) : Value
  (local [(define a (expr-i->c i-a 'object i-classes))
          (define classes-not-flat (map (λ (i-class) (class-i->c-not-flat i-class i-classes)) i-classes))
          (define classes
            (map (lambda (c)
                   (flatten-class c classes-not-flat i-classes))
                 classes-not-flat))]
    (interp a classes (numV -1) (numV -1))))

(module+ test
  (test (interp-i (instanceofI (newI 'posn3d (list (numI 5) (numI 3) (numI 1))) 'posn) ; add instanceof for #2
                  (list posn-i-class
                        posn3d-i-class))
        (numV 1))
  (test (interp-i (castI 'posn (newI 'posn3d (list (numI 5) (numI 3) (numI 1)))) ; add cast for #5
                  (list posn-i-class
                        posn3d-i-class))
        (objV 'posn3d (list (numV 5) (numV 3) (numV 1))))
  (test (interp-i (instanceofI (newI 'posn (list (numI 2) (numI 7))) 'posn3d)
                  (list posn-i-class
                        posn3d-i-class))
        (numV 0))
  (test (interp-i (numI 0) empty)
        (numV 0))

  (test (interp-i
         (sendI (newI 'posn3d (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'posn (list (numI 2) (numI 7))))
         (list posn-i-class
               posn3d-i-class))
        (numV 18)))
