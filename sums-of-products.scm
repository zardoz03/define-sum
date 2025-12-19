#!/bin/sh
# -*- mode: scheme -*-
# vim: ft=scheme
exec guile -s $0 $@
!#

(use-modules (scheme base))

(eval-when (eval load compile)
  (load "stx-format.scm"))

(define-syntax comment
  (lambda (stx)
    (syntax-case stx ()
      ((_ rest ...)
       #t))))

(define-syntax push!
  (syntax-rules ()
    ((_ newelt place)
     (set! place (cons newelt place)))))

(eval-when (eval load compile)
  (define (get-record-definition-cases stx name cases)
    (let ((list-cases (syntax->datum cases))
          (name-sym (syntax->datum name))
          (definitions '()))
      (let lp ((list-cases list-cases))
        (unless (null? list-cases)
          ;; for the example of  my-sum with '(quux frob) being the case
          (let* ((case-to-define (car list-cases))
                 ;; name-sym ::= 'quux
                 (name-sym 
                  (if (list? case-to-define)
                      (car case-to-define)
                      case-to-define))
                 ;; name-to-define ::= my-sum-quux
                 (name-to-define
                  (syntax-format stx "~a-~a"
                                 name
                                 name-sym))
                 ;; constructor-name ::= make-my-sum-quux
                 (constructor-name
                  (syntax-format stx "make-~a-~a"
                                 name name-sym))
                 ;; constructor-args ::= '(frob)
                 (constructor-args
                  (if (list? case-to-define)
                      (map (lambda (c) (datum->syntax stx c))
                           (cdr case-to-define))
                      '()))
                 ;; predicate-name ::= my-sum-quux? 
                 (predicate-name
                  (syntax-format stx "~a-~a?" name
                                 name-sym))
                 ;; fields-getters-spec ::= (frob my-sum-quux-frob)
                 (fields-getters-spec
                  (if (list? case-to-define)
                      (map
                       (lambda (x)
                         #`(#,x
                            #,(syntax-format stx "~a-~a-~a"
                                             name name-sym x)))
                       constructor-args)
                      '())))
            (push! 
             #`(define-record-type #,name-to-define
                 (#,constructor-name #,@constructor-args)
                 #,predicate-name
                 #,@fields-getters-spec)
             ;; i.e.
             #|
             (define-record-type my-sum-quux (make-my-sum-quux frob)
               my-sum-quux?
               (frob my-sum-quux-frob))
             |#
             definitions)
            (lp (cdr list-cases)))))
      definitions))
  (define (parent-type-predicate stx name cases)
    (let* ((parent-predicate-name
            (syntax-format stx "~A?" name))
           (sum-type
            (syntax-format stx "sum-type"))
           (checks
            (cons
             #`(#,(syntax-format stx "null-~a?" name) #,sum-type)
                (map
                 (lambda (c)
                   (let* ((case-name
                           (if (list? c)
                               (car c)
                               c))
                          (case-predicate
                           (syntax-format stx "~a-~a?"
                                          name case-name)))
                     #`(#,case-predicate #,sum-type)))
                 (syntax->datum cases)))))
      #`(define (#,parent-predicate-name #,sum-type)
          (or #,@checks))))
  (define (parent-type-constructor stx name cases)
    (let* ((parent-constructor-name
            (syntax-format stx "make-~a" name))
           ;; this is because of hygiene
           (field (syntax-format stx "field"))
           (args (syntax-format stx "args"))

           (cases-list (syntax->datum cases))
           (names
            (map (lambda (c) (if (list? c) (car c) c)) cases-list))
           (dispatch-rules
            (let ((out '()))
              (let lp ((names names))
                (unless (null? names)
                  (let* ((name-sym (datum->syntax stx (car names)))
                         (constructor
                          (syntax-format stx "make-~a-~a" name name-sym)))
                    (push!
                     #`((#,name-sym)
                        ;; maybe do arg-length checks here instead
                        ;; of failing later, idk
                        (apply #,constructor #,args))
                     out))
                  (lp (cdr names))))
              ;; simpler than trying to hack this onto `names'
              (push!
               #`((#,(syntax-format stx "null"))
                  (apply #,(syntax-format stx "make-null-~a" name) #,args))
               out)
              out)))
      #`(define (#,parent-constructor-name #,field . #,args)
          (case #,field
            #,@dispatch-rules))))
  (define (expand-define-sum stx name cases)
    (let ((record-definition-cases
           (get-record-definition-cases stx name cases))
          (null-constructor
           (syntax-format stx "make-null-~a" name))
          (null-predicate
           (syntax-format stx "null-~a?" name))
          (parent-constructor
           (parent-type-constructor stx name cases))
          (parent-predicate
           (parent-type-predicate stx name cases)))
      #`(begin
          (define-record-type #,name
            (#,null-constructor)
            #,null-predicate)
          #,@record-definition-cases
          #,parent-constructor
          #,parent-predicate)))
  #||#)

(define-syntax define-sum
  (lambda (stx)
    (syntax-case stx ()
      ((define-sum-macro name cases)
       (expand-define-sum stx #'name #'cases)))))

(comment
 (define-sum my-sum
   (foo
    bar
    (baz frob)
    (quux yrub herb)))
 => Effect<Definitions<
   my-sum
   my-sum-foo
   my-sum-bar
   my-sum-baz
   my-sum-baz-frob
   my-sum-quux
   my-sum-quux-yrub
   my-sum-quux-herb

   null-my-sum?
   my-sum?
   my-sum-foo?
   my-sum-bar?
   my-sum-baz?
   my-sum-quux?

   make-my-sum 
   make-null-my-sum
   make-my-sum-foo
   make-my-sum-bar
   make-my-sum-baz
   make-my-sum-quux
 >>

 (syntax->datum
  (get-record-definition-cases
   #'a 
   #'my-sum
   #'(foo bar (quux frob))))
 => #'((define-record-type my-sum-quux
         (make-my-sum-quux frob)
         my-sum-quux?
         (frob my-sum-quux-frob))
       (define-record-type my-sum-bar
         (make-my-sum-bar)
         my-sum-bar?)
       (define-record-type my-sum-foo
         (make-my-sum-foo)
         my-sum-foo?))

 (syntax->datum
  (parent-type-predicate #'a #'my-sum 
                         #'(foo bar (quux frob))))
 => 
 #'(define (my-sum? sum-type)
     (or (null-my-sum? sum-type)
         (my-sum-foo? sum-type)
         (my-sum-bar? sum-type)
         (my-sum-quux? sum-type)))

 (syntax->datum 
  (parent-type-constructor 
   #'a #'my-sum 
   #'(foo bar (quux frob))))
 =>
 #'(define (make-my-sum field . args)
     (case field
       ((quux) (apply make-my-sum-quux args))
       ((bar) (apply make-my-sum-bar args))
       ((foo) (apply make-my-sum-foo args))
       ((null) (apply make-null-my-sum args))))

 (define-sum my-sum
   (foo
    bar
    (baz frob)
    (quux yrub herb)))

 (format #t "~A~%" (my-sum-baz-frob (make-my-sum-baz 2)))
 => Print<"2\n">
 
 (make-my-sum 'null)
 => #<my-sum>

 (make-my-sum-quux 1 2)
 => #<my-sum-quux yrub: 1 herb: 2>
 (make-my-sum 'quux 1 2)
 => #<my-sum-quux yrub: 1 herb: 2>
 #||#)
