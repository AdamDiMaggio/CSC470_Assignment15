;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname send_message_implement) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;constructors

(define class
  (lambda (base-class  list-of-field-names list-of-field-values list-of-method-names list-of-method-values)
    (letrec ((create-pairs-for-scope      ;recall here: letrec allows you define a function in terms of itself
              (lambda (lon lov)
                (cond
                  ((null? lon) '())
                  (else (cons
                         (list (car lon) (car lov))
                         (create-pairs-for-scope (cdr lon) (cdr lov)))))))
             (global-env (list (create-pairs-for-scope (append list-of-field-names list-of-method-names)       ;global-env  is a list of  name-value pairs (2 lists)
                                                       (append list-of-field-values list-of-method-values)))))
      (list 'class base-class global-env))))

;empty object is also a class(a base class)
;has no real parent, so null
(define empty-object
  (lambda ()
    (class '(no-parent)
      '()
      '()
      '(get)  ;list-of-method-names
      (list (lambda (name global-env)
              (apply-global-env name global-env))))));list-of-method-values


(define Person
  (lambda (list-of-fields list-of-values)
    (class (empty-object) list-of-fields list-of-values '() '())))
;created objects
(define person1 (Person '(fname lname age) '(Mike Litman 56)))
;(define person2 (Person '(fname lname age) '(John Smith 56)))
person1
;(sendMessage p2 'getValue ‘(Fname))
   

;Getters
;returns parent class of given class
(define class->parent-class
  (lambda (class)
    (cadr class)))

;returns global-env of class
(define class->global-env
  (lambda (class)
    (caddr class)))

;return base class/final parent class
(define class->base-class
  (lambda (class)
    (cond
      ((empty-object? class) class)
      (else (class->base-class (class->parent-class class))))))

;return first env in global
(define get-first-local-env
  (lambda (global-env)
    (car global-env)))

;return all but first envs in global
(define pop-local-env
  (lambda (global-env)
    (cdr global-env)))

;is this scope empty?
(define empty-scope?
  (lambda (scope)
    (if (null? (car scope)) #t #f)))

;get the name in this scope
(define scope->name
  (lambda (scope)
    (car scope)))

(define scope->value
  (lambda (scope)
    (cadr scope)))

;is the name bound in this scope, if so return the value
(define bound-in-scope?
  (lambda (name scope)
    (cond
     ((empty-scope? scope) #f)
     ((eq? name (scope->name scope)) #t)
     (else #f))))

(define apply-scope
  (lambda (name scope)
    (cond
      ((empty-scope? scope) #f)
      ((bound-in-scope? name scope) (scope->value scope))
      (else #f))))
                               

(define bound-in-local-env?
  (lambda (name local-env)
    (cond
      ((null? local-env) #f)
      ((bound-in-scope? name (car local-env)) #t)
      (else (bound-in-local-env? name (cdr local-env))))))

(define apply-local-env
  (lambda (name local-env)
    (cond
      ((null? local-env) #f)
      ((bound-in-local-env? name local-env) (if (bound-in-scope? name (car local-env))
                                                            (apply-scope name (car local-env))
                                                            (apply-local-env name (cdr local-env))))
      (else #f))))

(define bound-in-global-env?
  (lambda (name global-env)
    (cond
      ((null? global-env) #f)
      ((bound-in-local-env? name (car global-env)) #t)
      (else (bound-in-global-env? name (pop-local-env global-env))))))

(define apply-global-env
  (lambda (name global-env)
    (cond
      ((null? global-env) #f)
      ((bound-in-global-env? name global-env) (if (bound-in-local-env? name (car global-env))
                                                                       (apply-local-env name (car global-env))
                                                                       (apply-global-env name (cdr global-env)))) 
      (else #f))))


;returns value bound to the given message name in any class
(define class->method-value
  (lambda (class message-name)
    (cond
      ((eq? 'no-parent (car class)) #f)
      ((bound-in-global-env? message-name (class->global-env class)) (apply-global-env
                                                                      message-name
                                                                      (class->global-env class)))
      (else (class->method-value (class->parent-class class) message-name)))))
                                    
    

;asks a class whether it is the empty-object
(define empty-object?
  (lambda (class)
    (if (eq? 'no-parent (car (class->parent-class class))) #t #f)))

(define send-message
  (lambda (target-class message-name list-of-params)  ;target class we want to apply the message to, params will be in ()
    ((class->method-value target-class message-name) list-of-params (class->global-env target-class))))

;(bound-in-global-env? 'getValue (class->global-env person1))

(class->method-value person1 'get)
(send-message person1 'get 'lname)
;(class->parent-class person1)
;(define test (empty-object))
;test
;(class->parent-class test)
;(class->global-env person1)
;(get-first-local-env (class->global-env person1))
;(pop-local-env (class->global-env person1))
;(empty-object? (class->parent-class person1))

     






    
             