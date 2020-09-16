(import (scheme base)
        (srfi 1)
        (srfi 64))

(include "indexes.scm")
(include "internals.scm")

(define (alist? vec l)
  (and (list? l) (every pair? l)))

(define (alist-map! vec proc alist)
  (map 
    (lambda (e)
      (define key (car e))
      (define value (cdr e))
      (cons key (proc key value))) 
    alist))

(define (alist-filter! vec pred alist)
  (filter
    (lambda (e)
      (pred (car e) (cdr e))) 
    alist))

(define (alist-search! vec alist key failure success)
  (define (handle-success pair)
    (define old-key (car pair))
    (define old-value (cdr pair))
    (define (update new-key new-value obj)
      (cond 
        ((and (eq? old-key
                   new-key)
              (eq? old-value
                   new-value))
         (values alist obj))
        (else
          (let ((new-list 
                  (alist-cons
                    new-key new-value
                    (alist-delete old-key alist))))
            (values new-list obj)))))
    (define (remove obj)
      (values (alist-delete old-key alist) obj))
    (success old-key old-value update remove))
  
  (define (handle-failure)
    (define (insert value obj)
      (values (alist-cons key value alist)
              obj))
    (define (ignore obj)
      (values alist obj))
    (failure insert ignore))
  (cond
    ((assoc key alist) => handle-success)
    (else (handle-failure))))

(define (alist-size vec alist)
  (define keys (map car alist))
  (define (fold-proc el set)
    (lset-adjoin equal? set el))
  (define key-set (fold fold-proc '() keys))
  (length key-set))

(define (alist-foreach vec proc alist)
  (define (proc* e)
    (proc (car e) (cdr e)))
  (for-each proc* alist))


(define vec (vector-copy model-vec))
(vector-set! vec d? alist?)
(vector-set! vec dmap! alist-map!)
(vector-set! vec dfilter! alist-filter!)
(vector-set! vec dsearch! alist-search!)
(vector-set! vec dsize alist-size)
(vector-set! vec dfor-each alist-foreach)

(test-begin "Dictionary test")

(test-group
  "idictionary?"
  (test-assert (dcall d? vec '()))
  (test-assert (dcall d? vec '((a . b)))))

(test-group
  "idict-empty?"
  (test-assert (dcall dempty? vec '()))
  (test-assert (not (dcall dempty? vec '((a . b))))))

(test-group
  "idict-contains?"
  (test-assert (not (dcall dcontains? vec '() 'a)))
  (test-assert (not (dcall dcontains? vec '((b . c)) 'a)))
  (test-assert (dcall dcontains? vec '((a . b)) 'a)))

(test-group
  "idict-ref"
  (test-assert (dcall dref vec '((a . b)) 'a (lambda () #f) (lambda (x) #t)))
  (test-assert (dcall dref vec '((a . b)) 'b (lambda () #t) (lambda (x) #f))))

(test-group
  "idict-ref/default"
  (test-equal (dcall dref/default vec '((a . b)) 'a 'c) 'b)
  (test-equal (dcall dref/default vec '((a* . b)) 'a 'c) 'c))

(test-group
  "idict-set!"
  (define d (dcall dset! vec '((a . b)) 'a 'c 'a2 'b2))
  (test-equal '(a . c) (assoc 'a d))
  (test-equal '(a2 . b2) (assoc 'a2 d)))

(test-group
  "idict-adjoin!"
  (define d (dcall dset! vec '((a . b)) 'a 'c 'a2 'b2))
  (test-equal '(a . c) (assoc 'a d))
  (test-equal '(a2 . b2) (assoc 'a2 d)))

(test-group
  "idict-delete!"
  (define d (dcall ddelete! vec '((a . b) (c . d)) 'a 'b))
  (test-equal d '((c . d))))

(test-group
  "idict-delete-all!"
  (define d (dcall ddelete-all! vec '((a . b) (c . d)) '(a b)))
  (test-equal d '((c . d))))

(test-group
  "idict-replace!"
  (define d (dcall dreplace! vec '((a . b) (c . d)) 'a 'b2))
  (test-equal '(a . b2) (assoc 'a d))
  (test-equal '(c . d) (assoc 'c d)))

(test-group
  "idict-intern!"
  
  ;; intern existing
  (let ()
   (define-values 
     (d value) 
     (dcall dintern! vec '((a . b)) 'a (lambda () 'd))) 
   (test-equal '(a . b) (assoc 'a d))
   (test-equal 'b value))
  
  ;; intern missing
  (let ()
   (define-values
     (d value)
     (dcall dintern! vec '((a . b)) 'c (lambda () 'd)))
   (test-equal '(a . b) (assoc 'a d))
   (test-equal '(c . d) (assoc 'c d))
   (test-equal 'd value)))

(test-group
  "idict-update!"
  
  ;; update existing
  (let ()
   (define d (dcall dupdate! vec '((a . "b")) 'a 
                    (lambda (value) 
                      (string-append value "2"))
                    error
                    (lambda (x) (string-append x "1")))) 
   (test-equal '(a . "b12") (assoc 'a d)))
  
  ;; update missing
  (let ()
   (define d (dcall dupdate! vec '((a . "b")) 'c
                    (lambda (value) 
                      (string-append value "2"))
                    (lambda () "d1")
                    (lambda (x) (string-append x "1")))) 
   (test-equal '(c . "d12") (assoc 'c d))))

(test-group
  "idict-update/default!"
  ;; update existing
  (let ()
   (define d (dcall dupdate/default! vec '((a . "b")) 'a 
                    (lambda (value) 
                      (string-append value "2"))
                    "d1")) 
   (test-equal '(a . "b2") (assoc 'a d)))
  
  ;; update missing
  (let ()
   (define d (dcall dupdate/default! vec '((a . "b")) 'c
                    (lambda (value) 
                      (string-append value "2"))
                    "d1")) 
   (test-equal '(c . "d12") (assoc 'c d))))

(test-group
  "idict-pop!"
  (define-values
    (new-dict key value)
    (dcall dpop! vec '((a . b) (c . d)) error))
  (test-equal new-dict '((c . d)))
  (test-equal key 'a)
  (test-equal value 'b))

(test-group
  "idict-map!"
  (define d (dcall dmap! vec 
                   (lambda (key value)
                     (string-append value "2"))
                   '((a . "a") (b . "b"))))
  (test-equal '(a . "a2") (assoc 'a d))
  (test-equal '(b . "b2") (assoc 'b d)))

(test-group
  "idict-filter!"
  (define d (dcall dfilter! vec
                   (lambda (key value)
                     (equal? value 'b))
                   '((a . b) (c . d))))
  (test-equal '((a . b)) d))

(test-group
  "idict-remove!"
  (define d (dcall dremove! vec
                   (lambda (key value)
                     (equal? value 'b))
                   '((a . b) (c . d))))
  (test-equal '((c . d)) d))

(test-group
  "idict-search!"
  
  ;; ignore 
  (let ()
   (define-values 
     (dict value)
     (dcall dsearch! vec '((a . b)) 'c
            (lambda (insert ignore)
              (ignore 'foo))
            (lambda args
              (error))))
   (test-equal '((a . b)) dict)
   (test-equal value 'foo))
  
  ;; insert
  (let ()
   (define-values 
     (dict value)
     (dcall dsearch! vec '((a . b)) 'c
            (lambda (insert ignore)
              (insert 'd 'foo))
            (lambda args
              (error))))
   (test-equal '(a . b) (assoc 'a dict))
   (test-equal '(c . d) (assoc 'c dict))
   (test-equal value 'foo))
  
  ;; update
  (let ()
   (define-values 
     (dict value)
     (dcall dsearch! vec '((a . b)) 'a
            (lambda args
              (error))
            (lambda (key value update delete)
              (update 'a2 'b2 'foo))))
   (test-equal '((a2 . b2)) dict)
   (test-equal value 'foo))
  
  ;; delete
  (let ()
   (define-values 
     (dict value)
     (dcall dsearch! vec '((a . b) (c . d)) 'a
            (lambda args
              (error))
            (lambda (key value update delete)
              (delete 'foo))))
   (test-equal '((c . d)) dict)
   (test-equal value 'foo)))

(test-group
  "idict-size"
  (test-equal 2 (dcall dsize vec '((a . b) (c . d))))
  (test-equal 0 (dcall dsize vec '())))

(test-group
  "idict-for-each"
  (define lst '())
  (dcall dfor-each vec
         (lambda (key value)
           (set! lst (append lst (list key value))))
         '((a . b) (c . d)))
  (test-equal '(a b c d) lst))

(test-group
  "idict-count"
  (define count (dcall dcount vec
                       (lambda (key value)
                         (equal? value 'b))
                       '((a . b) (c . d))))
  (test-equal count 1))

(test-group
  "idict-any"
  
  (let ()
   (define value 
     (dcall dany vec
            (lambda (key value)
              (if (equal? 'b value) 'foo #f))
            '((a . b) (c . d))))
   (test-equal value 'foo))

  (let ()
   (define value 
     (dcall dany vec
            (lambda (key value)
              (if (equal? 'e value) 'foo #f))
            '((a . b) (c . d))))
   (test-equal value #f)))

(test-group
  "idict-every"
  (let ()
   (define value
     (dcall devery vec
            (lambda (key value)
              (if (equal? 'b value) 'foo #f))
            '((a . b) (c . b))))
   (test-equal value 'foo))
  
  (let ()
   (define value
     (dcall devery vec
            (lambda (key value)
              (if (equal? 'b value) 'foo #f))
            '()))
   (test-equal value #t))
  
  (let ()
   (define value
     (dcall devery vec
            (lambda (key value)
              (if (equal? 'b value) 'foo #f))
            '((a . b) (c . d))))
   (test-equal value #f)))

(test-group
  "idict-keys"
  (define keys
    (dcall dkeys vec '((a . b) (c . d))))
  (test-equal '(a c) keys))

(test-group
  "idict-values"
  (define vals
    (dcall dvalues vec '((a . b) (c . d))))
  (test-equal '(b d) vals))

(test-group
  "idict-entries"
  (define-values
    (keys vals)
    (dcall dentries vec '((a . b) (c . d))))
  (test-equal '(a c) keys)
  (test-equal '(b d) vals))

(test-group
  "idict-fold"
  (define value
    (dcall dfold vec
           (lambda (key value acc)
             (append acc (list key value)))
           '()
           '((a . b) (c . d))))
  (test-equal value '(a b c d)))

(test-group
  "idict-map->list"
  (define lst
    (dcall dmap->list vec
           (lambda (key value)
             (string-append (symbol->string key)
                            value))
           '((a . "b") (c . "d"))))
  (test-equal '("ab" "cd") lst))

(test-group
  "idict->alist"
  (define alist
    (dcall d->alist vec '((a . b) (c . d))))
  (test-equal alist '((a . b) (c . d))))

(test-end)
