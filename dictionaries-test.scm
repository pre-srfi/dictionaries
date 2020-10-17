(import (scheme base)
        (srfi 1)
        (srfi 64)
        (dictionaries))

(define (do-test alist->dict)

  (test-group
    "dictionary?"
    (test-assert (dictionary? (alist->dict '())))
    (test-assert (dictionary? (alist->dict '((a . b))))))

  (test-group
    "dict-empty?"
    (test-assert (dict-empty? (alist->dict '())))
    (test-assert (not (dict-empty? (alist->dict '((a . b)))))))

  (test-group
    "dict-contains?"
    (test-assert (not (dict-contains? (alist->dict '()) 'a)))
    (test-assert (not (dict-contains? (alist->dict '((b . c))) 'a)))
    (test-assert (dict-contains? (alist->dict '((a . b))) 'a)))

  (test-group
    "dict-ref"
    (test-assert (dict-ref (alist->dict '((a . b))) 'a (lambda () #f) (lambda (x) #t)))
    (test-assert (dict-ref (alist->dict '((a . b))) 'b (lambda () #t) (lambda (x) #f))))

  (test-group
    "dict-ref/default"
    (test-equal (dict-ref/default (alist->dict '((a . b))) 'a 'c) 'b)
    (test-equal (dict-ref/default (alist->dict '((a* . b))) 'a 'c) 'c))

  (test-group
    "dict-set!"
    (define d (dict-set! (alist->dict '((a . b))) 'a 'c 'a2 'b2))
    (test-equal 'c (dict-ref d 'a ))
    (test-equal 'b2 (dict-ref d 'a2)))

  (test-group
    "dict-adjoin!"
    (define d (dict-adjoin! (alist->dict '((a . b))) 'a 'c 'a2 'b2))
    (test-equal 'b (dict-ref d 'a))
    (test-equal 'b2 (dict-ref d 'a2)))

  (test-group
    "dict-delete!"
    (define d (dict-delete! (alist->dict '((a . b) (c . d))) 'a 'b))
    (test-equal (dict->alist d) '((c . d))))

  (test-group
    "dict-delete-all!"
    (define d (dict-delete-all! (alist->dict '((a . b) (c . d))) '(a b)))
    (test-equal (dict->alist d) '((c . d))))

  (test-group
    "dict-replace!"
    (define d (dict-replace! '((a . b) (c . d)) 'a 'b2))
    (test-equal 'b2 (dict-ref d 'a))
    (test-equal 'd (dict-ref d 'c)))

  (test-group
    "dict-intern!"

    ;; intern existing
    (let ()
     (define-values 
       (d value) 
       (dict-intern! (alist->dict '((a . b))) 'a (lambda () 'd))) 
     (test-equal 'b (dict-ref d 'a))
     (test-equal 'b value))

    ;; intern missing
    (let ()
     (define-values
       (d value)
       (dict-intern! (alist->dict '((a . b))) 'c (lambda () 'd)))
     (test-equal 'b (dict-ref d 'a))
     (test-equal 'd (dict-ref d 'c))
     (test-equal 'd value)))

  (test-group
    "dict-update!"

    ;; update existing
    (let ()
     (define d (dict-update! (alist->dict '((a . "b"))) 'a 
                      (lambda (value) 
                        (string-append value "2"))
                      error
                      (lambda (x) (string-append x "1")))) 
     (test-equal "b12" (dict-ref d 'a)))

    ;; update missing
    (let ()
     (define d (dict-update! (alist->dict '((a . "b"))) 'c
                      (lambda (value) 
                        (string-append value "2"))
                      (lambda () "d1")
                      (lambda (x) (string-append x "1")))) 
     (test-equal "d12" (dict-ref d 'c))))

  (test-group
    "dict-update/default!"
    ;; update existing
    (let ()
     (define d (dict-update/default!  (alist->dict '((a . "b"))) 'a 
                      (lambda (value) 
                        (string-append value "2"))
                      "d1")) 
     (test-equal "b2" (dict-ref d 'a)))

    ;; update missing
    (let ()
     (define d (dict-update/default!  (alist->dict '((a . "b"))) 'c
                      (lambda (value) 
                        (string-append value "2"))
                      "d1")) 
     (test-equal "d12" (dict-ref d 'c))))

  (test-group
    "dict-pop!"
    (define-values
      (new-dict key value)
      (dict-pop! (alist->dict '((a . b) (c . d))) error))
    (test-equal (dict->alist new-dict) '((c . d)))
    (test-equal key 'a)
    (test-equal value 'b))

  (test-group
    "dict-map!"
    (define d (dict-map!  
                     (lambda (key value)
                       (string-append value "2"))
                     (alist->dict '((a . "a") (b . "b")))))
    (test-equal "a2" (dict-ref d 'a))
    (test-equal "b2" (dict-ref d 'b)))

  (test-group
    "dict-filter!"
    (define d (dict-filter! 
                     (lambda (key value)
                       (equal? value 'b))
                     (alist->dict '((a . b) (c . d)))))
    (test-equal '((a . b)) (dict->alist d)))

  (test-group
    "dict-remove!"
    (define d (dict-remove! 
                     (lambda (key value)
                       (equal? value 'b))
                     (alist->dict '((a . b) (c . d)))))
    (test-equal '((c . d)) (dict->alist d)))

  (test-group
    "dict-search!"

    ;; ignore 
    (let ()
     (define-values 
       (dict value)
       (dict-search!  '((a . b)) 'c
              (lambda (insert ignore)
                (ignore 'foo))
              (lambda args
                (error))))
     (test-equal '((a . b)) (dict->alist dict))
     (test-equal value 'foo))

    ;; insert
    (let ()
     (define-values 
       (dict value)
       (dict-search! (alist->dict '((a . b))) 'c
              (lambda (insert ignore)
                (insert 'd 'foo))
              (lambda args
                (error))))
     (test-equal 'b (dict-ref dict 'a))
     (test-equal 'd (dict-ref dict 'c))
     (test-equal value 'foo))

    ;; update
    (let ()
     (define-values 
       (dict value)
       (dict-search! (alist->dict '((a . b))) 'a
              (lambda args
                (error))
              (lambda (key value update delete)
                (update 'a2 'b2 'foo))))
     (test-equal '((a2 . b2)) (dict->alist dict))
     (test-equal value 'foo))

    ;; delete
    (let ()
     (define-values 
       (dict value)
       (dict-search! (alist->dict '((a . b) (c . d))) 'a
              (lambda args
                (error))
              (lambda (key value update delete)
                (delete 'foo))))
     (test-equal '((c . d)) (dict->alist dict))
     (test-equal value 'foo)))

  (test-group
    "dict-size"
    (test-equal 2 (dict-size  (alist->dict '((a . b) (c . d)))))
    (test-equal 0 (dict-size (alist->dict '()))))

  (test-group
    "dict-for-each"
    (define lst '())
    (dict-for-each 
           (lambda (key value)
             (set! lst (append lst (list key value))))
           (alist->dict '((a . b) (c . d))))
    (test-equal '(a b c d) lst))

  (test-group
    "dict-count"
    (define count (dict-count 
                         (lambda (key value)
                           (equal? value 'b))
                         (alist->dict '((a . b) (c . d)))))
    (test-equal count 1))

  (test-group
    "dict-any"

    (let ()
     (define value 
       (dict-any 
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '((a . b) (c . d)))))
     (test-equal value 'foo))

    (let ()
     (define value 
       (dict-any 
              (lambda (key value)
                (if (equal? 'e value) 'foo #f))
              (alist->dict '((a . b) (c . d)))))
     (test-equal value #f)))

  (test-group
    "dict-every"
    (let ()
     (define value
       (dict-every 
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '((a . b) (c . b)))))
     (test-equal value 'foo))

    (let ()
     (define value
       (dict-every 
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '())))
     (test-equal value #t))

    (let ()
     (define value
       (dict-every 
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '((a . b) (c . d)))))
     (test-equal value #f)))

  (test-group
    "dict-keys"
    (define keys
      (dict-keys (alist->dict '((a . b) (c . d)))))
    (test-equal '(a c) keys))

  (test-group
    "dict-values"
    (define vals
      (dict-values (alist->dict '((a . b) (c . d)))))
    (test-equal '(b d) vals))

  (test-group
    "dict-entries"
    (define-values
      (keys vals)
      (dict-entries (alist->dict '((a . b) (c . d)))))
    (test-equal '(a c) keys)
    (test-equal '(b d) vals))

  (test-group
    "dict-fold"
    (define value
      (dict-fold 
             (lambda (key value acc)
               (append acc (list key value)))
             '()
             (alist->dict '((a . b) (c . d)))))
    (test-equal value '(a b c d)))

  (test-group
    "dict-map->list"
    (define lst
      (dict-map->list 
             (lambda (key value)
               (string-append (symbol->string key)
                              value))
             (alist->dict '((a . "b") (c . "d")))))
    (test-equal '("ab" "cd") lst))

  (test-group
    "dict->alist"
    (define alist
      (dict->alist  (alist->dict '((a . b) (c . d)))))
    (test-equal alist '((a . b) (c . d)))))

(test-begin "Dictionaries")

(test-group 
  "alist"
  (do-test (lambda (alist) alist)))

(test-group
  "plist"
  (do-test 
    (lambda (alist) 
      (apply append 
             (map (lambda (pair) 
                    (list (car pair) (cdr pair))) 
                  alist)))))

(test-end)
