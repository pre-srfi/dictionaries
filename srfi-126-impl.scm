(define (register-srfi-126!)
  
  (define (hashtable-ref* table key fail success)
    (define-values (value found?) (hashtable-lookup table key))
    (if found?
        (success value)
        (fail)))
  
  (define (hashtable-ref/default* table key default)
    (hashtable-ref table key default))
  
  (define (hashtable-set!* table . obj)
    (let loop ((obj obj))
     (if (null? obj)
         table
         (begin
           (hashtable-set! table (car obj) (cadr obj))
           (loop (cddr obj))))))
  
  (define (hashtable-delete-all!* table keys)
    (for-each
      (lambda (key)
        (hashtable-delete! table key))
      keys)
    table)
  
  (define (hashtable-intern!* table key default)
    (define val (hashtable-intern! table key default))
    (values table val))
  
  (define (hashtable-update/default!* table key updater default)
    (hashtable-update! table key updater default)
    table)

  (define (hashtable-pop!* table fail)
    (if (hashtable-empty? table)
        (fail)
        (call-with-values
          (lambda () (hashtable-pop! table))
          (lambda (key value) (values table key value)))))
  
  (define (hashtable-update-all!* proc table)
    (hashtable-update-all! table proc)
    table)
  
  (define (hashtable-filter!* proc table)
    (hashtable-prune! table 
                      (lambda (key value) 
                        (not (proc key value))))
    table)
  
  (define (hashtable-remove!* proc table)
    (hashtable-prune! table proc)
    table)
  
  (define (hashtable-search* table key fail success)
    (define (handle-success value)
      (define (update new-key new-value obj)
        (unless (eq? new-key key)
          (hashtable-delete! table key))
        (hashtable-set! table new-key new-value)
        (values table obj))
      (define (remove obj)
        (hashtable-delete! table key)
        (values table obj))
      (success key value update remove))
    (define (handle-fail)
      (define (ignore obj) 
        (values table obj))
      (define (insert value obj)
        (hashtable-set! table key value)
        (values table obj))
      (fail insert ignore))
    
    (define default (cons #f #f))
    (define found (hashtable-ref table key default))
    (if (eq? default found)
        (handle-fail)
        (handle-success found)))
  
  (define (hashtable-for-each* proc table)
    (hashtable-walk table proc)
    table)
  
  (define (hashtable-map->lset* proc table)
    (hashtable-map->lset table proc))
  
  (define (hashtable-keys* table)
    (vector->list (hashtable-keys table)))
  
  (define (hashtable-values* table)
    (vector->list (hashtable-values table)))
  
  (define (hashtable-entries* table)
    (call-with-values
      (lambda () (hashtable-entries table))
      (lambda (keys vals)
        (values
          (vector->list keys)
          (vector->list vals)))))
  
  (register-dictionary!
    'dictionary? hashtable?
    'dict-empty? hashtable-empty?
    'dict-contains? hashtable-contains?
    'dict-ref hashtable-ref*
    'dict-ref/default hashtable-ref/default*
    'dict-set! hashtable-set!*
    'dict-delete-all! hashtable-delete-all!*
    'dict-intern! hashtable-intern!*
    'dict-update/default! hashtable-update/default!*
    'dict-pop! hashtable-pop!*
    'dict-map! hashtable-update-all!*
    'dict-filter! hashtable-filter!*
    'dict-remove! hashtable-remove!*
    'dict-search! hashtable-search*
    'dict-size hashtable-size
    'dict-for-each hashtable-for-each*
    'dict-keys hashtable-keys*
    'dict-values hashtable-values*
    'dict-entries hashtable-entries*
    'dict-map->list hashtable-map->lset*))
