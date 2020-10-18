(define (register-srfi-69!)
  
  (define (hash-table-ref* table key fail success)
    (define default (cons #f #f))
    (define found (hash-table-ref/default table key default))
    (if (eq? found default)
        (fail)
        (success found)))
  
  (define (hash-table-set!* table . obj)
    (let loop ((obj obj))
     (if (null? obj)
         table
         (begin
           (hash-table-set! table (car obj) (cadr obj))
           (loop (cddr obj))))))
  
  (define (hash-table-update!/default* table key proc default)
    (hash-table-update!/default table key proc default)
    table)
  
  (define (hash-table-delete-all!* table keys)
    (for-each
      (lambda (key)
        (hash-table-delete! table key))
      keys)
    table)
  
  (define (hash-table-foreach* proc table)
    (hash-table-walk table proc))
  
  (define (hash-table-map* proc table)
    (hash-table-walk table (lambda (key value)
                             (hash-table-set! table key (proc key value))))
    table)
  
  (define (hash-table-filter* proc table)
    (hash-table-walk table 
                     (lambda (key value)
                       (unless (proc key value)
                         (hash-table-delete! table key))))
    table)
  
  (define (hash-table-fold* proc knil table)
    (hash-table-fold table proc knil))
  
  (define (hash-table-search* table key fail success)
    (define (handle-success value)
      (define (update new-key new-value obj)
        (unless (eq? new-key key)
          (hash-table-delete! table key))
        (hash-table-set! table new-key new-value)
        (values table obj))
      (define (remove obj)
        (hash-table-delete! table key)
        (values table obj))
      (success key value update remove))
    (define (handle-fail)
      (define (ignore obj) 
        (values table obj))
      (define (insert value obj)
        (hash-table-set! table key value)
        (values table obj))
      (fail insert ignore))
    
    (define default (cons #f #f))
    (define found (hash-table-ref/default table key default))
    (if (eq? default found)
        (handle-fail)
        (handle-success found)))
  
  (register-dictionary!
    'dictionary? hash-table?
    'dict-ref hash-table-ref*
    'dict-ref/default hash-table-ref/default
    'dict-set! hash-table-set!*
    'dict-delete-all! hash-table-delete-all!*
    'dict-contains? hash-table-exists?
    'dict-update/default! hash-table-update!/default*
    'dict-size hash-table-size
    'dict-keys hash-table-keys
    'dict-values hash-table-values
    'dict-map! hash-table-map*
    'dict-filter! hash-table-filter*
    'dict-for-each hash-table-foreach*
    'dict-fold hash-table-fold*
    'dict->alist hash-table->alist
    'dict-search! hash-table-search*))
