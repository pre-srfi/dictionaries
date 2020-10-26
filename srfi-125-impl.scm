(define (register-srfi-125!)
  
  (define (hash-table-set!* table . obj)
    (apply hash-table-set! (cons table obj))
    table)
  
  (define (hash-table-update!* table key updater fail success)
    (hash-table-update! table key updater fail success)
    table)
  
  (define (hash-table-update!/default* table key proc default)
    (hash-table-update!/default table key proc default)
    table)
  
  (define (hash-table-intern!* table key failure)
    (define val (hash-table-intern! table key failure))
    (values table val))
  
  (define (hash-table-pop!* table fail)
    (if (hash-table-empty? table)
        (fail)
        (call-with-values
          (lambda () (hash-table-pop! table))
          (lambda (key value) (values table key value)))))
  
  (define (hash-table-delete-all!* table keys)
    (for-each
      (lambda (key)
        (hash-table-delete! table key))
      keys)
    table)
  
  (define (hash-table-map* proc table)
    (hash-table-map! proc table)
    table)
  
  (define (hash-table-filter* proc table)
    (hash-table-prune! 
      (lambda (key value)
        (not (proc key value)))
      table)
    table)
  
  (define (hash-table-remove!* proc table)
    (hash-table-prune! proc table)
    table)
  
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
    (hash-table-ref table key handle-fail handle-success))
  
  (register-dictionary!
    'dictionary? hash-table?
    'dict-empty? hash-table-empty?
    'dict-contains? hash-table-contains?
    'dict-ref hash-table-ref*
    'dict-ref/default hash-table-ref/default
    'dict-set! hash-table-set!*
    'dict-delete-all! hash-table-delete-all!*
    'dict-intern! hash-table-intern!*
    'dict-update! hash-table-update*
    'dict-update/default! hash-table-update!/default*
    'dict-pop! hash-table-pop!*
    'dict-map! hash-table-map*
    'dict-filter! hash-table-filter*
    'dict-remove! hash-table-remove!*
    'dict-search! hash-table-search* 
    'dict-size hash-table-size
    'dict-for-each hash-table-foreach*
    'dict-keys hash-table-keys
    'dict-values hash-table-values
    'dict-entries hash-table-entries
    'dict-fold hash-table-fold
    'dict-map->list hash-table-map->list
    'dict->alist hash-table->alist))
