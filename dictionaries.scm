(define-library
  (dictionaries)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1))
  (export 
    
    ;; predicates
    dictionary?
    dict-empty?
    dict-contains?
    
    ;; lookup
    dict-ref
    dict-ref/default
    
    ;; mutation
    dict-set!
    dict-adjoin!
    dict-delete!
    dict-delete-all!
    dict-replace!
    dict-intern!
    dict-update!
    dict-update/default!
    dict-pop!
    dict-map!
    dict-filter!
    dict-remove!
    dict-search!
    
    ;; whole dictionary
    dict-size
    dict-for-each
    dict-count
    dict-any
    dict-every
    dict-keys
    dict-values
    dict-entries
    dict-fold
    dict-map->list
    dict->alist
    
    ;; registering dictionary types
    register-dictionary!)
  
  (include "dictionaries-impl.scm"))