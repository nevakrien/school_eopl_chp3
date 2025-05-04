(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for let-lang.

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define (is-list-val x)
    (if (null? x) #t
        (and (expval? (car x))
             (expval? (cdr x)))))

   (define (to-lisp-val x)
     (cases expval x
       (num-val (n) n)
       (bool-val (b) b)
       (list-val (l)
          (if (null? l)
               '()
              (cons
               (to-lisp-val (car l))
               (to-lisp-val (cdr l))
                 )))))
    
  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (list-val
      (list is-list-val)) 
    ) 

;;; extractors:

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->list : ExpVal -> List<ExpVal>
  (define expval->list
    (lambda (v)
      (cases expval v
	(list-val (l) l)
	(else (expval-extractor-error 'list v)))))
  

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

)
