(define (null? lst)
  (eq? lst '()))

(define (not b)
  (cond (b #f)
        (#t #t)))

(define (filter fct lst)
  (cond ((null? lst) '())
        ((fct (car lst)) (cons (car lst) (filter fct (cdr lst))))
        (#t (filter fct (cdr lst)))))

(define (append l1 l2)
  (cond ((null? l1) l2)
        (#t (cons (car l1) (append (cdr l1) l2)))))

(define (qsort lst)
  (cond ((null? lst) '())
        (#t (let ((pivot (car lst))
                  (rest (cdr lst)))
              (let ((inf (filter (lambda (x) (< x pivot)) rest))
                    (sup (filter (lambda (x) (not (< x pivot))) rest)))
                (append (qsort inf)
                        (cons pivot (qsort sup))))))))

(qsort '(9 1 8 2 4 3 7 5 6))
