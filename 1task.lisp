# Вариант № 1

;1)Запишите последовательности вызовов CAR и CDR, выделяющие из приведенных ниже списков символ цель. Упростите эти вызовы с помощью ;комбинации селекторов:
;• (1 2 цель 3 4)
;• ((1) (2 цель) (3 (4)))
;• ((1 (2 (3 4 цель))))

(caddr `(1 2 цель 3 4))
(cadadr `(1 (2 цель) 3 4))
(caddar(cdadar `((1 (2 (3 4 цель))))))


;12)Определите функцию, заменяющую в исходном списке два подряд идущиходинаковых элемента одним

(defun task (lst)
 (cond ((null (cdr lst)) lst)
       ((equal (car lst) (cadr lst)) (cons (car lst) (task (cddr lst))))
       (t (cons (car lst) (task (cdr lst))))))

;(print(task '(3 3 2 2 1 1 2 )))
;(3 2 1 2)

(defun task (lst)
 ((lambda(x y)(cond ((null y) lst)
       ((equal x (cadr lst)) (cons x (task (cddr lst))))
       (t (cons x (task y))))) (car lst)(cdr lst)))

;(print(task '(3 3 3  1 1 2 2 2 )))
;(3 3 1 2 2)

;16)Определите функцию, добавляющую элементы одного списка во второй список, начиная с заданной позиции.

;17) Создайте предикат, порождающий всевозможные перестановки исходного множества.

(defun insert (a l r)
  (cond ((null r) (list (append l (list a))))
     (t (cons (append l (list a) r) (insert a (append l (list (car r))) (cdr r))))))
 
 
(defun rotate (lst)
  (cond ((null (cdr lst)) (list lst))
     (t (apply 'append (mapcar (lambda (x) (insert (car lst) nil x)) (rotate (cdr lst)))))))


;(print(rotate '(a b c )))
;((A B C) (B A C) (B C A) (A C B) (C A B) (C B A)) 

;29)Определите функцию, вычисляющую глубину списка (самой глубокой ветви).

(   defun depth_list(list)
(cond
((atom list) 0)
((null list) 1)
 
(t 
  (max
       (+ 1 (depth_list(car list)))
        (depth_list(cdr list))
   )
)))

(print(depth_list '(1 
                        (2 3 
                           (3 4 
                              (2 3 
                                  (1)
                              )
                           )
                        )
                    
                    )))



;35)Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно множество подмножеством другого. Определите также СОБСТВЕННОЕ ПОДМНОЖЕСТВО.



;36. Определите предикат НЕПЕРЕСЕКАЮЩИЕСЯ, проверяющий, что два множества не пересекаются, т.е. у них нет общих элементов.

(defun intersection1 (a b)

    ((lambda(x)(cond

        ((null a) nil)

        ((null b) nil)

        ((in-predicate x b) (cons x (intersection1 (cdr a) b)) )

        (t (intersection1 (cdr a) b)))

    )(car a))
)

(defun NOintersection (a b)
	 (cond
		((null (intersection1 a b)) T)
		(t nill)
	)
)

;(print(intersection1 '(d l e) '(b c d) ))
; (D) 

;(print(intersection1 '(a l e) '(b c d) ))
; NIL 

(defun NOintersection (a b)
	 (cond
		((null (intersection1 a b)) T)
		(t nill)
	)
) 
;(print(NOintersection '(d l e) '(b c d) ))
; NILL

;(print(NOintersection '(a l e) '(b c d) ))
; T


;38. Определите функцию ОБЪЕДИНЕНИЕ, формирующую объединение двух множеств


(defun in-predicate (a l)

    (cond

        ((null l) nil) 
        ((eq a (car l)) t) 

        (t (in-predicate a (cdr l))) 

    )

)

(defun union1 (a b)

    ((lambda(x )(cond ((null a) b)

        ((null b) a)

        ((in-predicate x b)  )

        (t (cons x (union1 (cdr a) b)))

    )

    ) (car a))
)
 
;(print(union1 '(a b c) 'nil))
;(A B C)


;46)Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. Напишите функ-цию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предика (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем



