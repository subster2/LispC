# Вариант № 1

;;;1)Запишите последовательности вызовов CAR и CDR, выделяющие из приведенных ниже списков символ цель. Упростите эти вызовы с помощью ;;;комбинации селекторов:
;;;• (1 2 цель 3 4)
;;;• ((1) (2 цель) (3 (4)))
;;;• ((1 (2 (3 4 цель))))

(caddr `(1 2 цель 3 4))
(cadadr `(1 (2 цель) 3 4))
(caddar(cdadar `((1 (2 (3 4 цель))))))


;;;12)Определите функцию, заменяющую в исходном списке два подряд идущиходинаковых элемента одним

(defun task (lst)
 (cond ((null (cdr lst)) lst)
       ((equal (car lst) (cadr lst)) (cons (car lst) (task (cddr lst))))
       (t (cons (car lst) (task (cdr lst))))))

(print(task '(3 3 2 2 1 1 2 )))




;;;17) Создайте предикат, порождающий всевозможные перестановки исходного множества.

(defun insert (a l r)
  (cond ((null r) (list (append l (list a))))
     (t (cons (append l (list a) r) (insert a (append l (list (car r))) (cdr r))))))
 
 
(defun rotate (lst)
  (cond ((null (cdr lst)) (list lst))
     (t (apply 'append (mapcar (lambda (x) (insert (car lst) nil x)) (rotate (cdr lst)))))))


(print(rotate '(a b c )))


;;;29)Определите функцию, вычисляющую глубину списка (самой глубокой ветви).

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



;;;35)Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно множество подмножеством другого. Определите также СОБСТВЕННОЕ ;;;ПОДМНОЖЕСТВО.

http://lisp.ru/page.php?id=23&pg=6

;;;36. Определите предикат НЕПЕРЕСЕКАЮЩИЕСЯ, проверяющий, что два множества не пересекаются, т.е. у них нет общих элементов.

(defun in-predicate (a l)

    (cond

        ((null l) nil) ; элемент не может принадлежать пустому множеству

        ((eq a (car l)) t) ; элемент принадлежит множеству, если в нем содержится

        (t (in-predicate a (cdr l))) ; продолжаем проверку

    )

)

(defun intersection1 (a b)

    (cond

        ((null a) nil)

        ((null b) nil)

        ((in-predicate (car a) b) (print `(peresecenie)) nil ) ;(cons (car a) (intersection~ (cdr a) b)

        (t (intersection1 (cdr a) b))

    )

)

(print(intersection1 '(d l e) '(b c d) ))


;;;38. Определите функцию ОБЪЕДИНЕНИЕ, формирующую объединение двух множеств


(defun in-predicate (a l)

    (cond

        ((null l) nil) ; элемент не может принадлежать пустому множеству

        ((eq a (car l)) t) ; элемент принадлежит множеству, если в нем содержится

        (t (in-predicate a (cdr l))) ; продолжаем проверку

    )

)

(defun union1 (a b)

    (cond ((null a) b)

        ((null b) a)

        ((in-predicate (car a) b) (union1 (cdr a) b) )

        (t (cons (car a) (union1 (cdr a) b)))

    )

)

 (union1 '(a b c) 'nil)


