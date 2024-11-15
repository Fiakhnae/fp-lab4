<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гармаш Дмитро Олегович КВ-13</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3  з такими змінами: 
* використати функції вищого порядку для роботи з послідовностями (де це доречно);
* додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: ```key``` та ```test``` , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому ```key``` має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.

## Варіант першої частини 3
Алгоритм сортування обміном №3 (із запам'ятовуванням місця останньої перестановки) за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
(defun bubble-step (lst limit last-swap &key (key #'identity) (test #'<))
  (if (or (null (cdr lst)) (= limit 0))
      (values lst last-swap)
      (let* ((a (car lst))
             (b (cadr lst))
             (key-a (funcall key a))
             (key-b (funcall key b)))
        (if (funcall test key-b key-a)
            (let* ((swapped (cons b (cons a (cddr lst)))))
              (multiple-value-bind (sorted-lst new-last-swap)
                  (bubble-step (cdr swapped) (1- limit) t :key key :test test)
                (values (cons (car swapped) sorted-lst)
                        (or new-last-swap (1- limit)))))
            (multiple-value-bind (sorted-lst new-last-swap)
                (bubble-step (cdr lst) (1- limit) last-swap :key key :test test)
              (values (cons a sorted-lst)
                      (or new-last-swap last-swap)))))))

(defun recursive-bubble-sort (lst &key (key #'identity) (test #'<))
  (labels ((recursive-sort (lst limit)
             (multiple-value-bind (new-lst last-swap)
                 (bubble-step lst limit nil :key key :test test)
               (if last-swap
                   (recursive-sort new-lst (1- limit))
                   new-lst))))
    (recursive-sort lst (length lst))))
```
### Тестові набори та утиліти першої частини
```lisp
(defun run-recursive-sort-test (input expected-result test-description &key (key #'identity) (test #'<))
  (let ((result (recursive-bubble-sort input :key key :test test)))
    (if (equal result expected-result)
        (format t "~A: success.~%" test-description)
        (format t "~A: failed! ~%Expected: ~A~%Got: ~A~%" test-description expected-result result))))

(defun test-sorting-recursive ()
  (format t "Testing recursive-bubble-sort...~%")
  (run-recursive-sort-test '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9) "1")
  (run-recursive-sort-test '(1 2 3 4 5) '(1 2 3 4 5) "2")
  (run-recursive-sort-test '(5 4 3 2 1) '(1 2 3 4 5) "3")
  (run-recursive-sort-test '() '() "4")
  (run-recursive-sort-test '(1) '(1) "5")
  (run-recursive-sort-test '(1 2 2 1) '(1 1 2 2) "6")
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9)
                           "7"
                           :key #'abs)
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9)
                           "8"
                           :key #'abs :test #'<)
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(1 -1 -2 3 3 -4 -5 -5 -5 6 9)
                           "9"
                           :key #'abs :test #'<=)
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(9 6 -5 -5 -5 -4 3 3 -2 -1 1)
                           "10"
                           :key #'abs :test #'>)
  (run-recursive-sort-test '(3 2 5 4 1) '(1 2 3 4 5) "11" :test #'<=)
  (run-recursive-sort-test '(3 2 5 4 1) '(5 4 3 2 1) "12" :test #'>)
  (run-recursive-sort-test '((2 . 3) (1 . 2) (4 . 5) (3 . 1))
                           '((1 . 2) (2 . 3) (3 . 1) (4 . 5))
                           "13"
                           :key #'car)
  (run-recursive-sort-test '((2 . 3) (1 . 2) (4 . 5) (3 . 1))
                           '((3 . 1) (1 . 2) (2 . 3) (4 . 5))
                           "14"
                           :key #'cdr))

```
### Тестування першої частини
```lisp
Testing recursive-bubble-sort...
1: success.
2: success.
3: success.
4: success.
5: success.
6: success.
7: success.
8: success.
9: success.
10: success.
11: success.
12: success.
13: success.
14: success.
```
## Варіант другої частини 3
Написати функцію add-next-fn , яка має один ключовий параметр — функцію
transform . add-next-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен
елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення
поточного елемента, а в комірці CDR знаходиться значення наступного елемента списку.
Якщо функція transform передана, тоді значення поточного і наступного елементів, що
потраплять у результат, мають бути змінені згідно transform . transform має
виконатись мінімальну кількість разів.

```lisp
CL-USER> (mapcar (add-next-fn) '(1 2 3))
((1 . 2) (2 . 3) (3 . NIL))
CL-USER> (mapcar (add-next-fn :transform #'1+) '(1 2 3))
((2 . 3) (3 . 4) (4 . NIL))
```
## Лістинг реалізації другої частини завдання
```lisp
(defun add-next-fn (&key (transform 'identity))
  (let ((prev-element nil))
  (lambda (current)
    (if prev-element  
         (progn (rplacd prev-element (funcall transform current))
                (setf prev-element (cons (cdr prev-element) nil)))
         (setf prev-element (cons (funcall transform current) nil)))
      )))
```
### Тестові набори та утиліти другої частини
```lisp
(defun run-next-fn-test (input transform expected-result test-description)
  (let ((result (if(not(null transform))(mapcar (add-next-fn :transform transform) input)(mapcar (add-next-fn) input))))
    (if (equal result expected-result)
        (format t "~A: success.~%" test-description)
        (format t "~A: failed! ~%Expected: ~A~%Got: ~A~%" test-description expected-result result))))

(defun add-next-fn-test ()
  (format t "Testing add-next-fn...~%")
  (run-next-fn-test '(1 2 3) nil '((1 . 2) (2 . 3) (3 . NIL)) "1 ")
  (run-next-fn-test '(1 2 3) #'1+ '((2 . 3) (3 . 4) (4 . NIL)) "2 ")
  (run-next-fn-test '() nil '() "3 ")
  (run-next-fn-test '(1) nil '((1 . NIL)) "4 ")
  (run-next-fn-test '(1) #'1+ '((2 . NIL)) "5 ")
  (format t "Test completed~%"))
```
### Тестування другої частини
```lisp
Testing add-next-fn...
1 : success.
2 : success.
3 : success.
4 : success.
5 : success.
Test completed
```
