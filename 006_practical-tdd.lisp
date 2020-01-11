;;;; excercise: http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html

(defun test-+ ()
    (and
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))