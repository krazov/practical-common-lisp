;;;; excercise: http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html

;;; iteration 1
(defun test-+-iteration-1 ()
    (and
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))

;;; iteration 2
(defun test-+-iteration-2 ()
    (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
    (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;;; iteration 3
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
    (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
    (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
    (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
