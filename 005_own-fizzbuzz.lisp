;; function taking a list of number
(defun fizz-buzz-list (&rest numbers)
    (dolist (number numbers)
        (let ((str ""))
            (when (= (mod number 3) 0)
                (setq str (concatenate 'string str "fizz")))
            (when (= (mod number 5) 0)
                (setq str (concatenate 'string str "buzz")))
            (format t "~a~%" (if (string= str "") number str)))))

;; function performing on from-to range
(defun fizz-buzz (start end)
    (do ((number start (1+ number)))
        ((= number end))
        (let ((str ""))
            (when (= (mod number 3) 0)
                (setq str (concatenate 'string str "fizz")))
            (when (= (mod number 5) 0)
                (setq str (concatenate 'string str "buzz")))
            (format t "~a~%" (if (string= str "") number str)))))

;; function that aims at being more concise; with range
(defun fizz-buzz-concise (start end)
    (do ((number start (1+ number)))
        ((= number end))
        (format t "~a~%"
            (let ((str (concatenate 'string (if (= (mod number 3) 0) "fizz" "") (if (= (mod number 5) 0) "buzz" ""))))
                (if (string= str "") number str)))))

;; concise function with abstraction
(defun fizz-buzz-conciser (start end)
    (let* ((fn (lambda (n n-checked label) (if (= (mod n-checked n) 0) label "")))
           (check-3 (lambda (n) (funcall fn 3 n "fizz")))
           (check-5 (lambda (n) (funcall fn 5 n "buzz"))))
        (do ((number start (1+ number)))
            ((= number end))
            (format t "~a~%"
                (let ((str (concatenate 'string (funcall check-3 number) (funcall check-5 number))))
                    (if (string= str "") number str))))))

;;; with abstraction outside function
(defun mod-or-else (moddable checked label)
    (if (= (mod checked moddable) 0) label ""))

(defun check-3 (n)
    (mod-or-else 3 n "fizz"))

(defun check-5 (n)
    (mod-or-else 5 n "buzz"))

(defun fizz-buzz-abstracted (start end)
    (do ((number start (1+ number)))
        ((= number end))
        (format t "~a~%"
            (let ((str (concatenate 'string (check-3 number) (check-5 number))))
                (if (string= str "") number str)))))
