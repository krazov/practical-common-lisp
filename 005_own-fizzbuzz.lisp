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
