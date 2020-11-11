(declare-fun |0 Fill 0| () String)
(declare-fun |0 Fill 1| () String)
(declare-fun |0 Fill 2| () String)
(declare-fun |0 Fill 5| () String)
(declare-fun |0 Fill 6| () String)
(declare-fun |0 Fill 4| () String)
(declare-fun |0 Fill 3| () String)
(declare-fun X () String)
(declare-fun |IsMatch_/(a\|b)/_0| () Bool)
(declare-fun |1 Fill 0| () String)
(declare-fun |1 Fill 1| () String)
(declare-fun |1 Fill 2| () String)
(declare-fun |1 Fill 5| () String)
(declare-fun |1 Fill 6| () String)
(declare-fun |IsMatch_/(a\|b)/_1| () Bool)
(define-funs-rec ( ( str.repeat ((x!1 String) (x!2 Int)) String)
                   ( str.whiteLeft ((x!1 String) (x!2 Int)) Int)
                   ( str.whiteRight ((x!1 String) (x!2 Int)) Int))
                 ( (ite (<= x!2 0)
                        ""
                        (str.++ x!1 ((_ str.repeat 0) x!1 (- x!2 1))))
                   (ite (= (str.at x!1 x!2) " ")
                        ((_ str.whiteLeft 0) x!1 (+ x!2 1))
                        x!2)
                   (ite (= (str.at x!1 x!2) " ")
                        ((_ str.whiteRight 0) x!1 (- x!2 1))
                        x!2)))
(assert (= |0 Fill 0| "a"))
(assert (= |0 Fill 1| "b"))
(assert (or (= |0 Fill 2| "a") (= |0 Fill 2| "b")))
(assert (str.in.re |0 Fill 2| (re.union (str.to.re "a") (str.to.re "b"))))
(assert (= |0 Fill 5| |0 Fill 2|))
(assert (= |0 Fill 6| |0 Fill 2|))
(assert (= X (str.++ |0 Fill 3| |0 Fill 2| |0 Fill 4|)))
(assert (str.in.re X
           (re.++ (re.* (re.range "\x00" "\xff"))
                  (re.union (str.to.re "a") (str.to.re "b"))
                  (re.* (re.range "\x00" "\xff")))))
(assert |IsMatch_/(a\|b)/_0|)
(assert (= |1 Fill 0| "a"))
(assert (= |1 Fill 1| "b"))
(assert (or (= |1 Fill 2| "a") (= |1 Fill 2| "b")))
(assert (str.in.re |1 Fill 2| (re.union (str.to.re "a") (str.to.re "b"))))
(assert (= |1 Fill 5| |1 Fill 2|))
(assert (= |1 Fill 6| |1 Fill 2|))
(assert (not |IsMatch_/(a\|b)/_1|))
(assert (let ((a!1 (>= (+ (str.len |0 Fill 3|) (* (- 1) (str.len X))) 0))
      (a!3 (ite (>= (+ (str.len |0 Fill 3|) (str.len X) (str.len |0 Fill 2|)) 0)
                (+ (str.len |0 Fill 3|) (str.len X) (str.len |0 Fill 2|))
                0)))
(let ((a!2 (ite (<= (str.len X) 0)
                ""
                (str.substr X 0 (ite a!1 (str.len X) (str.len |0 Fill 3|)))))
      (a!4 (ite (>= (+ (str.len |0 Fill 3|) (str.len |0 Fill 2|)) 0)
                (+ (str.len |0 Fill 3|) (str.len |0 Fill 2|))
                a!3)))
(let ((a!5 (ite (<= (+ (str.len X) (* (- 1) a!4)) 0)
                ""
                (str.substr X a!4 (+ (str.len X) (* (- 1) a!4))))))
(let ((a!6 (str.in.re (str.++ a!2 "hello" a!5)
                      (re.++ (re.* (re.range "\x00" "\xff"))
                             (re.union (str.to.re "a") (str.to.re "b"))
                             (re.* (re.range "\x00" "\xff"))))))
  (not a!6))))))
(assert (let ((a!1 (>= (+ (str.len |0 Fill 3|) (* (- 1) (str.len X))) 0))
      (a!3 (ite (>= (+ (str.len |0 Fill 3|) (str.len X) (str.len |0 Fill 2|)) 0)
                (+ (str.len |0 Fill 3|) (str.len X) (str.len |0 Fill 2|))
                0)))
(let ((a!2 (ite (<= (str.len X) 0)
                ""
                (str.substr X 0 (ite a!1 (str.len X) (str.len |0 Fill 3|)))))
      (a!4 (ite (>= (+ (str.len |0 Fill 3|) (str.len |0 Fill 2|)) 0)
                (+ (str.len |0 Fill 3|) (str.len |0 Fill 2|))
                a!3)))
(let ((a!5 (ite (<= (+ (str.len X) (* (- 1) a!4)) 0)
                ""
                (str.substr X a!4 (+ (str.len X) (* (- 1) a!4))))))
  (or (not (= "" a!2)) (not (= "" a!5)))))))

(check-sat)
(get-model)