(declare-fun |0 Fill 0| () String)
(declare-fun |0 Fill 3| () String)
(declare-fun |0 Fill 4| () String)
(declare-fun |0 Fill 2| () String)
(declare-fun |0 Fill 1| () String)
(declare-fun X () String)
(declare-fun |IsMatch_/(a)/_0| () Bool)
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
(assert (= |0 Fill 3| "a"))
(assert (= |0 Fill 4| "a"))
(assert (let ((a!1 (str.in.re X
                      (re.++ (re.* (re.range "\x00" "\xff"))
                             (str.to.re "a")
                             (re.* (re.range "\x00" "\xff"))))))
  (or (not a!1) (= X (str.++ |0 Fill 1| "a" |0 Fill 2|)))))
(assert (let ((a!1 (str.in.re X
                      (re.++ (re.* (re.range "\x00" "\xff"))
                             (str.to.re "a")
                             (re.* (re.range "\x00" "\xff"))))))
  (= a!1 |IsMatch_/(a)/_0|)))
(assert |IsMatch_/(a)/_0|)

(check-sat)
