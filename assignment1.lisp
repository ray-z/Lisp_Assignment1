;;;-*- Language: Common Lisp -*-
#|
 | This program can be execute to do simple polynomial arithmetic.
 | Basic functions: poly+, poly-, poly*, all of them take only two inputs.
 |
 | Author: Lei Zeng - LZ474
 |
 |
 | Inputs' format:
 | ________________
 |
 |  1) Representation of terms in polynomial:
 |      1) Each term should be represented like this:
 |         (coefficient letter exponent)
 |      2) You can put as many letters and exponents as you want.
 |      Example: 
 |          X^2 --> (1 X 2) 
 |          3XY^2Z --> (3 X 1 Y 2 Z 1)
 |      Warining: 
 |          Put a simplyfied terms:
 |              XX^2 should rather be X^3
 |              X^(1+2) should rather be X^3
 |
 |  2) Format of polynomials:
 |      1) Polynomials should be a form of lists contains terms.
 |      2) '+' and '-' symbols between terms should be represent by
 |         the coefficient of the second terms.
 |         Example:
 |             X + Y --> ((1 X 1) (1 Y 1))
 |             2XY - Z^2 --> ((2 X 1 Y 1) (-1 Z 2))
 |      3) '*' and "/' symbols between terms should be simplified first.
 |         Example:
 |             XY * XYZ --> X^2Y^2Z --> (1 X 2 Y 2 Z 1)
 |             XYZ / XY --> Z --> (1 Z 1)
 |
 |  3) There is not validation of inputs, so make sure the inputs are
 |     correctly formatted.
 |
 |
 | How to test:
 | ____________  
 |
 |  1) Run clisp
 |  2) Load this file: (load "assignment1.lisp")
 |  3) Call test by passing 2 polynomials: P1 P2
 |  4) Results are showed by this order: P1 + P2
 |                                       P2 - P2
 |                                       P1 * P2
 |  5) You can also test poly+, poly-, poly* individually. 
 |# 

(defun poly+ (p1 p2) 
  "Return sum of two polynomials in a simplified form."
  (let ((p (append p1 p2))
        (result '()))
    (labels ((find-letter (l term)
                          "Return ture if letter l is in term.
                          l is letter follow by exponent.
                          term does not have coefficient."
                          (if (equal term '())
                            Nil
                            (if (equal l (list (car term) (cadr term)))
                              t
                              (find-letter l (cddr term)))))
             (check-same (term1 term2 temp)
                         "Return true if they are the same, else false.
                         term1 and term2 here are terms without coefficients."
                         (if (equal term1 '())
                           (if (= (length temp) (length term2))
                             t
                             Nil)
                           (let ((l (list (car term1) (cadr term1))))
                             (if (find-letter l term2)
                               (check-same (cddr term1) term2 (append temp l))
                               Nil))))
             (term+term (term1 term2)
                        "Return a list contains sum of 2 terms.
                        If same term: add coefficients, put new term into a new
                        list and return it;
                        else put both terms into a new list and return it."
                        (if (check-same (cdr term1) (cdr term2) '())
                          (list (cons (+ (car term1) (car term2)) (cdr term1)))
                          (list term1 term2)))
             (term+list (new-list old-list)
                               "Return a simplified list = new-list + old-list. 
                               new-list is a list of one term that you want to
                               add to old-list.
                               "
                               (if (equal old-list '())
                                 new-list
                                 (let ((head-list (term+term (car new-list) 
                                                             (car old-list))))
                                   (term+list (append head-list
                                                             (cdr new-list))
                                                     (cdr old-list)))))
             (list+list (l1 l2)
                        "Return a simplified list = l1 + l2.
                        Add each term in l1 into l2."
                        (if (equal l1 '())
                          l2
                          (list+list (cdr l1)
                                     (term+list (list (car l1)) l2))))
             (clean-list (new-list old-list)
                         "Return a list.
                         Remove terms start with 0"
                         (if (equal old-list '())
                           new-list
                           (if (= (caar old-list) 0)
                             (clean-list new-list (cdr old-list))
                             (clean-list (cons (car old-list) new-list)
                                         (cdr old-list))))))
      (clean-list '() (list+list p result)))))

(defun poly- (p1 p2)
  "Return a polynomial = p1 - p2.
  Same as poly+, multiply all coefficients of p2 by -1,
  p1-p2 = p1 + (-1)*p2"
  (labels ((negtive-term (term)
                         "Return a term = (-1) * term.
                         Multiply coefficient of term by -1"
                         (cons (* -1 (car term)) (cdr term)))
           (reverse-poly (new-list old-list)
                         "Return a list = (-1) * old-list.
                         Multiply coefficients of all terms in old-list by -1."
                         (if (equal old-list '())
                           new-list
                           (reverse-poly (cons (negtive-term (car old-list))
                                               new-list)
                                         (cdr old-list)))))
    (poly+ p1 (reverse-poly '() p2))))
                         
(defun poly* (p1 p2)
  "Return a polynomial = p1 * p1.
  'letter' is a list contains letter and exponent, eg. (X 2),
  'term' is letters append together, eg. (X 1 Y 2).
  'list' is a list of terms, eg. ((X 1 Y 2) (Z 3 X 1))).
  "
  (labels ((letter*term (letter term result)
                        "Return a new term = letter * term.
                        Multiply all letters in term by letter."
                        (if (equal term '())
                          (append letter result)
                          (if (equal (car letter) (car term))
                            (append (list (car term)
                                          (+ (cadr letter) (cadr term)))
                                    (cddr term)
                                    result)
                            (letter*term letter
                                         (cddr term)
                                         (append (list (car term) (cadr term)) 
                                                 result)))))
           (term*term (term1 term2)
                      "Return a new term = term1 * term2.
                      Multiply all letters in term2 bt term1"
                      (if (equal term1 '())
                        term2
                        (term*term (cddr term1)
                                   (letter*term (list (car term1) (cadr term1))
                                                term2
                                                '()))))
           (term*list (term l result)
                      "Return a new list = term * l.
                      Multiply all terms in l by the term.
                      "
                      (if (equal l '())
                        result
                        (let ((new-term (cons (* (car term) (caar l))
                                             (term*term (cdr term) (cdar l)))))
                          (term*list term
                                     (cdr l)
                                              (cons new-term result)))))
           (list*list (l1 l2 result)
                      "Return a new list = l1 * l2.
                      Add the results of each term in l1 multipy l2."
                      (if (equal l1 '())
                        result
                        (list*list (cdr l1)
                                   l2
                                   (poly+ result 
                                          (term*list (car l1) l2 '()))))))
    (list*list p1 p2 '())))

(defun test (p1 p2)
  "This is used for testing."
  (print (poly+ p1 p2))
  (print (poly- p1 p2))
  (print (poly* p1 p2))
  (print "Test finished."))

