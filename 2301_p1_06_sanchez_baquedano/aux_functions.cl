(defun merge-two-lst (lst1 lst2)
  (IF (NULL lst1)
      lst2
      (CONS (CAR lst1) (merge-two-lst (CDR lst1) lst2))))


(defun combine-lst-lst (a b)
  (mapcan
    (lambda (item-from-a)
      (mapcar
        (lambda (item-from-b)
          (if (listp item-from-a)
            (nconc item-from-a (list item-from-b))
            (list item-from-a item-from-b)))
        b))
    a))


    (reduce #'cartesian-product '((a b) (1 2) (x y)))



(defun cart (a b)
  (mapcan
    (lambda (ai)
      (mapcar
          (lambda (bi)
            (list ai bi))
          b))
    a))


(defun n-ary-cart (lists)
  (mapcan

(defun nary-cart ()
(reduce #'cart '((a b) (+ -) (1 2 3)))


(defun combine-lst-lst (a b)
  (mapcan
    (lambda (ai)
      (combine-elt-lst ai b))
    a))

(defun combine-elt-lst (el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst)
  (mapcar
      (lambda (x)
        (if (listp elt) ;Si elt es un conjunto
            (merge-two-lst elt (list x))
          (list elt x)))
      lst))

(defun combine-lst-lst (a b)
  (mapcan
    (lambda (ai)
      (combine-elt-lst ai b))
    a))

(defun combine-list-of-lsts (a)
  (UNLESS (NULL a)
      (reduce #'combine-lst-lst a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge-two-lst (lst1 lst2)
  (IF (NULL lst1)
      lst2
      (CONS (CAR lst1) (merge-two-lst (CDR lst1) lst2))))
