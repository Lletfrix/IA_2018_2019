(defun merge-two-lst (lst1 lst2)
  (IF (NULL lst1)
      lst2
      (CONS (CAR lst1) (merge-two-lst (CDR lst1) lst2))))
