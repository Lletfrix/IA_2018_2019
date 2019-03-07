;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to-list
;;; Si x no es una lista, devuelve la lista que contiene a x.
;;;
;;; INPUT: x: objeto a enlistar
;;;
;;; OUTPUT: Si x no es una lista, la lista que contiene a x.
;;;         Si x es una lista, delvuelve la propia x.
(defun to-list (x)
  (IF (listp x)
    x
    (list x)))


(defun merge-two-lst (lst1 lst2)
  (IF (NULL lst1)
      lst2
      (CONS (CAR lst1) (merge-two-lst (CDR lst1) lst2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
  (IF (EQUAL lst '(NIL))
      (to-list elt)

  (mapcar
      (lambda (x)
        (if (listp elt) ;Si elt es un conjunto
            (merge-two-lst elt (list x))
          (list elt x)))
      lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas

(defun combine-lst-lst (a b)
  (mapcan
    (lambda (ai)
      (combine-elt-lst ai b))
    a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos

(defun combine-list-of-lsts (a)
  (UNLESS (NULL a)
      (reduce #'combine-lst-lst a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCIONES DE DERIVACION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun double-neg (x)
  (CAR (CDR (CAR (CDR x)))))

(defun neg-conj (x)
  (cons +or+ (combine-elt-lst +not+ (CDR (CAR (CDR x))))))

(defun neg-disj (x)
  (cons +and+ (combine-elt-lst +not+ (CDR (CAR (CDR x))))))

(defun implies (x)
  (let ((literals (CDR x) ))
  (list +or+ (list +not+ (CAR literals)) (CAR (CDR literals)))))

(defun neg-implies (x)
  (let ((literals (CDR (CAR (CDR x) ))))
  (list +and+ (CAR literals) (cons +not+ (CDR literals)))))

;(defun neg-implies (x)
  ;(neg-disj  (list +not+ (implies (CAR (CDR x) )))))

(defun bicond (x)
  (let ((literals (CDR x) ))
  (list +and+ (implies (cons +cond+ literals) ) (implies (cons +cond+ (reverse literals) ) ))))

(defun neg-bicond (x)
  (let ((literals (CDR (CAR (CDR x) ))))
  (list +or+ (neg-implies (list +not+ (cons +cond+ literals)) ) (neg-implies (list +not+ (cons +cond+ (reverse literals) ) )))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;
(defun truth-tree (fbf))

(defun expand-truth-tree-aux (fbf)
  (cond
    (OR (literal-p fbf)
        (list fbf))
    ((unary-connector-p (CAR fbf))
        (cond
          ((unary-connector-p (CAR (CAR (CDR fbf))))
              (expand-truth-tree-aux (double-neg fbf)))

          ((cond-connector-p (CAR (CAR (CDR fbf))))
              (expand-truth-tree-aux (neg-implies fbf)))

          ((bicond-connector-p (CAR (CAR (CDR fbf))))
              (expand-truth-tree-aux (neg-bicond fbf)))

          ((n-ary-connector-p (CAR (CAR (CDR fbf))))
              (if (eql (CAR (CAR (CDR fbf))) +or+)
                  (expand-truth-tree-aux (neg-disj fbf))
                  (expand-truth-tree-aux (neg-conj fbf))))))

    ((cond-connector-p (CAR fbf))
        (expand-truth-tree-aux (implies fbf)))

    ((bicond-connector-p (CAR fbf))
        (expand-truth-tree-aux (bicond fbf)))

    ((n-ary-connector-p (CAR fbf))
        (if (eql (CAR fbf) +or+)
            (UNLESS (NULL (CDR fbf))
                (nconc (expand-truth-tree-aux (CAR (CDR fbf))) (expand-truth-tree-aux (cons +or+ (CDDR fbf)))))

            (IF (NULL (CDDR fbf)) ; Si es un AND (^ A)
              (expand-truth-tree-aux (CAR (CDR fbf))) ;(A)
              (mapcan (lambda (x) (list (expand-truth-tree-aux x))) (combine-list-of-lsts  (list (list +and+) (CDR fbf)))))))))
