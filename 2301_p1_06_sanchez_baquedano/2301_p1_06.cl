;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun scalar-product-rec (x y)
  (IF (OR (NULL x) (NULL y))
      0
      (+
        (* (CAR x) (CAR y))
        (scalar-product-rec (CDR x) (CDR y)))))

(defun squared-norm-rec (x)
  (scalar-product-rec x x))

(defun cosine-distance-rec (x y)
  (IF (OR (= 0 (squared-norm-rec x)) (= 0 (squared-norm-rec y)))
      nil
      (-
        1
        (/
          (scalar-product-rec x y)
          (sqrt
            (* (squared-norm-rec x) (squared-norm-rec y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;

(defun scalar-product-mapcar (x y)
  (apply #'+
    (mapcar #'* x y)))

(defun squared-norm-mapcar (x)
  (scalar-product-mapcar x x))

(defun cosine-distance-mapcar (x y)
  (IF (OR (= 0 (squared-norm-mapcar x)) (= 0 (squared-norm-mapcar y)))
      NIL
      (-
        1
        (/
          (scalar-product-mapcar x y)
          (sqrt
            (* (squared-norm-mapcar x) (squared-norm-mapcar y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
;;;

;; '(2 3 4) '((1 2 3) (1 2 0)) 0.3

(defun likelihood (x y)
  (- 1 (cosine-distance-mapcar x y)))

(defun less-likelihood (x y vector)
  (< (likelihood x vector) (likelihood y vector)))

(defun insert-in-descending-order (vector element lst less-function)
  (IF (NULL lst)
      (CONS element lst) ; TODO: Considerar (CONS element nil)
      (IF (funcall less-function element (CAR lst) vector)
          (CONS (CAR lst) (insert-in-descending-order vector element (CDR lst) less-function)) ; TODO: Explicar un poco
          (CONS element lst))))

(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0)) ; TODO: Considerar UNLESS para parametros de entrada
  (IF (OR (NULL lst-of-vectors) (NULL vector))
      NIL
      (IF ( > (likelihood vector (CAR lst-of-vectors)) confidence-level)
          (insert-in-descending-order vector (CAR lst-of-vectors) (order-vectors-cosine-distance vector (CDR lst-of-vectors) confidence-level) 'less-likelihood) ; TODO: Explicar
          (order-vectors-cosine-distance vector (CDR lst-of-vectors) confidence-level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
(defun get-min-category (categories text distance-measure)
  (IF (NULL categories)
      '(NIL 3)
      (let ((current-distance (funcall distance-measure (CDR text) (CDR (CAR categories))))
            (last-pair (get-min-category (CDR categories) text distance-measure)))
        (IF (< current-distance (NTH 1 last-pair))
            (CONS (CAR (CAR categories)) (LIST current-distance))
            last-pair))))

(defun get-vectors-category (categories texts distance-measure)
  (UNLESS (OR (NULL (CAR categories)) (NULL (CAR texts))) ; TODO: preguntar si es necesario considerarlo '() '()
      (IF (OR (NULL texts)) ; Condicion de recursion // TODO: Preguntar si es necesario.
          NIL
          (CONS
            (get-min-category categories (CAR texts) distance-measure)
            (get-vectors-category categories (CDR texts) distance-measure)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;

(defun h-value (current-point f df)
  (/
    (funcall f current-point)
    (funcall df current-point))))

(defun newton (f df max-iter x0 &optional (tol 0.001)) ;; TODO: Comprobar derivada nula
  (UNLESS (OR (= (funcall df x0) 0) (= max-iter 0))
       (let ((h (h-value x0 f df))) ; TODO: Preguntar si usar let
        (IF (< (ABS h) tol) ; TODO: Preguntar si esta es la tolerancia
            x0
            (newton f df (- max-iter 1) (- x0 h) tol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (UNLESS (NULL semillas)
      (let ((solution (newton f df max-iter (CAR semillas) tol)))
        (IF (NULL solution)
            (one-root-newton f df max-iter (CDR semillas) tol)
            solution))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
  (mapcar #'(lambda(seed) (newton f df max-iter seed tol)) semillas))


(defun list-not-nil-roots-newton (f df max-iter semillas &optional ( tol 0.001))
  (mapcan (lambda (x) (UNLESS (NULL x) (list x))) (all-roots-newton f df max-iter semillas tol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun to-list (x)
  (IF (listp x)
    x
    (list x)))
    
(defun combine-elt-lst (elt lst)
    (UNLESS (NULL elt)
      (IF (equal lst '(NIL))
          (list elt)
          (mapcar #'(lambda(x) (cons elt (to-list x) )  ) lst)   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas

(defun combine-lst-lst (lst1 lst2) ; TODO: Mirar algunos casos raros
    (mapcan #'(lambda(x) (combine-elt-lst x lst2)) lst1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
  (IF (NULL lstolsts)
      '(NIL)
      (combine-lst-lst (CAR lstolsts) (combine-list-of-lsts (CDR lstolsts)))))

(defun combine-list-of-lsts (lstolsts)
  (IF (NULL lstolsts)
      '(NIL)
      (mapcan #'(lambda(x) (combine-elt-lst x (combine-list-of-lsts (CDR lstolsts)) ) ) (CAR lstolsts))))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net))


(defun shortest-path-improved (end queue net))
