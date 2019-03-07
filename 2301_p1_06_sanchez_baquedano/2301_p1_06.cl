;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar-product-rec (x y)
;;; Calcula el producto escalar de dos vectores x e y de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar de x e y
;;;
(defun scalar-product-rec (x y)
  (IF (OR (NULL x) (NULL y))
      0
      (+
        (* (CAR x) (CAR y))
        (scalar-product-rec (CDR x) (CDR y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; squared-norm-rec (x)
;;; Calcula la norma al cuadrado de un vector x
;;; INPUT: x: vector, representado como una lista
;;; OUTPUT: norma al cuadrado de x
;;;
(defun squared-norm-rec (x)
  (scalar-product-rec x x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
  (UNLESS (OR (= 0 (squared-norm-rec x)) (= 0 (squared-norm-rec y)))
      (-
        1
        (/
          (scalar-product-rec x y)
          (sqrt
            (* (squared-norm-rec x) (squared-norm-rec y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar-product-mapcar (x y)
;;; Calcula el producto escalar de dos vectores x e y usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar de x e y
;;;
(defun scalar-product-mapcar (x y)
  (apply #'+
    (mapcar #'* x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; squared-norm-mapcar (x)
;;; Calcula la norma al cuadrado de un vector x
;;; INPUT: x: vector, representado como una lista
;;; OUTPUT: norma al cuadrado de x
;;;
(defun squared-norm-mapcar (x)
  (scalar-product-mapcar x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
  (UNLESS (OR (= 0 (squared-norm-mapcar x)) (= 0 (squared-norm-mapcar y)))
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
  (UNLESS (OR (NULL lst-of-vectors) (NULL vector))
      (IF ( > (likelihood vector (CAR lst-of-vectors)) confidence-level)
          (insert-in-descending-order vector (CAR lst-of-vectors) (order-vectors-cosine-distance vector (CDR lst-of-vectors) confidence-level) 'less-likelihood) ; TODO: Explicar
          (order-vectors-cosine-distance vector (CDR lst-of-vectors) confidence-level))))


(defun get-min-category (categories text distance-measure)
  (IF (NULL categories)
      '(NIL 3)
      (let ((current-distance (funcall distance-measure (CDR text) (CDR (CAR categories))))
            (last-pair (get-min-category (CDR categories) text distance-measure)))
        (IF (< current-distance (NTH 1 last-pair))
            (CONS (CAR (CAR categories)) (LIST current-distance))
            last-pair))))

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
(defun get-vectors-category (categories texts distance-measure)
  (UNLESS (OR (NULL (CAR categories)) (NULL (CAR texts))) ; TODO: preguntar si es necesario considerarlo '() '()
      (UNLESS (OR (NULL texts)) ; Condicion de recursion // TODO: Preguntar si es necesario
          (CONS
            (get-min-category categories (CAR texts) distance-measure)
            (get-vectors-category categories (CDR texts) distance-measure)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; h-value
;;; Calcula el incremento en el eje X para la siguiente iteración del
;;; método de Newton.
;;; INPUT:  current-point: punto actual del eje X.
;;;         f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;
;;; OUTPUT: incremento para la siguente iteración
(defun h-value (current-point f df)
  (/
    (funcall f current-point)
    (funcall df current-point))))
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
  (IF (EQUAL lst1 '(NIL))
      lst2
      (mapcan #'(lambda(x) (combine-elt-lst x lst2)) lst1)))

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
  (IF (OR (NULL lstolsts) (EQUAL '((NIL)) lstolsts))
      '(NIL) ; Elemento neutro del producto cartesiano
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCIONES DE DERIVACION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; double-neg (x)
;;; Deshace la doble negacion de una fbf
;;; INPUT: formula bien formada  x representada como lista
;;; OUTPUT: la fbf x sin la doble negacion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun double-neg (x)
  (CDR (CAR (CDR x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neg-conj (x)
;;; Aplica las formula de De Morgan a la negacion de un conjuncion
;;; INPUT: formula bien formada  x representada como lista
;;; OUTPUT: la fbf x tras aplicar De Morgan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neg-conj (x)
  (let ( (literals (CDR (CAR (CDR x)) ) ) )
    (list +or+ (list +not+ (CAR literals)) (cons +not+ (CDR literals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neg-disj (x)
;;; Aplica las formula de De Morgan a la negacion de una disjuncion
;;; INPUT: formula bien formada  x representada como lista
;;; OUTPUT: la fbf x tras aplicar De Morgan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neg-disj (x)
  (let ( (literals (CDR (CAR (CDR x)) ) ) )
  (list +and+ (list +not+ (CAR literals)) (cons +not+ (CDR literals)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implies (x)
;;; Transforma la implicacion x en un disjuncion aplicando las
;;; reglas de derivacion
;;; INPUT: formula bien formada  x representada como lista
;;; OUTPUT: la fbf x tras la regla de derivacion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun implies (x)
  (let ((literals (CDR x) ))
  (list +or+ (list +not+ (CAR literals)) (CAR (CDR literals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; neg-implies (x)
;;; Transforma la negacion de una implicacion x en un conjuncion aplicando
;;; las reglas de derivacion y De Morgan
;;; INPUT: formula bien formada  x representada como lista
;;; OUTPUT: la fbf x tras la regla de derivacion y De Morgan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neg-implies (x)
  (let ((literals (CDR (CAR (CDR x) ))))
  (list +and+ (CAR literals) (cons +not+ (CDR literals)))))

;(defun neg-implies (x)
  ;(neg-disj  (list +not+ (implies (CAR (CDR x) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bicond (x)
;;; Transforma la doble implicacion x en una conjuncion de disjunciones
;;; aplicando las reglas de derivacion
;;; INPUT: formula bien formada  x representada como lista
;;; OUTPUT: la fbf x tras la regla de derivacion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bicond (x)
  (let ((literals (CDR x) ))
  (list +and+ (implies (cons +cond+ literals) ) (implies (cons +cond+ (reverse literals) ) ))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; bicond (x)
  ;;; Transforma la negacion de una doble implicacion x en una disjuncion
  ;;; de conjunciones aplicando las reglas de derivacion y De Morgan
  ;;; INPUT: formula bien formada  x representada como lista
  ;;; OUTPUT: la fbf x tras la regla de derivacion y De Morgan
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    ((literal-p fbf)
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
            (expand-truth tree-aux (neg-conj fbf))))))

    ((cond-connector-p (CAR fbf))
     (expand-truth-tree-aux (implies fbf)))

    ((bicond-connector-p (CAR fbf))
     (expand-truth-tree-aux (bicond fbf)))

    ((n-ary-connector-p (CAR fbf))
     (if (eql (CAR (CAR (CDR fbf))) +or+)
         (nconc (expand-truth-tree-aux (CAR (CDR fbf))) (expand-truth-tree-aux (CAR (CDR (CDR fbf)))))
         (combine-lst-lst (expand-truth-tree-aux (CAR (CDR fbf) )) (expand-truth-tree-aux (CAR (CDR (CDR fbf)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.2
BFS(grafo G, nodo_inical s)
  para cada nodo u de G(
                        estado[u]=NO_VISITADO
                        distancia[u]= INFINITO
                        padre[u]=null)
  estado[s]=VISITADO
  distancia[s]=0
  crear_cola(Q);
  Encolar(Q, s)
  while(!vacia(Q)){}
    u = extraer(Q)
    para cada v de adyacencia[u]{}






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
