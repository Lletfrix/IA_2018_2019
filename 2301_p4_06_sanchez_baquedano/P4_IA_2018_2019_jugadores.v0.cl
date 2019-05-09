(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
      (let ((puntuacion-actual 0)
	    (puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-actual columna fila))
		     (der (contar-derecha tablero ficha-actual columna fila))
		     (izq (contar-izquierda tablero ficha-actual columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
		      (+ puntuacion-actual
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000)))))
	      (let* ((altura (altura-columna tablero columna))
		     (fila (1- altura))
		     (abajo (contar-abajo tablero ficha-oponente columna fila))
		     (der (contar-derecha tablero ficha-oponente columna fila))
		     (izq (contar-izquierda tablero ficha-oponente columna fila))
		     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
		     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
		     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
		     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
		(setf puntuacion-oponente
		      (+ puntuacion-oponente
			 (cond ((= abajo 0) 0)
			       ((= abajo 1) 10)
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))

;;; -------------------------------------------------------------------
;;;  Funcion de jugador aleatorio mejorado
;;; -------------------------------------------------------------------
(defun f-aleatorio-mejor (estado)
  (let* ((tablero (estado-tablero estado)) (ficha (estado-turno estado)))
      (if (juego-terminado-p estado)
            (cond ((equal (ganador estado) ficha) +val-max+)
                  ((tablas-p estado) (floor +val-min+ 2))
                  (t +val-min+))
        (random 100))))

;;; -------------------------------------------------------------------
;;;  Funciones auxiliares de heurística base
;;; -------------------------------------------------------------------

;; Funcion de evaluación de una línea

(defun comprueba (ficha1 ficha2)
  (if (or (not ficha1) (not ficha2))
      t
    (= ficha1 ficha2)))

(defun eval-line (tablero ficha columna fila longitud ficha-valida f-coordenadas)
  (let ((ficha-actual (obtener-ficha tablero columna fila)))
  (cond ((< longitud 1)
      (cond ((not ficha-actual)
          (values 0 t))
        ((comprueba ficha-actual ficha-valida)
            (if (= ficha-actual ficha)
                (values 1 t)
              (values -1 t)))
        (t
            (values 0 nil))))

    ((not ficha-actual)
      (multiple-value-bind (ncol nfil) (funcall f-coordenadas columna fila)
        (eval-line tablero ficha ncol nfil (1- longitud) ficha-valida f-coordenadas)))

    ((comprueba ficha-actual ficha-valida)
      (multiple-value-bind (ncol nfil) (funcall f-coordenadas columna fila)
        (multiple-value-bind (res valid) (eval-line tablero ficha ncol nfil (1- longitud) ficha-actual f-coordenadas)
          (if valid
              (if (= ficha-actual ficha)
                  (values (1+ res) t)
                (values (- res 1) t))
            (values 0 nil)))))
    (t
        (values 0 nil)))))

;; Funciones de dirección

(defun arriba (x y) (values x (1+ y)))
(defun derecha (x y) (values (1+ x) y))
(defun arriba-derecha (x y) (values (1+ x) (1+ y)))
(defun arriba-izquierda (x y) (values (1- x) (1+ y)))
(defun abajo-izquierda (x y) (values (1- x) (1- y)))
(defun abajo-derecha (x y) (values (1+ x) (1- y)))

;; Funciones de evaluación con dirección fija

(defun eval-horizontal (tablero ficha columna fila)
  (eval-line tablero ficha columna fila 3 nil #'derecha))

(defun eval-vertical (tablero ficha columna fila)
  (eval-line tablero ficha columna fila 3 nil #'arriba))

(defun eval-diag-dcha (tablero ficha columna fila)
  (eval-line tablero ficha columna fila 3 nil #'arriba-derecha))

(defun eval-diag-izda (tablero ficha columna fila)
  (eval-line tablero ficha columna fila 3 nil #'arriba-izquierda))

;; Funciones para hallar los índices de arrays a evaluar según dirección

(defun range (min max)
   (loop for n from min below max
      collect n))

(defun cartesian-product (l1 l2)
  (loop
    for x in l1
    nconc (loop for y in l2
                collect (list x y))))

(defun indices-horizontales (ancho alto)
  (cartesian-product (range 0 (- ancho 3)) (range 0 alto)))
(defun indices-verticales (ancho alto)
  (cartesian-product (range 0 ancho) (range 0 (- alto 3))))
(defun indices-diag-dcha (ancho alto)
  (cartesian-product (range 0 (- ancho 3)) (range 0 (- alto 3))))
(defun indices-diag-izda (ancho alto)
  (cartesian-product (range 3 ancho) (range 0 (- alto 3))))

;;; -------------------------------------------------------------------------------
;;; Funcion de evaluación de *jugador-base*
;;; -------------------------------------------------------------------------------
(defun f-eval-base (estado)
  (let* ((tablero (estado-tablero estado)) (ancho (tablero-ancho tablero)) (alto (tablero-alto tablero)) (ficha (estado-turno estado)))

        (+
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-horizontal tablero ficha (car idx) (cadr idx))) (indices-horizontales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-vertical tablero ficha (car idx) (cadr idx))) (indices-verticales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-dcha tablero ficha (car idx) (cadr idx))) (indices-diag-dcha ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-izda tablero ficha (car idx) (cadr idx))) (indices-diag-izda ancho alto))))))

;;; -------------------------------------------------------------------------------
;;; Funcion de evaluación de *jugador-mejorado*
;;; -------------------------------------------------------------------------------
(defun f-eval-mejorado (estado)
  (let* ((tablero (estado-tablero estado)) (ancho (tablero-ancho tablero)) (alto (tablero-alto tablero)) (ficha (estado-turno estado)))
      (if (juego-terminado-p estado)
            (cond ((equal (ganador estado) ficha) +val-max+)
                  ((tablas-p estado) (floor +val-min+ 2))
                  (t +val-min+))
        (+
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-horizontal tablero ficha (car idx) (cadr idx))) (indices-horizontales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-vertical tablero ficha (car idx) (cadr idx))) (indices-verticales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-dcha tablero ficha (car idx) (cadr idx))) (indices-diag-dcha ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-izda tablero ficha (car idx) (cadr idx))) (indices-diag-izda ancho alto)))))))

;; -------------------------------------------------------------------------------
;; Funcion de evaluación de *jugador-final*
;; -------------------------------------------------------------------------------
(defun f-eval-final (estado)
  (let* ((tablero (estado-tablero estado)) (ancho (tablero-ancho tablero)) (alto (tablero-alto tablero)) (ficha (estado-turno estado)) (columna-central (floor ancho 2)))
      (if (juego-terminado-p estado)
            (cond ((equal (ganador estado) ficha) +val-max+)
                  ((tablas-p estado) (floor +val-min+ 2))
                  (t +val-min+))
        (+
          (if (equal (obtener-ficha tablero columna-central (1- alto)) ficha) -10 0)
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-horizontal tablero ficha (car idx) (cadr idx))) (indices-horizontales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-vertical tablero ficha (car idx) (cadr idx))) (indices-verticales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-dcha tablero ficha (car idx) (cadr idx))) (indices-diag-dcha ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-izda tablero ficha (car idx) (cadr idx))) (indices-diag-izda ancho alto)))))))

;; -------------------------------------------------------------------------------
;; Heurísticas fallidas
;; -------------------------------------------------------------------------------

;; -------------------------------------------------------------------------------
;; Restringido
;; -------------------------------------------------------------------------------
(defun indices-horizontales-v1 (tablero ancho alto)
 (horizontal-menos-no-posibles tablero ancho alto (1- ancho)
   (cartesian-product (range 0 (- ancho 3)) (range 0 alto))))

(defun horizontal-menos-no-posibles (tablero ancho alto ncol posibles)
 (if (eql ncol -1)
     posibles
   (set-difference (horizontal-menos-no-posibles tablero ancho alto (1- ncol) posibles)
     (cartesian-product (range (max (- ncol 3) 0) (1+ (min ncol (- ancho 4))))
                        (range (max 1 (1+ (altura-columna tablero ncol))) alto)) :test 'equal)))
;Diagonales
(defun coord-dir (x y direccion len)
  (unless (eql len 0)
    (multiple-value-bind (nx ny) (funcall direccion x y)
      (cons (list x y) (coord-dir nx ny direccion (1- len))))))

(defun indices-diagonales (tablero ancho alto direccion posibles)
  (set-difference posibles
      (diagonales-no-posibles tablero ancho alto (1- ancho) direccion) :test 'equal))

(defun diagonales-no-posibles (tablero ancho alto ncol direccion)
  (if (eql ncol -1)
      nil
    (union (diagonales-no-posibles tablero ancho alto (1- ncol) direccion)
           (mapcan (lambda (nfil) (coord-dir ncol nfil direccion 4))
              (range (1+ (altura-columna tablero ncol)) alto)))))

(defun indices-diag-dcha-v1 (tablero ancho alto)
  (indices-diagonales tablero ancho alto 'abajo-izquierda
      (cartesian-product (range 0 (- ancho 3)) (range 0 (- alto 3)))))

(defun indices-diag-izda-v1 (tablero ancho alto)
  (indices-diagonales tablero ancho alto 'abajo-derecha
      (cartesian-product (range 3 ancho) (range 0 (- alto 3)))))

;; -------------------------------------------------------------------------------
;; Funcion de evaluación de *jugador-restringido*
;; -------------------------------------------------------------------------------
(defun f-eval-restringido (estado)
  (let* ((tablero (estado-tablero estado)) (ancho (tablero-ancho tablero)) (alto (tablero-alto tablero)) (ficha (estado-turno estado)))
        (if (juego-terminado-p estado)
              (if (equal (ganador estado) ficha)
                  +val-max+
                +val-min+)
          (+
            (reduce #'+
                  (mapcar #'(lambda(idx) (eval-horizontal tablero ficha (car idx) (cadr idx))) (indices-horizontales-v1 tablero ancho alto)))
            (reduce #'+
                  (mapcar #'(lambda(idx) (eval-vertical tablero ficha (car idx) (cadr idx))) (indices-verticales ancho alto)))
            (reduce #'+
                  (mapcar #'(lambda(idx) (eval-diag-dcha tablero ficha (car idx) (cadr idx))) (indices-diag-dcha-v1 tablero ancho alto)))
            (reduce #'+
                  (mapcar #'(lambda(idx) (eval-diag-izda tablero ficha (car idx) (cadr idx))) (indices-diag-izda-v1 tablero ancho alto)))))))

;; -------------------------------------------------------------------------------
;; Ponderaciones
;; -------------------------------------------------------------------------------
(defun eval-horizontal-pond (tablero ficha columna fila ones twos threes)
  (let ((score (eval-line tablero ficha columna fila 3 nil #'derecha)))
    (cond ((or (>= score 3) (<= score -3))
        (* score threes))
      ((or (eql score 2) (eql score -2))
        (* score twos))
      ((or (eql score 1) (eql score -1))
        (* score ones))
      (t 0))))


(defun eval-vertical-pond (tablero ficha columna fila ones twos threes)
  (let ((score (eval-line tablero ficha columna fila 3 nil #'arriba)))
    (cond ((or (>= score 3) (<= score -3))
        (* score threes))
      ((or (eql score 2) (eql score -2))
        (* score twos))
      ((or (eql score 1) (eql score -1))
        (* score ones))
      (t 0))))


(defun eval-diag-dcha-pond (tablero ficha columna fila ones twos threes)
  (let ((score (eval-line tablero ficha columna fila 3 nil #'arriba-derecha)))
    (cond ((or (>= score 3) (<= score -3))
        (* score threes))
      ((or (eql score 2) (eql score -2))
        (* score twos))
      ((or (eql score 1) (eql score -1))
        (* score ones))
      (t 0))))

(defun eval-diag-izda-pond (tablero ficha columna fila ones twos threes)
  (let ((score (eval-line tablero ficha columna fila 3 nil #'arriba-izquierda)))
    (cond ((or (>= score 3) (<= score -3))
        (* score threes))
      ((or (eql score 2) (eql score -2))
        (* score twos))
      ((or (eql score 1) (eql score -1))
        (* score ones))
      (t 0))))

;; -------------------------------------------------------------------------------
;; Funcion de evaluación de *jugador-mejorado-ponderado*
;; -------------------------------------------------------------------------------
(defun f-eval-mejorado-ponderado (estado)
  (let* ((tablero (estado-tablero estado)) (ancho (tablero-ancho tablero)) (alto (tablero-alto tablero)) (ficha (estado-turno estado)) (columna-central (floor ancho 2)))
      (if (juego-terminado-p estado)
            (cond ((equal (ganador estado) ficha) +val-max+)
                  ((tablas-p estado) (floor +val-min+ 2))
                  (t +val-min+))
        (+
          (* 10 (contar-arriba tablero ficha columna-central 0))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-horizontal-pond tablero ficha (car idx) (cadr idx) 1 10 20)) (indices-horizontales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-vertical-pond tablero ficha (car idx) (cadr idx) 1 10 20)) (indices-verticales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-dcha-pond tablero ficha (car idx) (cadr idx) 1 10 20)) (indices-diag-dcha ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-izda-pond tablero ficha (car idx) (cadr idx) 1 10 20)) (indices-diag-izda ancho alto)))))))

;;--------------------------------------------------------------------------------
;; Funcion de evaluación de *jugador-restringido-ponderado*
;;--------------------------------------------------------------------------------
(defun f-eval-restringido-ponderado (estado)
  (let* ((tablero (estado-tablero estado)) (ancho (tablero-ancho tablero)) (alto (tablero-alto tablero)) (ficha (estado-turno estado)))
      (if (juego-terminado-p estado)
            (cond ((equal (ganador estado) ficha) +val-max+)
                  ((tablas-p estado) (floor +val-min+ 2))
                  (t +val-min+))
        (+
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-horizontal-pond tablero ficha (car idx) (cadr idx) 1 3 5)) (indices-horizontales-v1 tablero ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-vertical-pond tablero ficha (car idx) (cadr idx) 1 3 5)) (indices-verticales ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-dcha-pond tablero ficha (car idx) (cadr idx) 1 3 5)) (indices-diag-dcha-v1 tablero ancho alto)))
          (reduce #'+
                (mapcar #'(lambda(idx) (eval-diag-izda-pond tablero ficha (car idx) (cadr idx) 1 3 5)) (indices-diag-izda-v1 tablero ancho alto)))))))

;; -------------------------------------------------------------------------------
;; Jugadores
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
				       :f-eval  #'f-no-eval))

(defvar *jugador-aleatorio-mejor* (make-jugador :nombre 'Jugador-aleatorio-mejor
               :f-jugador #'f-jugador-negamax
               :f-eval #'f-aleatorio-mejor))

(defvar *jugador-base* (make-jugador :nombre 'Jugador-Base
               :f-jugador #'f-jugador-negamax
               :f-eval #'f-eval-base))

(defvar *jugador-mejorado* (make-jugador :nombre 'Jugador-Mejorado
              :f-jugador #'f-jugador-negamax
              :f-eval #'f-eval-mejorado))

(defvar *jugador-mejorado-ponderado* (make-jugador :nombre 'Jugador-Mejorado-Ponderado
              :f-jugador #'f-jugador-negamax
              :f-eval #'f-eval-mejorado-ponderado))

(defvar *jugador-restringido* (make-jugador :nombre 'Jugador-Restringido
              :f-jugador #'f-jugador-negamax
              :f-eval #'f-eval-restringido))

(defvar *jugador-restringido-ponderado* (make-jugador :nombre 'Jugador-Restringido-Ponderado
              :f-jugador #'f-jugador-negamax
              :f-eval #'f-eval-restringido-ponderado))
;; -------------------------------------------------------------------------------
;; Parámetros :
;; -------------------------------------------------------------------------------

(setf *verbose* nil)
