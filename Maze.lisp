(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)
(require 'lispbuilder-sdl)

(defvar posX -0.9)
(defvar posY -0.9)
(defvar auxiliar  0.0)
(defvar opcion 0)
(defvar bandera 0)
(defvar respuesta #\0)
(defvar paredX (make-hash-table))
(defvar paredY (make-hash-table))
(defvar grabado 0)
(defvar resolver)
(defvar auxresolver)

(defun chocar ()

    (cond  (
                ( and   (or (= opcion 3) (= opcion 4))
                        (= auxiliar 0.7399996)
                        (> posX (car (gethash auxiliar paredY)))
                        (< posX (car (cdr (gethash auxiliar paredY))))
                )

                (write-line "")
                (write-line "Ganaste !!!!")
                (write-line "Presione una tecla para continuar")
                (read)

                (setq grabado 1)
                (sdl:push-quit-event)
            )

            (t
                (cond   (
                            ( and   (or (= opcion 1) (= opcion 2))
                                    (not (NULL (gethash auxiliar paredX)))
                                    (> posY (car (gethash auxiliar paredX)))
                                    (< posY (car (cdr (gethash auxiliar paredX))))
                            )
                             
                            (setq bandera 0)
                        )

                        (
                            ( and   (or (= opcion 3) (= opcion 4))
                                    (not (NULL (gethash auxiliar paredY)))
                                    (> posX (car (gethash auxiliar paredY)))
                                    (< posX (car (cdr (gethash auxiliar paredY))))
                            )

                            (setq bandera 0)
                        )

                        (t (setq bandera 1))
                )
            )
    )
)

(defun decide ()

    (setq auxiliar 0.0)

    (cond   ((= opcion 1) (setq auxiliar (+ posX 0.02)) )
            ((= opcion 2) (setq auxiliar (- posX 0.02)) )
            ((= opcion 3) (setq auxiliar (+ posY 0.02)) )
            ((= opcion 4) (setq auxiliar (- posY 0.02)) )
    )

    (chocar)

    (cond   (   (and (= opcion 1) (< posX 0.91)  (= bandera 1))

                (setq posX auxiliar )
                (setq resolver (cons opcion resolver))
            )

            (   (and (= opcion 2) (> posX -0.91) (= bandera 1))
                
                (setq posX auxiliar )
                (setq resolver (cons opcion resolver))
            )

            (   (and (= opcion 3) (< posY 0.91)  (= bandera 1))
                
                (setq posY auxiliar )
                (setq resolver (cons opcion resolver))
            )

            (   (and (= opcion 4) (> posY -0.91) (= bandera 1))
                
                (setq posY auxiliar )
                (setq resolver (cons opcion resolver))
            )
    )
)

(defun coordenadas ()
    "Define las coordenadas de las paredes"

    (setf (gethash '-0.86        paredX) '( -0.93999994  0.4699998  ) )
    (setf (gethash '-0.6400002   paredX) '( -0.93999994  0.4699998  ) )
    (setf (gethash '0.45999977   paredY) '( -0.87       -0.63000024 ) )
    (setf (gethash '0.4599998    paredY) '( -0.87       -0.63000024 ) )

    (setf (gethash '0.8599995    paredX) '( -0.49000035  0.9399994  ) )
    (setf (gethash '0.6399997    paredX) '( -0.49000035  0.9399994  ) )
    (setf (gethash '-0.48000035  paredY) '(  0.6299997   0.86       ) )

    (setf (gethash '0.5399998    paredY) '( -0.93999994 -0.21000024 ) )
    (setf (gethash '0.7599996    paredY) '( -0.93999994 -0.21000024 ) )
    (setf (gethash '-0.22000024  paredX) '(  0.5299998   0.76999955 ) )

    (setf (gethash '-0.84000003  paredX) '(  0.7599996   0.8399995  ) )

    (setf (gethash '0.8399995    paredY) '( -0.93999994  0.67999965 ) )

    (setf (gethash '-0.84000003  paredY) '( -0.6400002   0.9399994  ) )

    (setf (gethash '-0.5400003   paredY) '(  0.12999969  0.9399994  ) )
    (setf (gethash '-0.7600001   paredY) '(  0.12999969  0.9399994  ) )
    (setf (gethash '0.13999972   paredX) '( -0.7700001  -0.5300003  ) )
    (setf (gethash '0.13999969   paredX) '( -0.7700001  -0.5300003  ) )

    (setf (gethash '0.8399995    paredX) '( -0.84000003 -0.7600001  ) )

    (setf (gethash '-0.16000025  paredX) '(  0.0399997   0.8399995  ) )
    (setf (gethash '0.05999973   paredX) '(  0.0399997   0.8399995  ) )
    (setf (gethash '-0.16000028  paredX) '(  0.0399997   0.8399995  ) )
    (setf (gethash '0.0599997    paredX) '(  0.0399997   0.8399995  ) )

    (setf (gethash '0.4399998    paredY) '( -0.47000034 -0.16000025 ) )
    (setf (gethash '0.43999976   paredY) '( -0.47000034 -0.16000025 ) )
    (setf (gethash '0.21999967   paredY) '( -0.47000034 -0.16000025 ) )
    (setf (gethash '0.2199997    paredY) '( -0.47000034 -0.16000025 ) )
    (setf (gethash '-0.46000034  paredX) '(  0.20999967  0.44999977 ) )

    (setf (gethash '0.0599997    paredY) '( -0.5700003   0.47       ) )
    (setf (gethash '0.05999973   paredY) '( -0.5700003   0.47       ) )
    (setf (gethash '-0.16000028  paredY) '( -0.5700003   0.47       ) )
    (setf (gethash '-0.16000025  paredY) '( -0.5700003   0.47       ) )
    (setf (gethash '0.4599998    paredX) '( -0.17000025  0.06       ) )
    (setf (gethash '0.45999977   paredX) '( -0.17000025  0.06       ) )

    (setf (gethash '-0.5600003   paredX) '( -0.7500001   0.06       ) )
    (setf (gethash '-0.34000027  paredX) '( -0.7500001   0.06       ) )
    (setf (gethash '-0.3400003   paredX) '( -0.7500001   0.06       ) )
    (setf (gethash '-0.7400001   paredY) '( -0.5700003  -0.34       ) )

    (setf (gethash '-0.24000023  paredX) '( -0.84       -0.24       ) )
    (setf (gethash '-0.020000268 paredX) '( -0.84       -0.24       ) )
    (setf (gethash '-0.24000026  paredX) '( -0.84       -0.24       ) )
    (setf (gethash '-0.020000298 paredX) '( -0.84       -0.24       ) )

    (setf (gethash '-0.24000023  paredY) '( -0.26        0.34       ) )
    (setf (gethash '-0.24000026  paredY) '( -0.26        0.34       ) )
    (setf (gethash '-0.46000034  paredY) '( -0.26        0.34       ) )
    (setf (gethash '0.3399997    paredX) '( -0.47       -0.24       ) )
    (setf (gethash '0.33999974   paredX) '( -0.47       -0.24       ) )

    (setf (gethash '0.13999969   paredY) '( 0.23         0.64       ) )
    (setf (gethash '0.13999972   paredY) '( 0.23         0.64       ) )
    (setf (gethash '0.35999975   paredY) '( 0.23         0.64       ) )
    (setf (gethash '0.35999972   paredY) '( 0.23         0.64       ) )
    (setf (gethash '0.23999967   paredX) '( 0.13         0.36       ) )
    (setf (gethash '0.2399997    paredX) '( 0.13         0.36       ) )

    (setf (gethash '0.49999982   paredY) '( 0.06         0.56       ) )
    (setf (gethash '0.4999998    paredY) '( 0.06         0.56       ) )
    (setf (gethash '0.7199996    paredY) '( 0.06         0.56       ) )
    (setf (gethash '0.55999976   paredX) '( 0.48         0.72       ) )

;;;;;;;;;;;;;;;; GANAR
    (setf (gethash '0.7399996    paredY) '( 0.85         0.94       ) )

)

(defun dibujaLaberinto ()
    "Dibuja el Laberinto y al personaje"
    (gl:matrix-mode :modelview)

;;;;;;;;;;;;;;;; FONDO
    (gl:load-identity)
    (gl:translate 0.0 0.0 0.0)

    (gl:begin :quads)
        (gl:color    1.0  1.0  1.0)
        (gl:vertex  -1.0  1.0  0.0)
        (gl:vertex   1.0  1.0  0.0)
        (gl:vertex   1.0 -1.0  0.0)
        (gl:vertex  -1.0 -1.0  0.0)
    (gl:end)

;;;;;;;;;;;;;;;; PERSONAJE
    (gl:load-identity)
    (gl:translate posX posY 0.0)

    (gl:begin :quads)
        (gl:color    1.0   0.0   0.0)
        (gl:vertex  -0.08  0.08  0.0)
        (gl:vertex   0.08  0.08  0.0)
        (gl:vertex   0.08 -0.08  0.0)
        (gl:vertex  -0.08 -0.08  0.0)
    (gl:end)

;;;;;;;;;;;;;;;; LABERINTO
    (gl:load-identity)
    (gl:translate 0.0 0.0 0.0)
    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -0.8   0.4  0.0)
        (gl:vertex  -0.7   0.4  0.0)
        (gl:vertex  -0.7  -1.0  0.0)
        (gl:vertex  -0.8  -1.0  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex   0.7   1.0  0.0)
        (gl:vertex   0.8   1.0  0.0)
        (gl:vertex   0.8  -0.42 0.0)
        (gl:vertex   0.7  -0.42 0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -1.0   1.0  0.0)
        (gl:vertex   0.7   1.0  0.0)
        (gl:vertex   0.7   0.9  0.0)
        (gl:vertex  -1.0   0.9  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -0.7  -0.9  0.0)
        (gl:vertex   1.0  -0.9  0.0)
        (gl:vertex   1.0  -1.0  0.0)
        (gl:vertex  -0.7  -1.0  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -1.0   0.9  0.0)
        (gl:vertex  -0.9   0.9  0.0)
        (gl:vertex  -0.9   0.7  0.0)
        (gl:vertex  -1.0   0.7  0.0)
    (gl:end)


    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex   0.9  -0.9  0.0)
        (gl:vertex   1.0  -0.9  0.0)
        (gl:vertex   1.0  -0.7  0.0)
        (gl:vertex   0.9  -0.7  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -1.0   0.7  0.0)
        (gl:vertex  -0.28  0.7  0.0)
        (gl:vertex  -0.28  0.6  0.0)
        (gl:vertex  -1.0   0.6  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -0.1   1.0  0.0)
        (gl:vertex   0.0   1.0  0.0)
        (gl:vertex   0.0   0.0  0.0)
        (gl:vertex  -0.1   0.0  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -0.5   0.0  0.0)
        (gl:vertex   0.4   0.0  0.0)
        (gl:vertex   0.4  -0.1  0.0)
        (gl:vertex  -0.5  -0.1  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5    0.35 0.05)
        (gl:vertex  -0.5   -0.1  0.0)
        (gl:vertex  -0.4   -0.1  0.0)
        (gl:vertex  -0.4   -0.68 0.0)
        (gl:vertex  -0.5   -0.68 0.0)
    (gl:end)    

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex   0.2  -0.6  0.0)
        (gl:vertex   1.0  -0.6  0.0)
        (gl:vertex   1.0  -0.7  0.0)
        (gl:vertex   0.2  -0.7  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35  0.05)
        (gl:vertex   0.0   0.66  0.0)
        (gl:vertex   0.5   0.66  0.0)
        (gl:vertex   0.5   0.56  0.0)
        (gl:vertex   0.0   0.56  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex   0.3   0.2  0.0)
        (gl:vertex   0.7   0.2  0.0)
        (gl:vertex   0.7   0.3  0.0)
        (gl:vertex   0.3   0.3  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35  0.05)
        (gl:vertex  -0.18 -0.4  0.0)
        (gl:vertex  -0.08 -0.4  0.0)
        (gl:vertex  -0.08 -1.0  0.0)
        (gl:vertex  -0.18 -1.0  0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35 0.05)
        (gl:vertex  -0.4   0.28 0.0)
        (gl:vertex  -0.1   0.28 0.0)
        (gl:vertex  -0.1   0.38 0.0)
        (gl:vertex  -0.4   0.38 0.0)
    (gl:end)

    (gl:begin :quads)
        (gl:color    0.5   0.35  0.05)
        (gl:vertex  -0.18 -0.3  0.0)
        (gl:vertex   0.28 -0.3  0.0)
        (gl:vertex   0.28 -0.4  0.0)
        (gl:vertex  -0.18 -0.4  0.0)
    (gl:end)

    (gl:flush)

)


(defun ventana()
    "Inicializa los eventos y la ventana"
    (sdl:with-init ()
        (sdl:window 600 600
            :title-caption "Proyecto Lisp"
            :opengl t
            :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                 (:sdl-gl-depth-size 16))
        )
        (setf (sdl:frame-rate) 60)

        (gl:viewport 0 0 600 600)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
     
        (gl:clear-color 0 0 0 0)
        (gl:shade-model :flat)
        (gl:clear :color-buffer :depth-buffer)
        (sdl:enable-key-repeat 100 30)
        (dibujaLaberinto)
        (sdl:update-display)
        (sdl:with-events ()
            (:quit-event () t)
            (:video-expose-event () (sdl:update-display))
            (:key-down-event (:key key)
                (case key
                    (:sdl-key-escape (sdl:push-quit-event))
                    (:sdl-key-right (setq opcion 1) )
                    (:sdl-key-left  (setq opcion 2) )
                    (:sdl-key-up    (setq opcion 3) )
                    (:sdl-key-down  (setq opcion 4) )
                )

                (decide)
                (dibujaLaberinto)
                (sdl:update-display)
            )
        )
    )
)

(defun retrasar (x)
    (sleep 0.05)
    (setq opcion x)
    (decide)
    (dibujaLaberinto)
    (sdl:update-display)
)


(defun ventanaSinEventos()
    "Inicializa los eventos y la ventana"
    (sdl:with-init ()
        (sdl:window 600 600
            :title-caption "Proyecto Lisp"
            :opengl t
            :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                 (:sdl-gl-depth-size 16))
        )
        (setf (sdl:frame-rate) 60)

        (gl:viewport 0 0 600 600)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
     
        (gl:clear-color 0 0 0 0)
        (gl:shade-model :flat)
        (gl:clear :color-buffer :depth-buffer)
        (dibujaLaberinto)
        (sdl:update-display)
        (sdl:with-events ()
            (:quit-event () t)
            (:idle ()         
                (setq auxresolver (reverse resolver))

                (mapcar #' retrasar auxresolver)
            )
        )     
    )
)

(defun evaluar()
    (cond   (
                (= grabado 0)
                (write-line "")
                (write-line "No se ha guardado una partida")
            )

            ( t
                (ventanaSinEventos)  
            )
    )
)

(defun menu ()
    "Despliega el menu"
    (clear-input)
    (format t "~%~% Seleccione su opcion~%")
    (format t "  1) Grabar Juego~%  2) Recordar partida~%  Otro caracter) Salir~%")
    (setq respuesta (read-char))

    (setq posX -0.9)
    (setq posY -0.9)

    (cond   ((char-equal respuesta #\1) (setq resolver nil) (setq grabado 0) (ventana))
            ((char-equal respuesta #\2) (evaluar))
            (t (exit))
    )

    (if (or (char-equal respuesta #\1) (char-equal respuesta #\2))
        (menu)
    )
)

(coordenadas)
(menu)