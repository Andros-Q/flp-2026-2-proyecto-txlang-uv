#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

#|
  Pruebas de los valores expresados de TxLang-UV: enteros, flotantes,
  cadenas, caracteres, booleanos, unit, referencias y procedimientos.
  Cubre los casos limite de las primitivas sobre cada clase de valor.

  Se ejecuta con:  racket pruebas-valores.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define prueba
  (lambda (etiqueta texto esperado)
    (let ((obtenido (intentar texto)))
      (if (string=? obtenido esperado)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a~%" etiqueta))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba ~s y llego ~s~%"
                              etiqueta esperado obtenido))))))

;; El programa debe rechazarse con un error que contenga el fragmento dado.
(define prueba-error
  (lambda (etiqueta texto fragmento)
    (let ((r (intentar texto)))
      (if (and (empieza-con-error? r) (contiene? r fragmento))
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a~%" etiqueta))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un error con ~s y llego ~s~%"
                              etiqueta fragmento r))))))

(define contiene?
  (lambda (texto fragmento)
    (let ((n (string-length texto)) (m (string-length fragmento)))
      (letrec ((buscar (lambda (i)
                         (cond ((> (+ i m) n) #f)
                               ((string=? (substring texto i (+ i m)) fragmento) #t)
                               (else (buscar (+ i 1)))))))
        (buscar 0)))))

(eopl:printf "~%== Valores expresados ==~%")

;; Enteros y flotantes se distinguen
(prueba "division entera trunca" "/(7, 2)" "3")
(prueba "division entera con negativos" "/(-7, 2)" "-3")
(prueba "division de flotantes no trunca" "/(7.0, 2.0)" "3.5")
(prueba "division de flotante entre entero" "/(7.5, 2)" "3.75")
(prueba "residuo de enteros" "%(7, 3)" "1")
(prueba-error "residuo de flotantes se rechaza" "%(7.5, 2)" "solo acepta enteros")
(prueba-error "division por cero entera" "/(1, 0)" "Division por cero")
(prueba-error "division por cero flotante" "/(1.5, 0.0)" "Division por cero")
(prueba "producto de flotantes" "*(1.5, 2.0)" "3.0")

;; Operandos que no son numeros: error del interprete, no de Racket
(prueba-error "suma de cadena y entero" "+(\"a\", 1)" "necesita dos numeros")
(prueba-error "resta de booleano" "-(true, 1)" "necesita dos numeros")
(prueba-error "comparar cadenas con <" "<(\"a\", \"b\")" "necesita dos numeros")
(prueba-error "multiplicar unit" "*(unit, 2)" "necesita dos numeros")

;; Cadenas y caracteres
(prueba "cadena vacia" "length(\"\")" "0")
(prueba "concat con cadena vacia" "concat(\"\", \"abc\")" "\"abc\"")
(prueba "cadena con // adentro no es comentario" "\"a // b\"" "\"a // b\"")
(prueba "caracter espacio" "' '" "' '")
(prueba-error "concat de cadena y caracter" "concat(\"a\", 'b')" "concat necesita dos cadenas")

;; Booleanos y unit
(prueba "igualdad de booleanos" "==(true, true)" "true")
(prueba "igualdad de entero y flotante es falsa" "==(1, 1.0)" "false")
(prueba "unit es un valor" "unit" "unit")
(prueba "set devuelve unit" "let c = ref(0) in set c := 1 end" "unit")
(prueba "while devuelve unit" "while false do 1 end" "unit")

;; Referencias: son valores de primera clase
(prueba "una referencia se escribe <ref n>" "ref(5)" "<ref 0>")
(prueba "una referencia puede guardarse en otra celda"
        "let a = ref(1), b = ref(ref(7)) in deref(deref(b)) end" "7")
(prueba "dos referencias distintas no son iguales"
        "let a = ref(1), b = ref(1) in ==(a, b) end" "false")
(prueba "una referencia es igual a si misma"
        "let a = ref(1) in ==(a, a) end" "true")
(prueba-error "deref de un entero" "deref(5)" "referencia")
(prueba-error "set sobre un entero" "set 5 := 3" "referencia")

;; Procedimientos
(prueba "un procedimiento se escribe <procedimiento>"
        "proc (x: int) x end" "<procedimiento>")
(prueba "un procedimiento se guarda en una variable"
        "let f = proc (x: int) x end in f end" "<procedimiento>")
(prueba "procedimiento sin parametros"
        "let a = 1 in let f = proc () a end in apply f() end end" "1")
(prueba "un procedimiento que devuelve otro (clausura)"
        "let suma = proc (a: int) proc (b: int) +(a, b) end end in apply apply suma(3)(4) end"
        "7")

;; print acepta cualquier valor y devuelve unit
(prueba "print de una referencia" "print(ref(1))" "unit")
(prueba "print de un procedimiento" "print(proc (x: int) x end)" "unit")
(prueba "print de un flotante" "print(2.5)" "unit")

(eopl:printf "~%==========================================~%")
(eopl:printf "  valores: pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")