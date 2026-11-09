#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

#|
  Batería de pruebas del interpretador de TxLang-UV.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. La primera entrega deja aquí al
  menos 15 programas que cubran todo su alcance; la segunda, al menos 20 que
  cubran el proyecto completo y al menos 5 programas mal tipados con el
  error que el chequeador reporta en cada uno.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; El chequeador asigna al programa el tipo esperado.
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; El programa se rechaza, por el chequeador o por el intérprete.
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del árbol de un texto vuelve a parsear al mismo árbol.
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; ==========================================================
;;; Primera entrega: léxico, gramática, unparse y núcleo
;;; ==========================================================

(eopl:printf "~%== Primera entrega ==~%")

(prueba "literal entero" "42" "42")
(ida-y-vuelta "unparse de un literal" "42")

;; Literales de cada clase. Las cadenas y los caracteres se escriben con sus
;; comillas, igual que en el programa.
(prueba "entero negativo" "-15" "-15")
(prueba "flotante" "3.14" "3.14")
(prueba "flotante negativo" "-2.5" "-2.5")
(prueba "cadena" "\"hola\"" "\"hola\"")
(prueba "caracter" "'a'" "'a'")
(prueba "booleano true" "true" "true")
(prueba "booleano false" "false" "false")
(prueba "valor unit" "unit" "unit")

;; TAD almacen: se prueba directo, sin pasar por el interpretador.
(initialize-store!)
(verificar "newref devuelve la primera ubicacion" (number->string (newref 7)) "0")
(verificar "newref devuelve la siguiente ubicacion" (number->string (newref 8)) "1")
(verificar "deref lee lo guardado" (number->string (deref 0)) "7")
(verificar "setref! cambia el contenido"
           (begin (setref! 0 9) (number->string (deref 0)))
           "9")
(verificar "referencia-valida? acepta una ubicacion existente"
           (if (referencia-valida? 1) "si" "no")
           "si")
(verificar "referencia-valida? rechaza una ubicacion inexistente"
           (if (referencia-valida? 5) "si" "no")
           "no")

;; let e identificadores
(prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
(prueba "let anidado" "let x = 5 in let y = 7 in *(x, y) end end" "35")
(prueba "let oculta una variable" "let x = 1 in let x = 2 in x end end" "2")
(prueba "las ligaduras de un let no se ven entre si"
        "let x = 1 in let x = 2, y = x in y end end"
        "1")

;; ref, deref y set
(prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
(prueba "deref de una referencia recien creada" "deref(ref(10))" "10")
(prueba "dos celdas independientes"
        "let a = ref(1), b = ref(2) in begin set a := 10; +(deref(a), deref(b)) end end"
        "12")

;; begin, if con elif y while
(prueba "begin devuelve la ultima expresion" "begin 1; 2; 3 end" "3")
(prueba "if toma la rama then" "if true then 1 else 2 end" "1")
(prueba "if toma la rama else" "if false then 1 else 2 end" "2")
(prueba "if con elif" "if false then 1 elif true then 2 else 3 end" "2")
(prueba "if con varios elif"
        "if false then 1 elif false then 2 elif true then 3 else 4 end"
        "3")
(prueba "while suma los primeros numeros"
        "let i = ref(0), s = ref(0) in begin while <(deref(i), 4) do begin set s := +(deref(s), deref(i)); set i := +(deref(i), 1) end end; deref(s) end end"
        "6")
(prueba "while que no entra"
        "let i = ref(5) in begin while <(deref(i), 0) do set i := 0 end; deref(i) end end"
        "5")

;; Primitivas aritmeticas y relacionales
(prueba "suma" "+(2, 3)" "5")
(prueba "resta" "-(10, 4)" "6")
(prueba "multiplicacion" "*(3, 4)" "12")
(prueba "division" "/(10, 2)" "5")
(prueba "residuo" "%(10, 3)" "1")
(prueba "suma con flotantes" "+(1.5, 2.5)" "4.0")
(prueba "mayor" ">(2, 1)" "true")
(prueba "menor" "<(2, 1)" "false")
(prueba "mayor o igual" ">=(2, 2)" "true")
(prueba "menor o igual" "<=(1, 2)" "true")
(prueba "igual" "==(3, 3)" "true")
(prueba "diferente" "!=(3, 3)" "false")

;; Primitivas booleanas, de cadenas y de conversion
(prueba "and" "and(true, false)" "false")
(prueba "or" "or(false, true)" "true")
(prueba "not" "not(true)" "false")
(prueba "concat" "concat(\"ho\", \"la\")" "\"hola\"")
(prueba "length" "length(\"hola\")" "4")
(prueba "to-string" "to-string(5)" "\"5\"")
(prueba "to-number" "to-number(\"42\")" "42")
;; print escribe hola en pantalla y devuelve unit.
(prueba "print devuelve unit" "print(\"hola\")" "unit")

;; Procedimientos y aplicacion
(prueba "proc y apply" "let f = proc (x: int) +(x, 1) end in apply f(41) end" "42")
(prueba "proc de dos parametros"
        "let suma = proc (a: int, b: int) +(a, b) end in apply suma(3, 4) end"
        "7")
(prueba "la clausura recuerda su ambiente"
        "let n = 10 in let f = proc (x: int) +(x, n) end in let n = 99 in apply f(1) end end end"
        "11")
(prueba "un procedimiento como argumento"
        "let aplica = proc (g: (int -> int), v: int) apply g(v) end in apply aplica(proc (x: int) *(x, 2) end, 21) end"
        "42")

;; letrec: recursion simple y recursion mutua
(prueba "letrec factorial"
        "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end in apply fact(5) end"
        "120")
(prueba "letrec fibonacci"
        "letrec int fib (n: int) = if <(n, 2) then n else +(apply fib(-(n, 1)), apply fib(-(n, 2))) end in apply fib(10) end"
        "55")
(prueba "letrec con recursion mutua"
        "letrec bool par (n: int) = if ==(n, 0) then true else apply impar(-(n, 1)) end bool impar (n: int) = if ==(n, 0) then false else apply par(-(n, 1)) end in apply par(10) end"
        "true")
(prueba "recursion mutua con respuesta falsa"
        "letrec bool par (n: int) = if ==(n, 0) then true else apply impar(-(n, 1)) end bool impar (n: int) = if ==(n, 0) then false else apply par(-(n, 1)) end in apply impar(10) end"
        "false")

;; Programas que el interpretador debe rechazar
(prueba-rechazo "identificador sin ligar" "x")
(prueba-rechazo "deref de algo que no es referencia" "deref(5)")
(prueba-rechazo "set sobre algo que no es referencia" "let x = 1 in set x := 2 end")
(prueba-rechazo "division por cero" "/(1, 0)")
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
(prueba-rechazo "while con condicion no booleana" "while 1 do 2 end")
(prueba-rechazo "and con un operando no booleano" "and(1, true)")
(prueba-rechazo "length de algo que no es cadena" "length(5)")
(prueba-rechazo "to-number de una cadena que no es numero" "to-number(\"abc\")")
(prueba-rechazo "primitiva con cantidad incorrecta de argumentos" "+(1)")
(prueba-rechazo "apply de algo que no es procedimiento" "apply 5(1)")
(prueba-rechazo "apply con cantidad incorrecta de argumentos"
                "let f = proc (x: int) x end in apply f(1, 2) end")

;; unparse: el texto que devuelve vuelve a parsear al mismo texto
(ida-y-vuelta "unparse de let" "let a = 1, b = 2 in +(a, b) end")
(ida-y-vuelta "unparse de if con elif" "if false then 1 elif true then 2 else 3 end")
(ida-y-vuelta "unparse de proc y apply" "let f = proc (x: int) +(x, 1) end in apply f(41) end")
(ida-y-vuelta "unparse de cadena y caracter" "begin \"hola\"; 'a' end")
(ida-y-vuelta "unparse de letrec"
              "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact(-(n, 1))) end in apply fact(5) end")

;;; ==========================================================
;;; Segunda entrega: transacciones, registros y tipos
;;; ==========================================================

(eopl:printf "~%== Segunda entrega ==~%")

;; Agregue aquí las pruebas de la segunda entrega: bloques atomic que
;; confirman y que revierten, anidamiento, registros, y al menos cinco
;; programas mal tipados con prueba-rechazo o prueba-tipo. Por ejemplo:
;;
;; (prueba-tipo "tipo de un proc" "proc (x: int) x end" "(int -> int)")
;; (prueba-rechazo "suma de int y bool" "+(1, true)")

;;; ==========================================================
;;; Resumen
;;; ==========================================================

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")