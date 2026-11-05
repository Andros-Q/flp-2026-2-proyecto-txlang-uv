#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

#|
  Interpretador de TxLang-UV.
  Proyecto final del curso Fundamentos de Interpretación y Compilación de
  Lenguajes de Programación, Universidad del Valle.

  El archivo va en nueve secciones, en el orden en que se construye un
  interpretador. La primera entrega llena las secciones 1 a 6 y la 8; la
  segunda agrega al almacén el registro que permite revertir, los registros
  del lenguaje y el chequeador de tipos de la sección 7.

  Lo que hay ahora es el andamiaje mínimo para que las baterías corran: una
  gramática con un solo literal, un value-of de una cláusula y la interfaz
  que usan pruebas-txlang.rkt y ejemplos-anexo.rkt. Todo lo demás lo pone el
  grupo, siguiendo la gramática y la semántica del enunciado.
|#

;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Los dos tipos de comentario, los literales de cada clase y los
;; identificadores. El comentario de bloque no admite | adentro.

(define especificacion-lexica
  '((espacio (whitespace) skip)
    ;;Comentario una linea
    (comentario ("//" (arbno (not #\newline))) skip)
    ;;Comentario Bloque
     (comentario ("#|" (arbno (not #\|)) "|#") skip)
     ;;Numeros Flotantes positivos
    (flotante (digit (arbno digit) "." digit(arbno digit))number)
    ;;Numeros Flotantes negativos
    (flotante ("-" digit (arbno digit) "." digit(arbno digit))number)
    ;;Numeros enteros 
    (numero (digit (arbno digit)) number)
    ;;Numeros enteros negativos
    (numero ("-" digit (arbno digit)) number)
    ;;Identificadores
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    ;;Chars caracteres
    (caracter ("'" any "'") string)
    ;;Cadenas de texto
    (cadena ("\"" (arbno (not #\")) "\"" ) string)
    ))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva. Los nombres de las variantes los escoge el grupo.

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)
    (decl-registro ("record" identificador "{"
     (separated-list campo-register "," ) "}")
                   decl-program)
    (campo-register (identificador ":" tipo) un-campo)
    ;;Tipos del lenguaje
    (tipo ("int") type-int)
    (tipo ("float") type-float)
    (tipo ("bool") type-bool)
    (tipo ("string") type-string)
    (tipo ("char") type-char)
    (tipo ("unit") type-unit)
    (tipo ("ref" "(" tipo ")" ) type-ref)
    (tipo ("(" (separated-list tipo ",")
               "->" tipo ")") type-tipo)
    (tipo (identificador) type-identificador)
    ;;Expresiones del lenguaje
    (expresion (numero) int-exp)
    (expresion (flotante) float-exp)
    (expresion (cadena) string-exp)
    (expresion (caracter) char-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) identificador-exp)
    (expresion ("let" (separated-list ligadura "," )
                "in" expresion "end") let-exp)

    (ligadura  
     (identificador "=" expresion) ligadura-exp)
    
     (expresion ("ref" "(" expresion ")") ref-exp)
     (expresion ("deref" "(" expresion ")") deref-exp)
     (expresion ("set" expresion ":=" expresion) set-exp)
     (expresion ("begin" expresion
             (arbno ";" expresion) "end") begin-exp)
     (expresion ("if" expresion "then" expresion
        (arbno "elif" expresion "then" expresion)
        "else" expresion "end") if-exp)
     (expresion ("while" expresion "do" expresion "end") while-exp)
     (expresion (primitiva "(" (separated-list expresion
       ",") ")") prim-exp)
     (expresion  ("proc" "(" (separated-list parametro ",") ")"
      expresion "end") proc-exp)
     ;;
     (parametro (identificador ":" tipo) un-parametro)

     (expresion
     ("apply" expresion "(" (separated-list expresion ",") ")")
     apply-exp)
     
     (expresion
      ("letrec"
      (arbno tipo identificador
             "(" (separated-list parametro ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)

     (expresion ("atomic" expresion "onfail"
       "(" identificador ")" expresion "end")
     atomic-exp)

     (expresion ("abort" "(" expresion ")") abort-exp)

     (expresion ("new" identificador "{"
      (separated-list ligadura ",") "}") new-exp)
      
     (expresion ("field"
      "(" expresion "," identificador ")") field-exp)

     ;; Primitivas aritmeticas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)

    ;; Primitivas relacionales
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") no-es-igual-prim)

    ;; Primitivas booleanas originales
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)

    ;;Primitivas para concatenar o ver cantidad de una lista
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)

    ;;Primitivas de conversion
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    ;;Primitiva de output
    (primitiva ("print") print-prim)
    
    ))

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; Muestra los define-datatype que SLLGEN generó a partir de la gramática.
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;;; ==========================================================
;;; 3. Unparse: del árbol de sintaxis abstracta al texto
;;; ==========================================================

(define unparse
  (lambda (pgm)
    (cases programa pgm
  (un-programa (declaraciones cuerpo)
    (unparse-expresion cuerpo)))
    ))
    ;;(cases programa pgm
      ;;(un-programa (cuerpo) (unparse-expresion cuerpo)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (int-exp (n) (number->string n)))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores. Su interfaz va aparte de la del almacén.

;;; ==========================================================
;;; 5. TAD almacén
;;; ==========================================================
;; Celdas que se reservan con ref, se leen con deref y se escriben con set.
;; En la primera entrega las escrituras son definitivas; en la segunda el
;; almacén lleva el registro que permite revertir un bloque atomic. Conviene
;; diseñar la interfaz desde ahora pensando en eso.

;;; ==========================================================
;;; 6. Valores expresados
;;; ==========================================================
;; Un define-datatype con los valores del lenguaje y valor->texto, que es
;; lo que imprime correr.

(define valor->texto
  (lambda (v)
    (cond ((number? v) (number->string v))
          (else (eopl:error 'valor->texto "no sé escribir el valor ~s" v)))))

;;; ==========================================================
;;; 7. Chequeador de tipos (segunda entrega)
;;; ==========================================================
;; Recibe el programa y lo devuelve si está bien tipado; si no, aborta con
;; eopl:error antes de que se evalúe. Mientras no exista, deja pasar todo.

(define chequear-programa
  (lambda (pgm) pgm))

;; Tipo del programa, escrito como texto: "int", "ref(bool)", "(int -> int)".
(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))

;;; ==========================================================
;;; 8. Interpretador
;;; ==========================================================

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) n))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (cuerpo) (value-of cuerpo 'ambiente-vacio)))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))
