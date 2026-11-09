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

(define especificacion-lexica
  '((espacio (whitespace) skip)
    ;;Comentario una linea
    (comentario ("//" (arbno (not #\newline))) skip)
    ;;Comentario Bloque
    (comentario ("#|" (arbno (not #\|)) "|#") skip)
    ;;Numeros Flotantes positivos
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    ;;Numeros Flotantes negativos
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ;;Numeros enteros
    (numero (digit (arbno digit)) number)
    ;;Numeros enteros negativos
    (numero ("-" digit (arbno digit)) number)
    ;;Identificadores
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    ;;Chars caracteres
    (caracter ("'" any "'") string)
    ;;Cadenas de texto
    (cadena ("\"" (arbno (not #\")) "\"") string)
    ))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================

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
    (expresion ("proc" "(" (separated-list parametro ",") ")"
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

;; Une una lista de textos con el separador indicado. La uso para las
;; listas de campos, parametros, argumentos y demas
(define unir-textos
  (lambda (textos separador)
    (cond
      ((null? textos) "")
      ((null? (cdr textos)) (car textos))
      (else (string-append (car textos)
                           separador
                           (unir-textos (cdr textos) separador))))))

;; El scanner guarda el lexema completo de cadenas y caracteres,
;; o sea que trae las comillas. Si el texto ya viene entre comillas lo dejo
;; igual; si no, se las pongo para que vuelva a parsear.
(define texto-cadena
  (lambda (s)
    (if (and (>= (string-length s) 2)
             (char=? (string-ref s 0) #\")
             (char=? (string-ref s (- (string-length s) 1)) #\"))
        s
        (string-append "\"" s "\""))))

(define texto-caracter
  (lambda (s)
    (if (and (>= (string-length s) 2)
             (char=? (string-ref s 0) #\')
             (char=? (string-ref s (- (string-length s) 1)) #\'))
        s
        (string-append "'" s "'"))))

;; Cada declaracion record va en su propia linea y despues va el cuerpo.
(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (declaraciones cuerpo)
        (if (null? declaraciones)
            (unparse-expresion cuerpo)
            (string-append
             (unir-textos
              (map (lambda (d)
                     (string-append (unparse-decl-registro d) "\n"))
                   declaraciones)
              "")
             (unparse-expresion cuerpo)))))))

(define unparse-decl-registro
  (lambda (decl)
    (cases decl-registro decl
      (decl-program (nombre campos)
        (string-append "record "
                       (symbol->string nombre)
                       " { "
                       (unir-textos (map unparse-campo campos) ", ")
                       " }")))))

(define unparse-campo
  (lambda (campo)
    (cases campo-register campo
      (un-campo (nombre tipo)
        (string-append (symbol->string nombre)
                       ": "
                       (unparse-tipo tipo))))))

(define unparse-tipo
  (lambda (t)
    (cases tipo t
      (type-int () "int")
      (type-float () "float")
      (type-bool () "bool")
      (type-string () "string")
      (type-char () "char")
      (type-unit () "unit")
      (type-ref (tipo-interno)
        (string-append "ref(" (unparse-tipo tipo-interno) ")"))
      (type-tipo (tipos retorno)
        (string-append "("
                       (unir-textos (map unparse-tipo tipos) ", ")
                       " -> "
                       (unparse-tipo retorno)
                       ")"))
      (type-identificador (nombre)
        (symbol->string nombre)))))

(define unparse-parametro
  (lambda (param)
    (cases parametro param
      (un-parametro (nombre tipo)
        (string-append (symbol->string nombre)
                       ": "
                       (unparse-tipo tipo))))))

(define unparse-ligadura
  (lambda (lig)
    (cases ligadura lig
      (ligadura-exp (nombre exp)
        (string-append (symbol->string nombre)
                       " = "
                       (unparse-expresion exp))))))

(define unparse-primitiva
  (lambda (prim)
    (cases primitiva prim
      (sum-prim () "+")
      (minus-prim () "-")
      (mult-prim () "*")
      (div-prim () "/")
      (mod-prim () "%")
      (mayor-prim () ">")
      (menor-prim () "<")
      (mayorigual-prim () ">=")
      (menorigual-prim () "<=")
      (igual-prim () "==")
      (no-es-igual-prim () "!=")
      (and-prim () "and")
      (or-prim () "or")
      (not-prim () "not")
      (concat-prim () "concat")
      (length-prim () "length")
      (to-string-prim () "to-string")
      (to-number-prim () "to-number")
      (print-prim () "print"))))

;; Las ramas elif vienen en dos listas paralelas: primero las condiciones
;; y despues los cuerpos. Las recorro al mismo tiempo.
(define unparse-elifs
  (lambda (condiciones cuerpos)
    (if (null? condiciones)
        ""
        (string-append " elif "
                       (unparse-expresion (car condiciones))
                       " then "
                       (unparse-expresion (car cuerpos))
                       (unparse-elifs (cdr condiciones) (cdr cuerpos))))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (int-exp (n)
        (number->string n))
      (float-exp (n)
        (number->string n))
      (string-exp (s)
        (texto-cadena s))
      (char-exp (s)
        (texto-caracter s))
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (identificador-exp (nombre)
        (symbol->string nombre))
      (let-exp (ligaduras cuerpo)
        (string-append "let "
                       (unir-textos (map unparse-ligadura ligaduras) ", ")
                       " in "
                       (unparse-expresion cuerpo)
                       " end"))
      (ref-exp (expresion)
        (string-append "ref(" (unparse-expresion expresion) ")"))
      (deref-exp (expresion)
        (string-append "deref(" (unparse-expresion expresion) ")"))
      (set-exp (izquierda derecha)
        (string-append "set "
                       (unparse-expresion izquierda)
                       " := "
                       (unparse-expresion derecha)))
      (begin-exp (primera resto)
        (string-append "begin "
                       (unir-textos
                        (map unparse-expresion (cons primera resto))
                        "; ")
                       " end"))
      (if-exp (prueba entonces elifs elif-cuerpos sino)
        (string-append "if "
                       (unparse-expresion prueba)
                       " then "
                       (unparse-expresion entonces)
                       (unparse-elifs elifs elif-cuerpos)
                       " else "
                       (unparse-expresion sino)
                       " end"))
      (while-exp (condicion cuerpo)
        (string-append "while "
                       (unparse-expresion condicion)
                       " do "
                       (unparse-expresion cuerpo)
                       " end"))
      (prim-exp (prim args)
        (string-append (unparse-primitiva prim)
                       "("
                       (unir-textos (map unparse-expresion args) ", ")
                       ")"))
      (proc-exp (parametros cuerpo)
        (string-append "proc ("
                       (unir-textos (map unparse-parametro parametros) ", ")
                       ") "
                       (unparse-expresion cuerpo)
                       " end"))
      (apply-exp (operador args)
        (string-append "apply "
                       (unparse-expresion operador)
                       "("
                       (unir-textos (map unparse-expresion args) ", ")
                       ")"))
      (letrec-exp (tipos nombres parametros cuerpos cuerpo)
        (string-append "letrec "
                       (unir-textos
                        (map (lambda (tipo nombre params exp)
                               (string-append (unparse-tipo tipo)
                                              " "
                                              (symbol->string nombre)
                                              " ("
                                              (unir-textos
                                               (map unparse-parametro params)
                                               ", ")
                                              ") = "
                                              (unparse-expresion exp)))
                             tipos nombres parametros cuerpos)
                        " ")
                       " in "
                       (unparse-expresion cuerpo)
                       " end"))
      (atomic-exp (cuerpo identificador manejador)
        (string-append "atomic "
                       (unparse-expresion cuerpo)
                       " onfail ("
                       (symbol->string identificador)
                       ") "
                       (unparse-expresion manejador)
                       " end"))
      (abort-exp (mensaje)
        (string-append "abort("
                       (unparse-expresion mensaje)
                       ")"))
      (new-exp (nombre campos)
        (string-append "new "
                       (symbol->string nombre)
                       " { "
                       (unir-textos (map unparse-ligadura campos) ", ")
                       " }"))
      (field-exp (registro campo)
        (string-append "field("
                       (unparse-expresion registro)
                       ", "
                       (symbol->string campo)
                       ")")))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores. Su interfaz va aparte de la del almacén.

;;Creacion de datatype de ambiente con su variante de ambiente extendido y otra variante de ambiente recursivo
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
    (ids (list-of symbol?))
    (valores list?)
    (env-old ambiente?))
  (ambiente-recursivo
    (proc-names (list-of symbol?))
    (idss list?)
    (bodies (list-of expresion?))
    (env-old ambiente?)))

;; apply-env : ambiente x symbol -> valor
;; Busca el identificador en el ambiente, respetando el shadowing de las
;; ligaduras más recientes, y levanta eopl:error si no está ligado.
(define apply-env
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env
                    "No se encontro el identificador: ~s"
                    id))
      (ambiente-extendido (ids valores env-old)
        (letrec
            ((buscar-listas
              (lambda (id ids valores)
                (cond
                  ((null? ids)
                   (apply-env env-old id))
                  ((eq? (car ids) id)
                   (car valores))
                  (else
                   (buscar-listas id (cdr ids) (cdr valores)))))))
          (buscar-listas id ids valores)))
      (ambiente-recursivo (proc-names idss bodies env-old)
        (letrec
            ((buscar-procname
              (lambda (proc-names idss bodies currently-env)
                (cond
                  ((null? proc-names)
                   (apply-env env-old id))
                  ((eq? (car proc-names) id)
                   (cerradura (car idss) (car bodies)
                              currently-env))
                  (else
                   (buscar-procname (cdr proc-names)
                                    (cdr idss)
                                    (cdr bodies)
                                    currently-env))))))
          (buscar-procname proc-names idss bodies env))))))

;;; ==========================================================
;;; 5. TAD almacén
;;; ==========================================================

;; El almacen se representa mediante una lista.
;; Cada posicion de la lista representa una ubicacion.
;;
;; Ejemplo:
;;   Ubicacion 0 -> primer elemento
;;   Ubicacion 1 -> segundo elemento
;;   Ubicacion 2 -> tercer elemento
;;
;; En esta primera entrega las modificaciones son definitivas.
;; En la segunda entrega se ampliara para soportar transacciones.

(define the-store '())

;; Inicializa el almacen vacio.
(define initialize-store!
  (lambda ()
    (set! the-store '())))

;; Comprueba que una ubicacion exista en el almacen.
(define referencia-valida?
  (lambda (ubicacion)
    (and (integer? ubicacion)
         (>= ubicacion 0)
         (< ubicacion (length the-store)))))

;; Reserva una nueva celda y devuelve su ubicacion.
(define newref
  (lambda (valor)
    (let ((ubicacion (length the-store)))
      (begin
        (set! the-store (append the-store (list valor)))
        ubicacion))))

;; Consulta el contenido de una ubicacion.
(define deref
  (lambda (ubicacion)
    (if (referencia-valida? ubicacion)
        (list-ref the-store ubicacion)
        (eopl:error 'deref
                    "ubicacion invalida: ~s"
                    ubicacion))))

;; Funcion auxiliar que reemplaza un elemento de una lista.
(define reemplazar-posicion
  (lambda (lista posicion nuevo-valor)
    (cond
      ((null? lista)
       (eopl:error 'reemplazar-posicion
                   "posicion inexistente: ~s"
                   posicion))
      ((= posicion 0)
       (cons nuevo-valor (cdr lista)))
      (else
       (cons (car lista)
             (reemplazar-posicion
              (cdr lista)
              (- posicion 1)
              nuevo-valor))))))

;; Modifica el contenido de una ubicacion existente.
(define setref!
  (lambda (ubicacion nuevo-valor)
    (if (referencia-valida? ubicacion)
        (begin
          (set! the-store
                (reemplazar-posicion
                 the-store ubicacion nuevo-valor))
          'unit)
        (eopl:error 'setref!
                    "ubicacion invalida: ~s"
                    ubicacion))))

;;; ==========================================================
;;; 6. Valores expresados
;;; ==========================================================
;; Un define-datatype con los valores del lenguaje y valor->texto, que es
;; lo que imprime correr.

;;Datatype del procedure (cerradura)clausula donde se guarda el contenido de la funcion recursiva
(define-datatype procval procval?
  (cerradura
    (parametros list?)
    (cuerpo expresion?)
    (ambiente ambiente?)))

(define entero-txl?
  (lambda (v) (and (integer? v) (exact? v))))

(define flotante-txl?
  (lambda (v) (and (real? v) (inexact? v))))

(define numero-txl?
  (lambda (v) (or (entero-txl? v) (flotante-txl? v))))

;; Convierte el valor que devuelve value-of en el texto que imprime correr.
;; Las cadenas y los caracteres se escriben con sus comillas, igual que en el
;; programa. Las referencias y los procedimientos no tienen literal propio,
;; por eso se escriben entre < >. Los demas valores (numeros, booleanos y
;; unit) usan la misma conversion que las primitivas.
(define valor->texto
  (lambda (v)
    (cond
      ((string? v) (string-append "\"" v "\""))
      ((char? v) (string-append "'" (string v) "'"))
      ((referencia-interprete? v)
       (string-append "<ref " (number->string (ubicacion-referencia v)) ">"))
      ((procval? v) "<procedimiento>")
      (else (valor-interprete-a-texto v)))))

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

;; Obtiene el contenido de una cadena quitando las comillas externas.
(define quitar-comillas
  (lambda (texto)
    (if (and (>= (string-length texto) 2)
             (char=? (string-ref texto 0) #\")
             (char=? (string-ref texto (- (string-length texto) 1)) #\"))
        (substring texto 1 (- (string-length texto) 1))
        texto)))

;; Obtiene el contenido de un caracter quitando las comillas simples.
(define quitar-comillas-caracter
  (lambda (texto)
    (if (and (>= (string-length texto) 2)
             (char=? (string-ref texto 0) #\')
             (char=? (string-ref texto (- (string-length texto) 1)) #\'))
        (substring texto 1 (- (string-length texto) 1))
        texto)))

;; Comprueba que un valor sea booleano.
(define comprobar-booleano
  (lambda (valor expresion)
    (if (boolean? valor)
        valor
        (eopl:error 'value-of
                    "Se esperaba un booleano en: ~s"
                    expresion))))

;; Comprueba la cantidad de argumentos de una primitiva.
(define comprobar-argumentos
  (lambda (args cantidad nombre)
    (if (= (length args) cantidad)
        args
        (eopl:error 'value-of
                    "La primitiva ~s necesita ~s argumentos"
                    nombre cantidad))))

(define comprobar-numeros
  (lambda (args nombre)
    (if (and (numero-txl? (car args)) (numero-txl? (cadr args)))
        args
        (eopl:error 'value-of
                    "La primitiva ~s necesita dos numeros y recibio: ~s y ~s"
                    nombre (car args) (cadr args)))))

(define comprobar-enteros
  (lambda (args nombre)
    (if (and (entero-txl? (car args)) (entero-txl? (cadr args)))
        args
        (eopl:error 'value-of
                    "La primitiva ~s solo acepta enteros y recibio: ~s y ~s"
                    nombre (car args) (cadr args)))))

(define valor-a-imprimir
  (lambda (v)
    (if (or (referencia-interprete? v) (procval? v))
        (valor->texto v)
        (valor-interprete-a-texto v))))

;; Obtiene el nombre de un parametro.
(define nombre-parametro
  (lambda (param)
    (cases parametro param
      (un-parametro (nombre tipo)
        nombre))))

;; Obtiene el nombre de una ligadura.
(define nombre-ligadura
  (lambda (lig)
    (cases ligadura lig
      (ligadura-exp (nombre exp)
        nombre))))

;; Evalua las expresiones de las ligaduras en el ambiente exterior.
;; Las ligaduras de un mismo let no se ven entre si.
(define valores-ligaduras
  (lambda (ligaduras amb)
    (if (null? ligaduras)
        '()
        (cases ligadura (car ligaduras)
          (ligadura-exp (nombre exp)
            (cons (value-of exp amb)
                  (valores-ligaduras (cdr ligaduras) amb)))))))

;; Evalua las expresiones de una secuencia y devuelve la ultima.
(define evaluar-secuencia
  (lambda (primera resto amb)
    (if (null? resto)
        (value-of primera amb)
        (begin
          (value-of primera amb)
          (evaluar-secuencia (car resto) (cdr resto) amb)))))

;; Evalua las condiciones elif hasta encontrar una verdadera.
(define evaluar-elif
  (lambda (condiciones cuerpos sino amb)
    (if (null? condiciones)
        (value-of sino amb)
        (let ((resultado (comprobar-booleano
                          (value-of (car condiciones) amb)
                          (car condiciones))))
          (if resultado
              (value-of (car cuerpos) amb)
              (evaluar-elif (cdr condiciones)
                            (cdr cuerpos)
                            sino
                            amb))))))

;; Ejecuta el ciclo mientras su condicion sea verdadera.
(define evaluar-while
  (lambda (condicion cuerpo amb)
    (let ((resultado (comprobar-booleano
                      (value-of condicion amb)
                      condicion)))
      (if resultado
          (begin
            (value-of cuerpo amb)
            (evaluar-while condicion cuerpo amb))
          'unit))))

;; Convierte valores del lenguaje a texto para las primitivas.
(define valor-interprete-a-texto
  (lambda (valor)
    (cond
      ((number? valor) (number->string valor))
      ((string? valor) valor)
      ((char? valor) (string valor))
      ((boolean? valor)
       (if valor "true" "false"))
      ((eq? valor 'unit) "unit")
      (else
       (eopl:error 'value-of
                   "No se puede convertir el valor: ~s"
                   valor)))))

;; Evalua las primitivas del lenguaje.
(define evaluar-primitiva
  (lambda (prim args)
    (cases primitiva prim

      ;; Primitivas aritmeticas
      (sum-prim ()
        (comprobar-argumentos args 2 '+)
        (comprobar-numeros args '+)
        (+ (car args) (cadr args)))

      (minus-prim ()
        (comprobar-argumentos args 2 '-)
        (comprobar-numeros args '-)
        (- (car args) (cadr args)))

      (mult-prim ()
        (comprobar-argumentos args 2 '*)
        (comprobar-numeros args '*)
        (* (car args) (cadr args)))

      (div-prim ()
        (comprobar-argumentos args 2 '/)
        (comprobar-numeros args '/)
        (if (zero? (cadr args))
            (eopl:error 'value-of "Division por cero")
            (if (and (entero-txl? (car args))
                     (entero-txl? (cadr args)))
                (quotient (car args) (cadr args))
                (/ (car args) (cadr args)))))

      (mod-prim ()
        (comprobar-argumentos args 2 '%)
        (comprobar-enteros args '%)
        (if (zero? (cadr args))
            (eopl:error 'value-of "Division por cero")
            (remainder (car args) (cadr args))))

      ;; Primitivas relacionales
      (mayor-prim ()
        (comprobar-argumentos args 2 '>)
        (comprobar-numeros args '>)
        (> (car args) (cadr args)))

      (menor-prim ()
        (comprobar-argumentos args 2 '<)
        (comprobar-numeros args '<)
        (< (car args) (cadr args)))

      (mayorigual-prim ()
        (comprobar-argumentos args 2 '>=)
        (comprobar-numeros args '>=)
        (>= (car args) (cadr args)))

      (menorigual-prim ()
        (comprobar-argumentos args 2 '<=)
        (comprobar-numeros args '<=)
        (<= (car args) (cadr args)))

      (igual-prim ()
        (comprobar-argumentos args 2 '==)
        (equal? (car args) (cadr args)))

      (no-es-igual-prim ()
        (comprobar-argumentos args 2 '!=)
        (not (equal? (car args) (cadr args))))

      ;; Primitivas booleanas
      (and-prim ()
        (comprobar-argumentos args 2 'and)
        (and (comprobar-booleano (car args) 'and)
             (comprobar-booleano (cadr args) 'and)))

      (or-prim ()
        (comprobar-argumentos args 2 'or)
        (or (comprobar-booleano (car args) 'or)
            (comprobar-booleano (cadr args) 'or)))

      (not-prim ()
        (comprobar-argumentos args 1 'not)
        (not (comprobar-booleano (car args) 'not)))

      ;; Primitivas para cadenas
      (concat-prim ()
        (comprobar-argumentos args 2 'concat)
        (if (and (string? (car args))
                 (string? (cadr args)))
            (string-append (car args) (cadr args))
            (eopl:error 'value-of
                        "concat necesita dos cadenas")))

      (length-prim ()
        (comprobar-argumentos args 1 'length)
        (if (string? (car args))
            (string-length (car args))
            (eopl:error 'value-of
                        "length necesita una cadena")))

      ;; Primitivas de conversion
      (to-string-prim ()
        (comprobar-argumentos args 1 'to-string)
        (valor-interprete-a-texto (car args)))

      (to-number-prim ()
        (comprobar-argumentos args 1 'to-number)
        (if (string? (car args))
            (let ((numero (string->number (car args))))
              (if numero
                  numero
                  (eopl:error 'value-of
                              "La cadena no representa un numero: ~s"
                              (car args))))
            (eopl:error 'value-of
                        "to-number necesita una cadena")))

      ;; Primitiva de output
      (print-prim ()
        (comprobar-argumentos args 1 'print)
        (display (valor-a-imprimir (car args)))
        (newline)
        'unit))))

;; Representacion de las referencias del lenguaje.
;; La ubicacion real se guarda en el TAD almacen.
(define referencia-interprete?
  (lambda (valor)
    (and (list? valor)
         (= (length valor) 2)
         (eq? (car valor) 'referencia)
         (integer? (cadr valor)))))

;; Obtiene la ubicacion de una referencia.
(define ubicacion-referencia
  (lambda (valor)
    (if (referencia-interprete? valor)
        (cadr valor)
        (eopl:error 'value-of
                    "Se esperaba una referencia y se recibio: ~s"
                    valor))))

;; Evalua una aplicacion de procedimiento.
(define aplicar-procedimiento
  (lambda (procedimiento argumentos)
    (if (procval? procedimiento)
        (cases procval procedimiento
          (cerradura (parametros cuerpo ambiente-guardado)
            (if (= (length parametros) (length argumentos))
                (let ((nombres (map nombre-parametro parametros)))
                  (value-of cuerpo
                            (ambiente-extendido
                             nombres
                             argumentos
                             ambiente-guardado)))
                (eopl:error 'value-of
                            "Cantidad incorrecta de argumentos"))))
        (eopl:error 'value-of
                    "Se intento aplicar un valor que no es procedimiento: ~s"
                    procedimiento))))

(define value-of
  (lambda (exp amb)
    (cases expresion exp

      ;; Literales
      (int-exp (n) n)
      (float-exp (n) n)
      (string-exp (s) (quitar-comillas s))
      (char-exp (s)
        (let ((contenido (quitar-comillas-caracter s)))
          (if (= (string-length contenido) 1)
              (string-ref contenido 0)
              (eopl:error 'value-of
                          "Caracter invalido: ~s"
                          s))))
      (true-exp () #t)
      (false-exp () #f)
      (unit-exp () 'unit)

      ;; Variables
      (identificador-exp (id)
        (apply-env amb id))

      ;; Ligaduras locales
      (let-exp (ligaduras cuerpo)
        (let ((nombres (map nombre-ligadura ligaduras))
              (valores (valores-ligaduras ligaduras amb)))
          (value-of cuerpo
                    (ambiente-extendido nombres valores amb))))

      ;; Referencias
      (ref-exp (expresion)
        (list 'referencia
              (newref (value-of expresion amb))))

      (deref-exp (expresion)
        (deref
         (ubicacion-referencia
          (value-of expresion amb))))

      (set-exp (izquierda derecha)
        (let ((referencia (value-of izquierda amb))
              (nuevo-valor (value-of derecha amb)))
          (setref! (ubicacion-referencia referencia)
                   nuevo-valor)
          'unit))

      ;; Secuencia de expresiones
      (begin-exp (primera resto)
        (evaluar-secuencia primera resto amb))

      ;; Condicionales
      (if-exp (prueba entonces elifs elif-cuerpos sino)
        (let ((resultado (comprobar-booleano
                          (value-of prueba amb)
                          prueba)))
          (if resultado
              (value-of entonces amb)
              (evaluar-elif elifs elif-cuerpos sino amb))))

      ;; Ciclos
      (while-exp (condicion cuerpo)
        (evaluar-while condicion cuerpo amb))

      ;; Primitivas
      (prim-exp (prim args)
        (evaluar-primitiva
         prim
         (map (lambda (arg)
                (value-of arg amb))
              args)))

      ;; Procedimientos y clausuras
      (proc-exp (parametros cuerpo)
        (cerradura parametros cuerpo amb))

      ;; Aplicacion de procedimientos
      (apply-exp (operador args)
        (let ((procedimiento (value-of operador amb))
              (argumentos
               (map (lambda (arg)
                      (value-of arg amb))
                    args)))
          (aplicar-procedimiento procedimiento argumentos)))

      ;; Recursion mediante letrec
      (letrec-exp (tipos nombres parametros cuerpos cuerpo)
        (let ((ambiente-rec
               (ambiente-recursivo
                nombres
                parametros
                cuerpos
                amb)))
          (value-of cuerpo ambiente-rec)))

      ;; Expresiones reservadas para la segunda entrega.
      (else
       (eopl:error 'value-of
                   "Esta expresion no pertenece al alcance de la primera entrega: ~s"
                   exp)))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        ;; Cada programa empieza con el almacen vacio.
        (initialize-store!)
        (cases programa pgm
          (un-programa (declaraciones cuerpo)
            (value-of cuerpo (ambiente-vacio))))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))