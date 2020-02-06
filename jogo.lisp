;;Código relacionado com o problema
;; Autores: André Reis e Bruno Alves
;; Ano letivo 19/20
(defpackage :jogo)

;;;;;;;;;;;;;;;;;;;;;;;; Seletores ;;;;;;;;;;;;;;;;;;;;;;;;
(defun linha (index tabuleiro)
  (cond
   ((NULL tabuleiro) NIL)
   ((equal index 0) (car tabuleiro))
   (T (linha (- index 1) (cdr tabuleiro)))
   ))

(defun celula (x y tabuleiro)
  (cond
   ((NULL tabuleiro) NIL)
   (T (labels ((atomo (i l)
       (cond
        ((NULL l) NIL)
        ((equal i 0) (car l))
        (T (atomo (- i 1) (cdr l)))
        ))) (atomo y (linha x tabuleiro))))
   ))

(defun indice-linha(linha valor &optional (index 0))
  (cond
   ((null linha) nil)
   ((null (car linha)) (indice-linha (cdr linha) valor (1+ index)))
   ((= (car linha) valor) index)
   (T (indice-linha (cdr linha) valor (1+ index)))
   )
)

(defun casa-melhor-valor(linha &optional (melhor-valor nil) (linha-inicial linha))
  (let ((nova-linha (remover-se #'(lambda (x) (null x)) linha)))
    (cond
     ((and (null nova-linha) (null melhor-valor)) nil)
     ((null nova-linha) (indice-linha linha-inicial melhor-valor))
     (T (casa-melhor-valor (cdr nova-linha) (if(or (null melhor-valor) (> (car nova-linha) melhor-valor)) (car nova-linha) melhor-valor) linha-inicial))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;; Construção tabuleiro ;;;;;;;;;;;;;;;;;;;;;;;;
(defun lista-numeros(&optional (n 100))
  "Recebe um número positivo n e cria uma lista com todos os números entre 0 e n"
  (cond
   ((< n 0) nil)
   ((equal n 1) ( cons (- n 1) NIL))
   (T (cons (- n 1) (lista-numeros (- n 1))))
   )
)

(defun baralhar(lista)
  "Baralha uma lista de números"
  (cond
   ((NULL lista) NIL)
   (T (let ((n (nth (random (length lista)) lista)))
        (cons n (baralhar (remover-se #'(lambda (x) (= x n)) lista)))
        )
   )))

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
  "Gera um tabuleiro aleatorio"
  (cond
   ((null lista) nil)
   (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;; Posicionamento ;;;;;;;;;;;;;;;;;;;;;;;;
(defun substituir-posicao(x lista &optional (valor NIL))
  "Substituir a posição x da lista pela variavel valor"
  (cond
   ((NULL lista) NIL)
   ((equal x 0) (cons valor (cdr lista)))
   (T (cons (car lista) (substituir-posicao (- x 1) (cdr lista) valor)))
   ))

(defun substituir(x y tabuleiro &optional (valor NIL))
  "Substituir a posição (x y) da matriz pela variavel valor"
  (cond 
   ((or (NULL tabuleiro)) NIL)
   ((equal x 0) (cons (substituir-posicao y (car tabuleiro) valor) 
                               (substituir (- x 1) y (cdr tabuleiro) valor)))
   (T (cons (car tabuleiro) (substituir (- x 1) y (cdr tabuleiro) valor)))
   ))

(defun obter-posicao(x y tabuleiro)
  "Verifica se uma posição está dentro dos limites do tabuleiro e depois retorna o seu valor."
  (if (AND (>= x 0) (<= x (1- (length tabuleiro))) (>= y 0) (<= y (1- (length tabuleiro))) (NOT (NULL tabuleiro)))
      (nth y (nth x tabuleiro))
    NIL))

(defun obter-valor-linha(valor linha &optional (y 0))
  (cond
   ((NULL linha) NIL)
   ((equal valor (car linha)) y)
   (T (obter-valor-linha valor (cdr linha) (1+ y)))
   )
)

(defun obter-valor(valor tabuleiro &optional (x 0))
  (let ((y (obter-valor-linha valor (car tabuleiro))))
    (cond
     ((NULL tabuleiro) NIL)
     ((NULL y) (obter-valor valor (cdr tabuleiro) (1+ x)))
     (T (cons x (cons y nil)))
     )
    )
)

(defun posicao-jogador(tabuleiro jogador)
  "Verifica se o cavalo está no tabuleiro e caso esteja retorna a posição do mesmo"
  (cond
   ((NULL tabuleiro) NIL)
   ((NULL (find jogador (car tabuleiro))) (posicao-jogador (cdr tabuleiro) jogador))
   (T (list (- (length (car tabuleiro)) (length tabuleiro)) (- (length (car tabuleiro)) (length (member jogador (car tabuleiro))))))
  ))


(defun colocar-jogador(no jogador)
  (let* ((pos-cavalo (posicao-jogador (no-estado no) jogador))
        (linha (if(= jogador -1) 0 (1- (length (no-estado no)))))
        (pos-cavalo-y (casa-melhor-valor (linha linha (no-estado no))))
        (valor-nova-posicao (obter-posicao linha pos-cavalo-y (no-estado no)))
        )
    (cond
     ((and (NULL pos-cavalo) 
           (NOT (NULL valor-nova-posicao))
           (or (NOT (equal jogador valor-nova-posicao))
               (NOT (equal (trocar-jogador jogador) valor-nova-posicao))))
      (let* ((tabuleiro1 (substituir linha pos-cavalo-y (no-estado no) jogador))
             (simetrico (posicao-simetrico (celula linha pos-cavalo-y (no-estado no)) tabuleiro1)))
        (cond
         ((NULL simetrico) (construir-no (substituir linha pos-cavalo-y (no-estado no) jogador) no valor-nova-posicao))
         (T (construir-no (substituir (car simetrico) (cadr simetrico) tabuleiro1 'NIL) no valor-nova-posicao))
         )
        ))
     (T (no-estado no))
     )
    ))

(defun escolher-posicao-jogador(coordenadas tabuleiro jogador)
  (cond
   ((or (null coordenadas) (null tabuleiro) (null jogador)) nil)
   ((and (= jogador -1) (= 0 (first coordenadas))) (substituir (first coordenadas) (second coordenadas) tabuleiro -1))
   ((and (= jogador -2) (= (1- (length tabuleiro)) (first coordenadas))) (substituir (first coordenadas) (second coordenadas) tabuleiro -2))
   (T nil)
   )
)

;;;;;;;;;;;;;;;;;;;;;;;; Funções auxiliares aos operadores ;;;;;;;;;;;;;;;;;;;;;;;;
(defun numero-para-lista(n)
  (loop for c across (write-to-string n) collect (digit-char-p c))
)

(defun maior-duplo-linha(linha &optional (max-duplo NIL))
  (let ((number-list (numero-para-lista (car linha))))
    (cond
     ((NULL linha) max-duplo)
     ((AND (NULL max-duplo) (NOT (EQUAL T (car linha))) (equal (car number-list) (cadr number-list))) (maior-duplo-linha (cdr linha) (car linha)))
     ((AND (NOT ( NULL max-duplo)) (NOT ( NULL (car linha)))
           (NOT (EQUAL T max-duplo)) (NOT (EQUAL T (car linha)))
           (equal (car number-list) (cadr number-list)) (< max-duplo (car linha))) 
      (maior-duplo-linha (cdr linha) (car linha)))
     (T (maior-duplo-linha (cdr linha) max-duplo))
     )
    ))

(defun contem(lista valor)
  (cond
   ((null lista) nil)
   ((equal (car lista) valor))
   (T (contem (cdr lista) valor))
   )
  )

(defun maior-duplo(tabela &optional (max-duplo NIL))
  (let ((max-linha (maior-duplo-linha (car tabela))))
    (cond 
     ((NULL tabela) max-duplo)
     ((AND (NULL max-duplo) (NOT (NULL max-linha)) (NOT (EQUAL T max-linha))) (maior-duplo (cdr tabela) max-linha))
     ((AND (NOT ( NULL max-duplo)) (NOT ( NULL max-linha)) 
           (NOT (EQUAL T max-duplo)) (NOT (EQUAL T max-linha)) (< max-duplo max-linha)) 
      (maior-duplo (cdr tabela) max-linha))
     (T (maior-duplo (cdr tabela) max-duplo))
     )
    ))

(defun reverter-lista (l)
  (cond
   ((null l) '())
   (T (append (reverter-lista (cdr l)) (list (car l))))
   ))

(defun posicao-simetrico-linha(n linha &optional (y 0))
  (let* ((numero-lista (numero-para-lista n)) (car-linha-lista (numero-para-lista (car linha))))
    (cond
     ((NULL linha) NIL)
     ((equal 1 (length numero-lista)) (cond
                                       ((equal (cons n (cons 0 nil)) car-linha-lista) y)
                                       (T (posicao-simetrico-linha n (cdr linha) (1+ y)))
                                       ))
     ((and (equal (car numero-lista) (car linha)) (equal 0 (cadr numero-lista))) y)
     ((equal (reverter-lista numero-lista) car-linha-lista) y)
     (T (posicao-simetrico-linha n (cdr linha) (1+ y)))
     )
    ))

(defun posicao-simetrico(n tabuleiro &optional (x 0))
  (let ((simetrico (posicao-simetrico-linha n (car tabuleiro))))
    (cond
     ((NULL tabuleiro) NIL)
     ((or (equal (car (numero-para-lista n)) (cadr (numero-para-lista n))) (equal n 0)) (obter-valor (maior-duplo tabuleiro) tabuleiro))
     ((NOT (NULL simetrico)) (cons x (cons simetrico nil)))
     (T (posicao-simetrico n (cdr tabuleiro) (1+ x)))
     )
    )
  )

(defun converter-string-coordenada()
  (let* ((coordenada (format nil "~S" (read))))
    (list (1- (parse-integer (subseq coordenada 1 (length coordenada)))) (- (char-code (char coordenada 0)) 65))
    )
  )

"exemplo: (remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4))"
"resultado: (1 2 2 4)"
(defun remover-se(pred lista)
  (cond ((null lista) NIL)
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

(defun fatorial(n)
  (cond
   ((<= n 1) n)
   (T (* n (fatorial (1- n))))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;; Operadores ;;;;;;;;;;;;;;;;;;;;;;;;
(defun operador-por-coordenada(coordenada tabuleiro jogador)
  (let ((pos-cavalo (posicao-jogador tabuleiro jogador)) (linha (first coordenada)) (coluna (second coordenada)))
    (cond
     ((NULL pos-cavalo) NIL)
     (T (cond
         ((and (= (+ (car pos-cavalo) 2) linha) (= (1- (cadr pos-cavalo)) coluna)) 'operador-1)
         ((and (= (+ (car pos-cavalo) 2) linha) (= (1+ (cadr pos-cavalo)) coluna)) 'operador-2)
         ((and (= (1+ (car pos-cavalo)) linha) (= (+ (cadr pos-cavalo) 2) coluna)) 'operador-3)
         ((and (= (1- (car pos-cavalo)) linha) (= (+ (cadr pos-cavalo) 2) coluna)) 'operador-4)
         ((and (= (- (car pos-cavalo) 2) linha) (= (1+ (cadr pos-cavalo)) coluna)) 'operador-5)
         ((and (= (- (car pos-cavalo) 2) linha) (= (1- (cadr pos-cavalo)) coluna)) 'operador-6)
         ((and (= (1- (car pos-cavalo)) linha) (= (- (cadr pos-cavalo) 2) coluna)) 'operador-7)
         ((and (= (1+ (car pos-cavalo)) linha) (= (- (cadr pos-cavalo) 2) coluna)) 'operador-8)
         (T NIL)
         ))
     )
    ))

(defun movimentos-possiveis(tabuleiro operadores jogador)
  (cond
   ((or (NULL (posicao-jogador tabuleiro jogador)) (NULL operadores)) nil)
   ((NULL (funcall (car operadores) tabuleiro jogador)) (movimentos-possiveis tabuleiro (cdr operadores) jogador))
   (T (cons (car operadores) (movimentos-possiveis tabuleiro (cdr operadores) jogador)))
   )
)

(defun operadores()
  (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8))

(defun operador-aux(inc-x inc-y tabuleiro jogador)
  (let* ((pos-cavalo (posicao-jogador tabuleiro jogador)) (nova-posicao (obter-posicao (+ (car pos-cavalo) inc-x) (+ (cadr pos-cavalo) inc-y) tabuleiro)))
    (cond
     ((OR (NULL nova-posicao) (equal jogador nova-posicao) (equal (trocar-jogador jogador) nova-posicao)) NIL)
     (T (let* ((tabuleiro1 (substituir (car pos-cavalo) (cadr pos-cavalo) tabuleiro 'NIL))
               (tabuleiro2 (substituir (+ (car pos-cavalo) inc-x) (+ (cadr pos-cavalo) inc-y) tabuleiro1 jogador))
               (simetrico (posicao-simetrico (celula (+ (car pos-cavalo) inc-x) (+ (cadr pos-cavalo) inc-y) tabuleiro) tabuleiro2)))
          (cond
           ((NULL simetrico) tabuleiro2)
           (T (substituir (car simetrico) (cadr simetrico) tabuleiro2 'NIL))
           )
          ))
     )))

(defun operador-1(tabuleiro jogador) (operador-aux 2 -1 tabuleiro jogador))

(defun operador-2(tabuleiro jogador) (operador-aux 2 1 tabuleiro jogador))

(defun operador-3(tabuleiro jogador) (operador-aux 1 2 tabuleiro jogador))

(defun operador-4(tabuleiro jogador) (operador-aux -1 2 tabuleiro jogador))

(defun operador-5(tabuleiro jogador) (operador-aux -2 1 tabuleiro jogador))

(defun operador-6(tabuleiro jogador) (operador-aux -2 -1 tabuleiro jogador))

(defun operador-7(tabuleiro jogador) (operador-aux -1 -2 tabuleiro jogador))

(defun operador-8(tabuleiro jogador) (operador-aux 1 -2 tabuleiro jogador))

;;;;;;;;;;;;;;;;;;;;;;;;;;; HEURISTICA ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun append-tabuleiro(tabuleiro)
  (cond
   ((NULL tabuleiro) NIL)
   (T (append (car tabuleiro) (append-tabuleiro (cdr tabuleiro))))
   )
)

(defun media-pontos(tabuleiro &optional primeira-iteracao)
  (cond
   ((NULL tabuleiro) 0.0)
   ((NULL primeira-iteracao) (let ((linha (remover-se #'(lambda (x) (or (NULL x) (equal 'T x))) (append-tabuleiro tabuleiro))))
                               (cond
                                ((or (equal 0 (length linha)) (NULL linha)) 1)
                                (T (/ (+ (car linha) (media-pontos (cdr linha) 1)) (length linha)))
                                )
                               ))
   (T (+ (car tabuleiro) (media-pontos (cdr tabuleiro) 1)))
   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;; SUCESSORES ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun valor-posicao(no sucessor jogador)
  (let ((pos-cavalo (posicao-jogador sucessor jogador)))
    (cond
     ((NULL pos-cavalo) nil)
     ((= jogador -1) (obter-posicao (car pos-cavalo) (cadr pos-cavalo) (no-estado no)))
     (T (* -1 (obter-posicao (car pos-cavalo) (cadr pos-cavalo) (no-estado no))))
     )
    ))

(defun novo-sucessor(no op jogador)
  (let* ((sucessor (construir-no (funcall op (no-estado no) jogador) no)) (valor (valor-posicao no (no-estado sucessor) jogador)))
    (cond 
     ((or (NULL no) (NULL sucessor) (NULL valor)) NIL)
     (T (construir-no (no-estado sucessor) no valor))
     )
    )
  )

(defun sucessores(no &optional (jogador -1) (operadores (operadores)))
  (cond
   ((or (NULL no) (NULL operadores)) NIL)
   ((null (posicao-jogador (no-estado no) jogador)) (list (colocar-jogador no jogador)))
   (T (remover-se #'(lambda (x) (NULL x)) (cons (novo-sucessor no (car operadores) jogador) (sucessores no jogador (cdr operadores)))))
   )
  )

(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)
