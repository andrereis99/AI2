;;Código relacionado com o algoritmo Negamax
;; Autores: André Reis e Bruno Alves
;; Ano letivo 19/20
(defpackage :algoritmo)

;;;;;;;;;;;;;;;;;;;;;;;;;;; NOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun construir-no (tabuleiro pai &optional (f 0))
  "Constroi a estrutura de dados para o no do problema"
  (list tabuleiro pai f)
  )

(defun criar-no-solucao (no nos-analisados numero-cortes tempo-inicial)
"Constrói o nó solução"
  (list no (list nos-analisados numero-cortes (- (get-internal-real-time) tempo-inicial)))
)

(defun no-estado (no)
  "Retorna o estado 'tabuleiro' de um no"
  (car no))

(defun no-pai (no)
  "Devolve o no pai de um no"
  (cadr no))

(defun no-f(no)
  "Devolve o valor g de um no"
  (caddr no))

(defun no-existep (no lista-no)
  "Verifica se um no existe numa determinada lista"
  (eval (cons 'or (mapcar #'(lambda(noaux) (if (equal noaux no) T NIL)) lista-no))))

(defun profundidade-no(no)
"Calcula a profundidade de um no"
  (cond
   ((null (no-pai no)) 0)
   (T (1+ (profundidade-no (no-pai no))))
   )
)

(defun melhor-f(a b)
  (let ((value-a (no-f a))
        (value-b (no-f b)))
    (if (> value-a value-b) a b)
    )
  )

(defun obter-jogada(no &optional no-filho)
  (cond
   ((null (no-pai no)) no-filho)
   (T (obter-jogada (no-pai no) no))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Algoritmo ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negamax (no tempo-limite f-sucessores &optional (jogador -1) (prof 50) 
                   (alfa most-negative-fixnum) (beta most-positive-fixnum) 
                   (tempo-inicial (get-internal-real-time)) (nos-gerados 0) (cortes 0))
  "Executa a função negamax para um nó"
  (let ((sucessores (ordenar-sucessores-negamax (funcall f-sucessores no jogador) jogador)))
    (cond
     ((or (= prof 0) (null sucessores) (>= (- (get-internal-real-time) tempo-inicial) tempo-limite))
      (criar-no-solucao no nos-gerados cortes tempo-inicial))
     (T (negamax-suc no sucessores tempo-limite f-sucessores jogador prof alfa beta tempo-inicial nos-gerados cortes))
     )
    ))

(defun negamax-suc(no-pai sucessores tempo-limite f-sucessores jogador prof
                          alfa beta tempo-inicial nos-gerados cortes)
  "NegamaxAuxiliar - Executa a função negamax para os sucessores de um nó"
  (cond
   ((equal (length sucessores) 1) 
    (negamax (car sucessores) tempo-limite f-sucessores (trocar-jogador jogador) 
             (1- prof) (- alfa) (- beta) tempo-inicial (1+ nos-gerados) cortes))
   (T
    (let*  ((car-solucao (negamax (car sucessores) tempo-limite f-sucessores (trocar-jogador jogador) 
                                  (1- prof) (- alfa) (- beta) tempo-inicial (1+ nos-gerados) cortes))
            (car-no (car car-solucao))
            (melhor-valor (melhor-f car-no no-pai))
            (novo-alfa (max alfa (no-f melhor-valor)))
            )
      (if (>= novo-alfa beta);corte
          (criar-no-solucao no-pai (nth 0 (cadr car-solucao)) (1+ (nth 1 (cadr car-solucao))) tempo-inicial)
        (negamax-suc no-pai (cdr sucessores) tempo-limite f-sucessores jogador prof novo-alfa beta 
                     tempo-inicial (nth 0 (cadr car-solucao)) (nth 1 (cadr car-solucao)))
        )
      )
    )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Funções Auxiliares ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ordenar-sucessores-negamax (lista-nos jogador)
  "Executa o algoritmo quicksort na lista de nós"
  (cond 
   ((null lista-nos) nil)
   ((= jogador -1) 
    (append
     (ordenar-sucessores-negamax (qsort< (no-f (car lista-nos)) (cdr lista-nos)) jogador)
     (cons (car lista-nos) nil)
     (ordenar-sucessores-negamax (qsort>= (no-f (car lista-nos)) (cdr lista-nos)) jogador)
     ))
   (T 
    (append
     (ordenar-sucessores-negamax (qsort>= (no-f (car lista-nos)) (cdr lista-nos)) jogador)
     (cons (car lista-nos) nil)
     (ordenar-sucessores-negamax (qsort< (no-f (car lista-nos)) (cdr lista-nos)) jogador)
     ))
   )
  )

(defun qsort< (N lista-nos)
  "Auxiliar quicksort para valores menores"
  (cond
   ((or (null N) (null lista-nos)) nil)
   ((< N (no-f (car lista-nos))) (qsort< N (cdr lista-nos)))
   (T (cons (car lista-nos) (qsort< N (cdr lista-nos))))
   )
  )

(defun qsort>= (N lista-nos)
  "Auxiliar quicksort para valores maiores"
  (cond
   ((or (null N) (null lista-nos)) nil)
   ((>= N (no-f (car lista-nos))) (qsort>= N (cdr lista-nos)))
   (T (cons (car lista-nos) (qsort>= N (cdr lista-nos))))
   )
  )

(defun trocar-jogador(jogador)
  (if(equal jogador -1) -2 -1)
)
