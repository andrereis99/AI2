;;;;Carrega os outros ficheiros de código, escreve e lê de ficheiros e trata da interação com o utilizador
;;;; Disciplina de IA - 2019 / 2020
;;;; 2º projeto
;;;; Autores: André Reis 170221035 e Bruno Alves 170221041

(defpackage :p170221035-170221041)
(defvar *jogada*)
(defvar *jogador1*)
(defvar *jogador2*)

(compile-file (concatenate 'string "C:/lisp/" "jogo.lisp"))
(compile-file (concatenate 'string "C:/lisp/" "algoritmo.lisp"))
(load (concatenate 'string "C:/lisp/" "jogo.ofasl"))
(load (concatenate 'string "C:/lisp/" "algoritmo.ofasl"))

;;; Menus
(defun menu-inicial()
  "Apresenta a mensagem para escolher o algoritmo"
  (progn
    (format t "    ~%---------------------------------------------------------")
    (format t "   ~%|           JOGO DO CAVALO - Escolha o algoritmo          |")
    (format t "   ~%|                                                         |")
    (format t "   ~%|               1 - Humano vs Computador                  |")
    (format t "   ~%|               2 - Computador vs Computador              |")
    (format t "   ~%|               s - Sair                                  |")
    (format t "   ~%|                                                         |")
    (format t "   ~% ---------------------------------------------------------~%~%> ")
    )
  )

(defun start()
  "Executa um algoritmo, dependendo da opcao escolhida"
  (progn 
    (menu-inicial)
    (setq *jogador1* 0)
    (setq *jogador2* 0)
    (let ((opt (read)))
      (cond 
       ((eq opt 's) (format t "Até à próxima!"))
       ((or (not (numberp opt)) (> opt 2) (< opt 1)) 
        (progn (format t "Insira uma opção válida") (start)))
       (T (ecase opt
            (1 (humano-computador))
            (2 (computador-computador))
            ))
       ))
    ))

(defun posicionar-cavalo(tabuleiro jogador)
  (cond
   ((null (posicao-jogador tabuleiro jogador)) 
    (progn 
      (if (= jogador -1)
          (format t "~%Escolha uma posição da primeira linha para colocar o seu cavalo: ")
        (format t "~%Escolha uma posição da última linha para colocar o seu cavalo: "))
      (let* ((coordenada (converter-string-coordenada))
             (novo-tabuleiro (escolher-posicao-jogador coordenada tabuleiro jogador)))
        (cond
         ((null novo-tabuleiro) (progn (format t "~%Opção inválida!") (posicionar-cavalo tabuleiro jogador)))
         (T (setq *jogada* novo-tabuleiro))
         )
        )))
   ))

;;Função para campeonato
;<estado> = (<tabuleiro> <jogador(-1|-2)> <pontos-jogador-1> <pontos-jogador-2>)
;devolve coordenadas da jogada e estado resultante
(defun jogar(estado tempo)
  (let* ((tabuleiro (first estado)) (jogador (second estado)) 
         (pontos-jogador-1 (third estado)) (pontos-jogador-2 (fourth estado))
         (caminho (negamax (construir-no tabuleiro nil) tempo 'sucessores jogador)) 
         (novo-no (obter-jogada (car caminho)))
         (novo-estado (no-estado novo-no)))
    (cond 
     ((null novo-estado) nil)
     (T (let ((pontos (if(= jogador -1)
                          (+ pontos-jogador-1 (abs (no-f novo-no)))
                        (+ pontos-jogador-2 (abs (no-f novo-no))))))
          (cond
           ((= jogador -1) 
            (cons (posicao-jogador novo-estado jogador) (cons (cons novo-estado (cons jogador (cons pontos (cons pontos-jogador-2 nil)))) nil)))
           ((= jogador -2)
            (cons (posicao-jogador novo-estado jogador) (cons (cons novo-estado (cons jogador (cons pontos-jogador-1 (cons pontos nil)))) nil)))
           ))))
    ))

(defun jogada-computador(estado tempo jogador)
  (let* ((caminho (negamax (construir-no estado nil) tempo 'sucessores jogador)) 
         (novo-no (obter-jogada (car caminho)))
         (novo-estado (no-estado novo-no))
         (stats (second caminho)))
    (cond 
     ((null novo-estado)nil)
     (T (let ((pontos (abs (no-f novo-no))))
          (progn
            (setq *jogada* novo-estado)
            (escrever-tabuleiro *jogada*)
            (cond
             ((= jogador -1) (progn 
                               (setq *jogador1* (+ pontos *jogador1*)) 
                               (format t "~%Pontos feitos na jogada pelo Jogador 1 (Computador): ~d" pontos)))
             ((= jogador -2) (progn 
                               (setq *jogador2* (+ pontos *jogador2*)) 
                               (format t "~%Pontos feitos na jogada pelo Jogador 2 (Computador): ~d" pontos)))
             )
            (format t "~%Número de Nós Analisados: ~d ~%Número de Cortes: ~d ~%Tempo de Execução: ~d~% " (first stats) (second stats) (third stats))
            (escrever-pontos 'computador jogador pontos stats)
            T
            ))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HUMANO-COMPUTADOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun humano-computador()
  (let* ((primeiro-jogador (ler-primeiro-jogador)) (tempo (ler-tempo-limite)) 
         (formato (ler-formato-input)) (tabuleiro (tabuleiro-aleatorio))
         (jogador (if(equal primeiro-jogador 'humano) -1 -2)))
    (progn
      (cond
       ((equal primeiro-jogador 'humano) 
        (progn
          (escrever-tabuleiro tabuleiro)
          (posicionar-cavalo tabuleiro jogador)
          (escrever-inicio-ficheiro)
          (jogada-computador *jogada* tempo (trocar-jogador jogador))
          ))
       (T (progn
            (escrever-inicio-ficheiro)
            (jogada-computador tabuleiro tempo (trocar-jogador jogador))
            (posicionar-cavalo *jogada* jogador)
            ))
       )
      (cond
       ((equal primeiro-jogador 'humano)
        (humano-computadorAux tempo formato -1))
       (T (humano-computadorAux tempo formato -2))
       )))
  )

(defun humano-computadorAux(tempo formato primeiro-jogador)
  (let* ((jogada-pc-primeiro (if (= primeiro-jogador -2) (jogada-computador *jogada* tempo (trocar-jogador primeiro-jogador))))
         (operador (tratamento-jogada formato primeiro-jogador))
         (jogada-humano (jogada-humano primeiro-jogador operador))
         (jogada-pc-segundo (if (= primeiro-jogador -1) (jogada-computador *jogada* tempo (trocar-jogador primeiro-jogador)))))
    (cond 
     ((and (null jogada-humano) (null jogada-pc-primeiro) (null jogada-pc-segundo)) (acabar-jogo 'humano-pc primeiro-jogador))
     (T (humano-computadorAux tempo formato primeiro-jogador))))
  )

(defun jogada-humano(jogador operador)
  (cond
   ((or (NULL *jogada*)(NULL jogador)(NULL operador)) NIL)
   ((null (posicao-jogador *jogada* jogador)) (no-estado (colocar-jogador (construir-no *jogada* nil) jogador)))
   (T (let* ((sucessor (novo-sucessor (construir-no *jogada* nil) operador jogador)) (pontos (abs (no-f sucessor))))
        (progn
          (setq *jogada* (no-estado sucessor))
          (escrever-tabuleiro *jogada*)
          (cond
           ((= jogador -1) (progn 
                             (setq *jogador1* (+ pontos *jogador1*)) 
                             (format t "~%Pontos feitos na jogada pelo Jogador 1 (Humano): ~d~%" pontos)))
           ((= jogador -2) (progn 
                             (setq *jogador2* (+ pontos *jogador2*)) 
                             (format t "~%Pontos feitos na jogada pelo Jogador 2 (Humano): ~d~%" pontos)))
           )
          (escrever-pontos 'humano jogador pontos)
          )))
   ))

(defun tratamento-jogada(formato primeiro-jogador)
  (if (equal formato 'coordenada) 
      (mover-para-coordenadas primeiro-jogador) 
    (ler-movimentos-possiveis *jogada* primeiro-jogador))
)

(defun movimentos-possiveis-menu(tabuleiro jogador)
  "Escreve no ecrã uma lista de operadores possiveis de executar tendo em conta a posição atual do cavalo"
  (let ((pos-cavalo (posicao-jogador tabuleiro jogador)))
    (cond
     ((NULL pos-cavalo) (format t "O cavalo ainda não está posicionado no tabuleiro."))
     (T (progn
          (format t "    ~% ------------------------")
          (format t "    ~%|  Movimentos Possiveis  |")
          (if (obter-posicao (+ (car pos-cavalo) 2) (1- (cadr pos-cavalo)) tabuleiro) (format t "~%|     1- Operador-1      |"))
          (if (obter-posicao (+ (car pos-cavalo) 2) (1+ (cadr pos-cavalo)) tabuleiro) (format t "~%|     2- Operador-2      |"))
          (if (obter-posicao (1+ (car pos-cavalo)) (+ (cadr pos-cavalo) 2) tabuleiro) (format t "~%|     3- Operador-3      |"))
          (if (obter-posicao (1- (car pos-cavalo)) (+ (cadr pos-cavalo) 2) tabuleiro) (format t "~%|     4- Operador-4      |"))
          (if (obter-posicao (- (car pos-cavalo) 2) (1+ (cadr pos-cavalo)) tabuleiro) (format t "~%|     5- Operador-5      |"))
          (if (obter-posicao (- (car pos-cavalo) 2) (1- (cadr pos-cavalo)) tabuleiro) (format t "~%|     6- Operador-6      |"))
          (if (obter-posicao (1- (car pos-cavalo)) (- (cadr pos-cavalo) 2) tabuleiro) (format t "~%|     7- Operador-7      |"))
          (if (obter-posicao (1+ (car pos-cavalo)) (- (cadr pos-cavalo) 2) tabuleiro) (format t "~%|     8- Operador-8      |"))
          (format t "   ~% ------------------------~%~%> ")
          ))
     )
    ))

(defun ler-movimentos-possiveis(tabuleiro jogador)
  (cond
   ((null (movimentos-possiveis tabuleiro (operadores) jogador)) nil)
   (T 
    (progn
      (movimentos-possiveis-menu tabuleiro jogador)
      (format t "~%Insira a opção relativa ao operador que pretende executar: ")
      (let* ((opt (read)) (possibilidades (movimentos-possiveis tabuleiro (operadores) jogador)))
        (cond
         ((and (= opt 1) (contem possibilidades 'operador-1)) 'operador-1)
         ((and (= opt 2) (contem possibilidades 'operador-2)) 'operador-2)
         ((and (= opt 3) (contem possibilidades 'operador-3)) 'operador-3)
         ((and (= opt 4) (contem possibilidades 'operador-4)) 'operador-4)
         ((and (= opt 5) (contem possibilidades 'operador-5)) 'operador-5)
         ((and (= opt 6) (contem possibilidades 'operador-6)) 'operador-6)
         ((and (= opt 7) (contem possibilidades 'operador-7)) 'operador-7)
         ((and (= opt 8) (contem possibilidades 'operador-8)) 'operador-8)
         (T (progn
              (format t "~%O operador introduzido é inválido!")
              (ler-movimentos-possiveis tabuleiro jogador)))
         ))))
   ))

(defun mover-para-coordenadas(jogador)
  (progn
    (format t "~%Insira as coordenadas para onde pretende mover o cavalo (ex: A01): ")
    (let ((jogada (operador-por-coordenada (converter-string-coordenada) *jogada* -1)))
      (cond
       ((null jogador) nil)
       ((null jogada) (progn 
                        (format t "~%Coordenadas Inválidas! É impossivel movereste para esta posição!")
                        (mover-para-coordenadas jogador)))
       (T jogada)
       )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMPUTADOR-COMPUTADOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun computador-computador()
  (let ((tempo (ler-tempo-limite)) (tabuleiro (tabuleiro-aleatorio)))
    (progn
      (escrever-inicio-ficheiro)
      (jogada-computador tabuleiro tempo -1)
      (jogada-computador *jogada* tempo -2)
      (computador-computadorAux tempo)
      )
    ))

(defun computador-computadorAux(tempo-limite)
  (let((jogada1 (jogada-computador *jogada* tempo-limite -1)) (jogada2 (jogada-computador *jogada* tempo-limite -2)))
    (cond
     ((and (null jogada1) (null jogada2)) (acabar-jogo 'pc-pc))
     (T (computador-computadorAux tempo-limite))
     )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MENUS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun menu-tempo-limite()
  "Apresenta a mensagem para definir um tempo limite de processamento"
  (progn
    (format t "    ~%----------------------------------------------------------")
    (format t "   ~%|           JOGO DO CAVALO - Insira o tempo limite         |")
    (format t "   ~%|                 de processamento da máquina              |")
    (format t "   ~%|           (O tempo limite de estar compreendido          |")
    (format t "   ~%|                     entre 1 e 5 segundos)                |")
    (format t "   ~%|                                                          |")
    (format t "   ~%|                         s - Sair                         |")
    (format t "   ~%|                                                          |")
    (format t "   ~% ----------------------------------------------------------~%~%> ")
    )
)

(defun ler-tempo-limite()
  (progn 
    (menu-tempo-limite)
    (let ((tempo (read)))
      (cond 
       ((eq tempo 's) (format t "Até à próxima!"))
       ((or (not (numberp tempo)) (> tempo 5) (< tempo 1)) 
        (progn (format t "O tempo limite deve estar compreendido entre 1 e 5 segundos.")
          (format t "~%Insira o tempo limite novamente.")(ler-tempo-limite)))
       (T (* tempo 1000))
       ))
    )
  )

(defun menu-primeiro-jogador()
  "Apresenta a mensagem para escolher o algoritmo"
  (progn
    (format t "    ~%---------------------------------------------------------")
    (format t "   ~%|      JOGO DO CAVALO - Escolha quem começa a jogar       |")
    (format t "   ~%|                                                         |")
    (format t "   ~%|                      1 - Eu                             |")
    (format t "   ~%|                      2 - Computador                     |")
    (format t "   ~%|                      s - Sair                           |")
    (format t "   ~%|                                                         |")
    (format t "   ~% ---------------------------------------------------------~%~%> ")
    )
  )

(defun ler-primeiro-jogador()
  (progn 
    (menu-primeiro-jogador)
    (let ((opt (read)))
      (cond 
       ((eq opt 's) (format t "Até à próxima!"))
       ((or (not (numberp opt)) (> opt 2) (< opt 1)) 
        (progn (format t "Insira uma opção válida") (ler-primeiro-jogador)))
       (T (ecase opt
            (1 'humano)
            (2 'computador)
            ))
       ))
    )
)

(defun menu-formato-input()
  "Apresenta a mensagem para escolher o algoritmo"
  (progn
    (format t "    ~%---------------------------------------------------------")
    (format t "   ~%|           JOGO DO CAVALO - Escolha o formato de         |")
    (format t "   ~%|               input com que pretender jogar             |")
    (format t "   ~%|                                                         |")
    (format t "   ~%|              1 - Letra/Número (ex: A10)                 |")
    (format t "   ~%|              2 - Operador (ex:Operador-4 (2 1))         |")
    (format t "   ~%|              s - Sair                                   |")
    (format t "   ~%|                                                         |")
    (format t "   ~% ---------------------------------------------------------~%~%> ")
    )
  )

(defun ler-formato-input()
  (progn 
    (menu-formato-input)
    (let ((opt (read)))
      (cond 
       ((eq opt 's) (format t "Até à próxima!"))
       ((or (not (numberp opt)) (> opt 2) (< opt 1)) 
        (progn (format t "Insira uma opção válida") (ler-formato-input)))
       (T (ecase opt
            (1 'coordenada)
            (2 'operador)
            ))
       ))
    ))

(defun escrever-tabuleiro(tabuleiro &optional (stream t) (index 65))
  "Mostra cabeçalho para tabuleiro"
  (cond
   ((< (- index 65) (length tabuleiro)) 
    (progn
      (if(= index 65) (format stream "~%  "))
      (format stream " |~C" (int-char index))
      (escrever-tabuleiro tabuleiro stream (1+ index))
      ))
   (T (escrever-tabuleiroAux tabuleiro stream))
   ))

(defun escrever-tabuleiroAux(tabuleiro &optional (stream t) (index 1))
  "Mostra um tabuleiro bem formatado"
  (cond
   ((= (length tabuleiro) 1) 
    (progn
      (format stream "~%~d |" index)
      (mapcar #'(lambda(a) (format stream "~a~t" a)) (car tabuleiro))
      (format stream "~t|")
      (format stream "~%")
      ))
   (T (progn
        (format stream "~% ~d |" index)
        (mapcar #'(lambda(a) (format stream "~a~t" a)) (car tabuleiro))
        (format stream "|")
        (escrever-tabuleiroAux (cdr tabuleiro) stream (1+ index))
        ))
   ))

(defun acabar-jogo(tipo-jogo &optional primeiro-jogador)
  (progn
    (format t "~% ----------------------------------")
    (format t "~%            FIM DO JOGO            ")
    (format t "~% ----------------------------------")
    (format t "~% Pontos jogador 1: ~d" *jogador1*)
    (format t "~% Pontos jogador 2: ~d" *jogador2*)

    (escrever-ficheiro tipo-jogo primeiro-jogador)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ESCRITA NOS FICHEIROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun caminho-ficheiro-resultados()
"Devolve o path para o ficheiro (C:\lisp\resultados.dat)"
    (make-pathname :host "c" :directory '(:absolute "lisp") :name "resultados" :type "dat"))

(defun escrever-inicio-ficheiro()
  (progn
    (with-open-file (file (caminho-ficheiro-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
      (progn
        (format file "~%-------------------------------------------------------------")
        (format file "~%                      INICIO DA PARTIDA                      ")
        (format file "~%-------------------------------------------------------------")
        ))
    )
  )

(defun escrever-pontos (tipo-jogador jogador pontos &optional stats)
  (progn
    (with-open-file (file (caminho-ficheiro-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
      (progn
        (escrever-tabuleiro *jogada* file)
        (format file "~%Pontos feitos na jogada pelo Jogador ~d (~s): ~d" (if(= jogador -1) 1 2) tipo-jogador pontos)
        (if(equal tipo-jogador 'computador)
            (format file "~%Número de Nós Analisados: ~d ~%Número de Cortes: ~d ~%Tempo de Execução: ~d~% " (first stats) (second stats) (third stats)))
        ))
    )
  )

(defun escrever-ficheiro (tipo-jogo primeiro-jogador)
  "Escreve, no ficheiro de resultados, as pontuações dos jogadores"
  (with-open-file (file (caminho-ficheiro-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
    (progn
      (format file "~% -------------------------------------")
      (if(equal tipo-jogo 'pc-pc)
          (format file "~%       Computador VS Computador       ")
        (format file "~%         Humano VS Computador         "))
      (format file "~% -------------------------------------")
      (if (equal tipo-jogo 'humano-pc) 
          (if (equal primeiro-jogador -1)
              (format file "~% Primeiro Jogador: ~S" 'Humano)
            (format file "~% Humano VS Computador: ~S" 'Computador)))
      (format file "~% Pontos jogador 1: ~d" *jogador1*)
      (format file "~% Pontos jogador 2: ~d" *jogador2*)
      ))
  )
