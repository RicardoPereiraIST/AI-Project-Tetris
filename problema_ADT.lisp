
;;-------------------------------Tipo ACCAO----------------------------------------
(defun cria-accao (int array) 
	(cons int array)
)

(defun accao-coluna (act)
	(first act)
)

(defun accao-peca (act)	
	(cdr act)
)

;-----------------------------------------------Tipo TABULEIRO-----------------------------------
(defun cria-tabuleiro()
	(make-array (list 18 10))
)

(defun tabuleiro-preenchido-p (tab int1 int2)
	(if 
	(equal(aref tab int1 int2) t)
	t
	nil)
)

(defun tabuleiro-altura-coluna (tab int)  ;QUALQUER COISA MAL
	(loop for i from 17 downto 0 do
		(if (tabuleiro-preenchido-p tab i int)
			(return-from tabuleiro-altura-coluna (1+ i))
		)	
	)
	0
)

(defun tabuleiro-linha-completa (tab int)
	(loop for j from 0 to 9 do
		(if
		(equal(aref tab int j) nil)
		(return-from tabuleiro-linha-completa nil)
		)
	)
)	

(defun tabuleiro-linha-completa-p (tab int)
	(loop for j from 0 to 9 do
		(if
		(equal(aref tab int j) nil)
		(return-from tabuleiro-linha-completa-p nil)   ;PROBLEMA AQUI
		)
	)
	t
)

(defun tabuleiro-preenche!(tab1 int1 int2)
	(if(and(<= int1 17)(>= int1 0)(<= int2 9)(>= int2 0))
		(setf (aref tab1 int1 int2) t))
)	

(defun tabuleiro-remove-linha!(tab num_linha)
	(if (equal num_linha 0)
		;true
		(progn
			(copia-linha-abaixo tab num_linha)
			;(preenche-linha tab num_linha 9 nil)
		)
		;false
		(progn
			(copia-linha-abaixo tab num_linha)
			(tabuleiro-remove-linha! tab (+ num_linha 1))	
		)
	)

)


(defun tabuleiro-topo-preenchido-p(tab)
	;mudar para recursivo
	
	(loop for num_coluna from 0 to 9 do
		(if (equal (aref tab 17 num_coluna) t)
			(return-from tabuleiro-topo-preenchido-p t)
		)
	)
	(return-from tabuleiro-topo-preenchido-p nil)
)

(defun tabuleiros-iguais-p(tab1 tab2)
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(if(not (equal (aref tab1 num_linha num_coluna)
					(aref tab2 num_linha num_coluna)))
				(return-from tabuleiros-iguais-p nil)
			)
		)
	)
	t
)
(defun tabuleiro->array (tab)
	(let ((array (make-array (list 18 10))))


	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref array num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
	array
	)
)
	
(defun array->tabuleiro(array)
	(let ((tabuleiro (cria-tabuleiro)))

	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref array num_linha num_coluna))
		)
	)
	tabuleiro)
)

(defun copia-tabuleiro(tab)
	(let ((tabuleiro (cria-tabuleiro)))
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
	tabuleiro)
)

; funcoes auxiliares 

(defun preenche-linha(tab num_linha num_coluna value)
	(if (equal num_coluna 0)
		(setf (aref tab num_linha num_coluna) value)
		(progn
			(setf (aref tab num_linha num_coluna) value)
			(preenche-linha tab num_linha (- num_coluna 1) value)
		)
		
	)
)

(defun copia-linha-abaixo(tab num_linha)

)
;-------------------------------------------------Tipo ESTADO
(defstruct estado
	pontos ;;optional
	pecas-por-colocar
	pecas-colocadas ;;optional
	tabuleiro)

(defun cria-estado (points pieces_to_place pieces_placed board)
	(make-estado 
		:pontos points
		:pecas-por-colocar (cdr(cons nil pieces_to_place))
		:pecas-colocadas (cdr(cons nil pieces_placed))
		:tabuleiro board)
)

(defun copia-estado(state)
	(let ((novo-tabuleiro nil)
		(novo-pecas-colocadas nil)
		(novo-pecas-por-colocar nil))

	(setf novo-pecas-por-colocar (copy-list (estado-pecas-por-colocar state) ))
	(setf novo-pecas-colocadas (copy-list (estado-pecas-colocadas state)))
	(setf novo-tabuleiro (copia-tabuleiro (estado-tabuleiro state)))

	(make-estado 
		:pontos (estado-pontos state)    	
		:pecas-por-colocar novo-pecas-por-colocar
		:pecas-colocadas novo-pecas-colocadas
		:tabuleiro novo-tabuleiro) 
	)
)


(defun estados-iguais-p(state1 state2)
	(if (and (equal (estado-pontos state1)(estado-pontos state2))
		(equal (estado-pecas-por-colocar state1)(estado-pecas-por-colocar state2))
		(equal(estado-pecas-colocadas state1)(estado-pecas-colocadas state2))

		(tabuleiros-iguais-p (estado-tabuleiro state1)(estado-tabuleiro state2)))
	t
	nil)
)


(defun estado-final-p (state)
	(if (or (equal(estado-pecas-por-colocar state) nil) 
		(tabuleiro-topo-preenchido-p (estado-tabuleiro state)))
	t
	nil)	
)

;----------------------------------------Tipo PROBLEMA---------------------------------------------------
(defstruct problema 
	estado-inicial
	solucao
	accoes
	resultado
	custo-caminho)

(defun solucao (state)
	(if (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro state)))
		(equal(estado-pecas-por-colocar state) nil))
		t
		nil)
)

(defun accoes(state)
	(case (car(estado-pecas-por-colocar state)) 
		(I (accao-i)) ;ATENCAO UPPER CASE
		(L (accao-l))
		(J (accao-j))
		(O (accao-o))
		(S (accao-s))
		(Z (accao-z))
		(t (accao-t))
	)
) 	


(defun accao-i()
	(append (accao-i0) (accao-i1))
) 

(defun accao-l()
	(append (accao-l0) (accao-l1) (accao-l2) (accao-l3))
)

(defun accao-j()
	(append (accao-j0) (accao-j1) (accao-j2) (accao-j3))
)

(defun accao-o()
	(accao-o0)
)

(defun accao-s()
	(append (accao-s0) (accao-s1))
)

(defun accao-z()
	(append (accao-z0) (accao-z1))
)

(defun accao-t()
	(append (accao-t0) (accao-t1) (accao-t2) (accao-t3))
)

(defun accao-i0()
	(let((lista (list)))
		(loop for k from 0 to 9 do
			(push (cria-accao k peca-i0) lista)
		)
	(nreverse lista)
	)
)


(defun accao-i1()
	(let ((lista (list)))
		(loop for k from 0 to 6 do
			(push (cria-accao k peca-i1) lista)
		)
	(nreverse lista)
	)
)

(defun accao-l0 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-l0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-l1 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-l1) lista)
		)	
	(nreverse lista)
	)
)

(defun accao-l2 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-l2) lista)	
		)
	(nreverse lista)
	)
)

(defun accao-l3 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-l3) lista)
		)
	(nreverse lista)
	)
) 

(defun accao-j0()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-j0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-j1()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-j1) lista)

		)
	(nreverse lista)
	)
)

(defun accao-j2()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-j2) lista)
		)
	(nreverse lista)
	)
)

(defun accao-j3()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-j3) lista)
		)
	(nreverse lista)
	)
)


(defun accao-o0()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-o0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-s0()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-s0) lista)

		)
	(nreverse lista)
	)
)

(defun accao-s1()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-s1) lista)
		)
	(nreverse lista)
	)
)


(defun accao-z0()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-z0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-z1()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-z1) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t0 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push (cria-accao k peca-t0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t1 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push (cria-accao k peca-t1) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t2 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push (cria-accao k peca-t2) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t3 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push (cria-accao k peca-t3) lista)
		)
	(nreverse lista)
	)
)




(defun resultado(state action)
	(let ((state_var (copia-estado state))
		(pontos nil)
		(tab nil)
		(tabuleiro nil)
		(peca_a_colocar nil)

		(minimo_pivot nil)
		(altura_final_pivot nil))

		(setf pontos (estado-pontos state_var))
		(setf peca_a_colocar (car(estado-pecas-por-colocar state_var)))
		(setf tab (estado-tabuleiro state_var))

		(setf minimo_pivot (calcula-max-altura tab action))
		(setf altura_final_pivot (calcula-onde-desenha tab minimo_pivot action))
		(setf tabuleiro (desenha tab altura_final_pivot action))


		(pop(estado-pecas-por-colocar state_var))
		(push peca_a_colocar (estado-pecas-colocadas state_var))


		(if (tabuleiro-topo-preenchido-p tabuleiro)
			()
			(and (tabuleiro-remove-linha tabuleiro) (incf pontos (soma-pontos peca_a_colocar)))
		)
	(setf tab tabuleiro)	
	state_var
	)
)

(defun calcula-max-altura(tabu action)						;Calcula a altura da maior coluna onde a peca se podera apoiar 
	(let ((largura (second (array-dimensions (accao-peca action))))
		(inicio (accao-coluna action))
		(maximo (tabuleiro-altura-coluna tabu (accao-coluna action))))

		(loop for k from inicio to (+ inicio (1- largura)) do
			(if (< maximo (tabuleiro-altura-coluna tabu k))
				(setf maximo (tabuleiro-altura-coluna tabu k))
			)
		)
	maximo
	)
)
(defun calcula-onde-desenha(tabu min action)		;Calcula a posicao mais abaixo onde pode ser desenhada a peca
	(let ((k min)
		(nr-linhas (first(array-dimensions(accao-peca action)))))

		(loop while (and (> k 0) (posso-desenhar tabu action k)) do
			(decf k 1)
		)
		(incf k 1)
	)
)

(defun posso-desenhar (tabu action linha)			;Devolve T caso seja possivel desenhar a accao recebida no tabuleiro, nil cc
	(let ((coluna (accao-coluna action))
		(nr-linhas (first(array-dimensions(accao-peca action))))
		(nr-colunas (second(array-dimensions(accao-peca action))))
		(x linha))

	(when (and(< x 18) (> x 0)) 
		(loop for x from 0 to (- nr-linhas 1) do
			(loop for y from 0 to (- nr-colunas 1) do

				(if (aref (accao-peca action) x y)
					(if (tabuleiro-preenchido-p tabu (+ linha x) (+ coluna y))
						(return-from posso-desenhar nil)
					)
				)
			)
		)
	)
	)
	t
) 

(defun desenha (state altura action)			;Preenche as quadriculas do tabuleiro conforme a configuracao da peca
	(let ((coluna (accao-coluna action))
		(tabu (estado-tabuleiro state))
		(nr-linhas (first(array-dimensions(accao-peca action))))
		(nr-colunas (second(array-dimensions(accao-peca action))))
		(i altura)
		(x 0)
		(y 0))


		(when (< i 18) 
			(loop for x from 0 to (- nr-linhas 1) do
				(loop for y from 0 to (- nr-colunas 1) do
					(if (aref (accao-peca action) x y)
						(tabuleiro-preenche! tabu (+ i x)(+ coluna y))
					)
				)
			)
		)
	tabu
	)
)

(defun qualidade (state)
	(-(estado-pontos state))
)

(defun custo-oportunidade(state)
	(let ((lista_pecas (estado-pecas-colocadas state))
		(total 0))

		(loop for k from 1 to (list-length lista_pecas) do
			(cond 
			( (eq (car lista_pecas) 'I) (incf total 800) )
			( (eq (car lista_pecas) 'J) (incf total 500) )
			( (eq (car lista_pecas) 'L) (incf total 500) )
			( (eq (car lista_pecas) 'S) (incf total 300) )
			( (eq (car lista_pecas) 'Z) (incf total 300) )
			( (eq (car lista_pecas) 'O) (incf total 300) )
			( (eq (car lista_pecas) 'T) (incf total 300) )
			( (eq (car lista_pecas) 'nil) () )
			)
		)
	(- total (estado-pontos state))
	)
)

(defun soma-pontos (peca)
	(case peca 
		(I 800)
		(J 500)
		(L 500)
		(S 300)
		(Z 300)
		(O 300)
		(t 300)
	)
)

(load "utils.lisp")


;(load "problema_ADT.lisp")
;(setf l2(cria-accao 3 peca-l2))
;(setf t2(cria-accao 1 peca-t2))
;(setf *a*(cria-tabuleiro))
;(setf *est1*(cria-estado 0 '(l) nil *a*))
;(preenche-linha *a* 1 9 t)

(setf Tab (cria-tabuleiro))
(preenche-linha Tab 2 9 T)
(setf (aref Tab 0 0) T)
(setf (aref Tab 3 3) T)
