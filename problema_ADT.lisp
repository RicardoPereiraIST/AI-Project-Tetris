<<<<<<< HEAD
;;-------------------------------Tipo ACCAO----------------------------------------
(defstruct accao 
		nr_coluna
		configuracao)

(defun cria-accao (int lst) 
	(make-accao 
			:nr_coluna int
			:configuracao lst)
)

(defun accao-coluna (act)
	(accao-nr_coluna act))


(defun accao-peca (act)
	(accao-configuracao act))
=======
(load "utils.fas")
;;-------------------------------Tipo ACCAO----------------------------------------
(defun cria-accao (int array) 
	(cons int array)
)

(defun accao-coluna (act)
	(first act)
)

(defun accao-peca (act)	
	(cdr act))

>>>>>>> Miguel


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

(defun tabuleiro-altura-coluna (tab int)
	(loop for i from 17 downto 0 do
		(if 
<<<<<<< HEAD
		(equal(aref tab i int) t)
=======
		(tabuleiro-preenchido-p tab i int)
>>>>>>> Miguel
		(return-from tabuleiro-altura-coluna (+ i 1))
		)	
	)
	0
)

<<<<<<< HEAD
(defun tabuleiro-linha-completa (tab int)
	(loop for j from 0 to 9 do
		(if
		(equal(aref tab int j) nil)
		(return-from tabuleiro-linha-completa nil)
=======
(defun tabuleiro-linha-completa-p (tab int)
	(loop for j from 0 to 9 do
		(if
		(equal(aref tab int j) nil)
		(return-from tabuleiro-linha-completa-p nil)   ;PROBLEMA AQUI
>>>>>>> Miguel
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
		(preenche-linha tab num_linha 9 nil)
		;false
		(progn
			(copia-linha-acima tab num_linha)
			(tabuleiro-remove-linha! tab (- num_linha 1))	
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

<<<<<<< HEAD
(defun tabuleiro->array(tab)
	(setf array (make-array (list 18 10)))
=======
(defun tabuleiro->array (tab)
	(let ((array (make-array (list 18 10))))

>>>>>>> Miguel
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref array num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
<<<<<<< HEAD
	(return-from tabuleiro->array array)
)

(defun array->tabuleiro(array)
	(setf tabuleiro (cria-tabuleiro))
=======
	array
	)
)
	
(defun array->tabuleiro(array)
	(let ((tabuleiro (cria-tabuleiro)))
>>>>>>> Miguel
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref array num_linha num_coluna))
		)
	)
<<<<<<< HEAD
	(return-from array->tabuleiro tabuleiro)
)

(defun copia_tabuleiro(tab)
	(setf tabuleiro (cria-tabuleiro))
=======
	tabuleiro)
)

(defun copia-tabuleiro(tab)
	(let ((tabuleiro (cria-tabuleiro)))
>>>>>>> Miguel
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
<<<<<<< HEAD
	(return-from copia_tabuleiro tabuleiro)
=======
	tabuleiro)
>>>>>>> Miguel
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

(defun copia-linha-acima(tab num_linha)
	(if (equal (- num_linha 1) 0)
		(return-from copia-linha-acima nil)
		(progn
			(loop for num_coluna from 0 to 9 do
				(setf (aref tab num_linha num_coluna) 
					(aref tab (- num_linha 1) num_coluna)
				)
			)
			(copia-linha-acima tab (- num_linha 1))
		)
	)
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
	(make-estado 
<<<<<<< HEAD
		:pontos (estado-pontos state)  
=======
		:pontos (estado-pontos state)    ;PROBLEMA, se fizermos (eq(estado-pecas-colocadas estado1) 
										;				(estado-pecas-colocadas estado2))), devolve T quando nao devia	
>>>>>>> Miguel
		:pecas-por-colocar (estado-pecas-por-colocar state) 
		:pecas-colocadas (estado-pecas-colocadas state) 
		:tabuleiro (estado-tabuleiro state)) 
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
;;lambda para lista ser "global" para o accao i0 e i1??
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
;			(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 3))))
;				(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 2)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 3))))
;					(push (cria-accao i '(T T T T)) lista)
;					()
;				)	
		)
	(nreverse lista)
	)
)

(defun accao-l0 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do

;			(setf linha (max(tabuleiro-altura-coluna (estado-tabuleiro state) i)
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))))
;				(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 2) i)))
					(push(cria-accao k peca-l0) lista)
;					()
;				)
		)
	(nreverse lista)
	)
)

(defun accao-l1 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			
;			(setf linha_do_par (max(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))
;			
;			(setf linha_pivot (tabuleiro-altura-coluna (estado-tabuleiro state) i))
;
;				(if (<= linha_do_par (+ linha_pivot 1))
;					(if(and (not(tabuleiro-preenchido-p (estado-tabuleiro state)linha_pivot i)) 
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha_pivot 1) i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha_pivot 1) (+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha_pivot 1)(+ i 2))))
					(push(cria-accao k peca-l1) lista)
;					()
;					)
;					(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_do_par i))
;						(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_do_par (+ i 1)))
;						(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_do_par (+ i 2)))
;						(not(tabuleiro-preenchido-p (estado-tabuleiro state) (- linha_do_par 1) i)))
;						(push(cria-accao i '((T nil nil)(T T T))) lista)
;						()
;					)
				)	
		(nreverse lista)
	)
)

(defun accao-l2 ()
	(let ((lista (list)))
	(loop for k from 0 to 8 do
;		(setf linha_pivot (tabuleiro-altura-coluna (estado-tabuleiro state) i))
;		(setf linha_maybe (tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1)))
;
;			(if (>=(- linha_pivot 2) linha-maybe)
;				(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_pivot i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_pivot (+ i 1))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (- linha_pivot 1)(+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (- linha_pivot 2) (+ i 1)))))
					(push(cria-accao k peca-l2) lista)
;					()
;				)
;				(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_maybe (+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha_maybe 1)(+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha_maybe 2) i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha_maybe 2) (+ i 1))))
;					(push(cria-accao i '((nil T)(nil T)(T T))) lista)
;					()
;				)
;			)	
		)
	
	(nreverse lista)
	)
)

(defun accao-l3 ()
	(let ((lista (list)))

	(loop for k from 0 to 7 do
;		(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))

;		(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;			(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;			(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 2)))
;			(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) (+ i 2))))
				(push(cria-accao k peca-l3) lista)
;			()
;		)
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
			; A TESTAR LINHA SEGUINTE
;			(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))
;
;			(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 2)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
;					(+ i 1))))
				(push (cria-accao k peca-t0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t1 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
;			(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))))
;
;			(if (and (not (tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) i))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) (+ i 1)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 2) i)))
				(push (cria-accao k peca-t1) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t2 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			; A TESTAR LINHA SEGUINTE
;			(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))
;
;			(if (and (not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
;					i))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
;					(+ i 1)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
;					(+ i 2))))
				(push (cria-accao k peca-t2) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t3 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
;			(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))))
;
;			(if (and (not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) i))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) (+ i 1)))
;				(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 2) (+ i 1))))
				(push (cria-accao k peca-t3) lista)
		)
	(nreverse lista)
	)
)




;(defun resultado(state action)
;	(let ((state_var (copia-estado state))
;		(peca_a_colocar (car(estado-pecas-por-colocar state_var))))
;		
;		(pop(estado-pecas-por-colocar state_var))
;		(push peca_a_colocar (estado-pecas-colocadas state_var))
;		;(coloca-peca (estado-tabuleiro state_var) action))
;	)
;)


;(defun coloca-peca (tabuleiro peca))


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




;(load "problema_ADT.lisp")
;(setf l(cria-accao 3 peca-l2))
;(setf *a*(cria-tabuleiro))
;(setf *est1*(cria-estado 0 '(l) nil *a*))