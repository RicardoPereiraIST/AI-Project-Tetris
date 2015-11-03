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
		(equal(aref tab i int) t)
		(return-from tabuleiro-altura-coluna (+ i 1))
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

(defun tabuleiro->array(tab)
	(setf array (make-array (list 18 10)))
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref array num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
	(return-from tabuleiro->array array)
)

(defun array->tabuleiro(array)
	(setf tabuleiro (cria-tabuleiro))
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref array num_linha num_coluna))
		)
	)
	(return-from array->tabuleiro tabuleiro)
)

(defun copia_tabuleiro(tab)
	(setf tabuleiro (cria-tabuleiro))
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
	(return-from copia_tabuleiro tabuleiro)
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
		:pontos (estado-pontos state)  
		:pecas-por-colocar (estado-pecas-por-colocar state) 
		:pecas-colocadas (estado-pecas-colocadas state) 
		:tabuleiro (estado-tabuleiro state)) 
)


(defun estados-iguais-p(state1 state2)
	(if (and (equal (estado-pontos state1)(estado-pontos state2))
		(equal (estado-pecas-por-colocar state1)(estado-pecas-por-colocar state2))
		(equal(estado-pecas-colocadas state1)(estado-pecas-colocadas state2))
		(equal(estado-tabuleiro state1)(estado-tabuleiro state2))
		))
	t
	nil)


(defun estado-final-p (state)
	(if (or (equal(estado-pecas-por-colocar state) 0) (tabuleiro-topo-preenchido (estado-tabuleiro state))) 
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
	(if (and(not(tabuleiro-topo-preenchido (estado-tabuleiro state)))
		(equal(estado-pecas-por-colocar state) nil)))
	t
	nil)

(defun accoes(state)
	(setf proxima_peca (estado-pecas-por-colocar state))
		(print proxima_peca)
		(case proxima_peca 
			(I (accao-i state))  ;ATENCAO UPPER CASE
			(L (accao-l state))
			;(j accao-j)
			;(o accao-o)
			;(s accao-s)
			;(z accao-z)
			;(otherwise)
		) 	
)

(defun accao-i(state)
	(append (accao-i0 state) (accao-i1 state))
) 

(defun accao-l(state)
	(append (accao-l0 state) (accao-l1 state) (accao-l2 state) (accao-l3 state))
)

(defun accao-i0(state)
;;lambda para lista ser "global" para o accao i0 e i1??
	(let(lista (make-list 0))
		(loop for k from 0 to 9 do
			(push (cria-accao k '((T)(T)(T)(T))) lista)
		)
	(nreverse lista)
	)
)


(defun accao-i1(state)
	(let (lista (make-list 0))
		(loop for k from 0 to 6 do
			(push (cria-accao k '(T T T T)) lista)
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


;(defun accao-l(state)
;	(cons (accao-l0 state) (accao-l1 state) (accao-l2 state) (accao-l3 state))
;)

(defun accao-l0 (state)
	(let (lista (make-list 0))
		(loop for k from 0 to 8 do

;			(setf linha (max(tabuleiro-altura-coluna (estado-tabuleiro state) i)
;				(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))))
;				(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 2) i)))
					(push(cria-accao k '((T T)(T nil)(T nil))) lista)
;					()
;				)
		)
	(nreverse lista)
	)
)

(defun accao-l1 (state)
	(setf lista nil)
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
					(push(cria-accao k '((T nil nil)(T T T))) lista)
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

(defun accao-l2 (state)
	(setf lista nil)
	(loop for k from 0 to 8 do
;		(setf linha_pivot (tabuleiro-altura-coluna (estado-tabuleiro state) i))
;		(setf linha_maybe (tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1)))
;
;			(if (>=(- linha_pivot 2) linha-maybe)
;				(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_pivot i))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha_pivot (+ i 1))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (- linha_pivot 1)(+ i 1)))
;					(not(tabuleiro-preenchido-p (estado-tabuleiro state) (- linha_pivot 2) (+ i 1)))))
					(push(cria-accao k '((nil T)(nil T)(T T))) lista)
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

(defun accao-l3 (state)
	(setf lista nil)

	(loop for k from 0 to 7 do
;		(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
;			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))

;		(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
;			(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
;			(not(tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 2)))
;			(not(tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) (+ i 2))))
				(push(cria-accao k '((T T T)(nil nil T))) lista)
;			()
;		)
	)
	(nreverse lista)
)
;(load "problema_ADT.lisp")
;(setf i (cria-accao 3 '(T T T T)))
;(setf *a*(cria-tabuleiro))
;(setf *est*(cria-estado 0 'i nil *a*)) 

;;peca t
;(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)
																;(nil T nil))))
(defun accao-t0 (state)
	(setf lista nil)
	(loop for i from 0 to 7 do
		; A TESTAR LINHA SEGUINTE
		(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))

		(if (and (not(tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 2)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
				(+ i 1))))
			(push (cria-accao i '((T T T) (nil T nil))) lista)
		)
	)

)





;(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defun accao-t1 (state)
	(setf lista nil)
	(loop for i from 0 to 8 do
		(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))))

		(if (and (not (tabuleiro-preenchido-p (estado-tabuleiro state) linha i))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) i))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) (+ i 1)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 2) i)))
			(push (cria-accao i '((T nil)(T T)(T nil))))

		)
	)
)
;(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defun accao-t2 (state)
	(setf lista nil)
	(loop for i from 0 to 7 do
		; A TESTAR LINHA SEGUINTE
		(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))
			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 2))))

		(if (and (not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
				i))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
				(+ i 1)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1)
				(+ i 2))))
			(push (cria-accao i '((nil T nil) (T T T))) lista)
		)
	)

)


;(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

(defun accao-t3 (state)
	(setf lista nil)
	(loop for i from 0 to 8 do
		(setf linha (max (tabuleiro-altura-coluna (estado-tabuleiro state) i)
			(tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1))))

		(if (and (not (tabuleiro-preenchido-p (estado-tabuleiro state) linha (+ i 1)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) i))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 1) (+ i 1)))
			(not (tabuleiro-preenchido-p (estado-tabuleiro state) (+ linha 2) (+ i 1))))
			(push (cria-accao i '((nil T)(T T)(nil T))))
		)
	)
)

(defun accao-i1(state)
	nil)


;;peca j
;(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))

(defun accao-j0(state)
	(setf lista nil)
		(loop for i from 0 to 8 do
			(push(cria-accao i '((T T)(nil T)(nil T))) lista)
		)
)

;(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))

(defun accao-j1(state)
	(setf lista nil)
		(loop for i from 0 to 7 do
			(push(cria-accao i '((T T T)(T nil nil))) lista)
		)
)

;(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))

(defun accao-j2(state)
	(setf lista nil)
		(loop for i from 0 to 8 do
			(push(cria-accao i '((T nil)(T nil)(T T))) lista)
		)
)

;(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))

(defun accao-j3(state)
	(setf lista nil)
		(loop for i from 0 to 7 do
			(push(cria-accao i '((nil nil T)(T T T))) lista)
		)
)

;;peca o
;(defconstant peca-o0 (make-array (list 2 2) :initial-element T))

(defun accao-o0(state)
	(setf lista nil)
		(loop for i from 0 to 8 do
			(push(cria-accao i '((T T)(T T))) lista)
		)
)

;;peca s
;(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))

(defun accao-s0(state)
	(setf lista nil)
		(loop for i from 0 to 7 do
			(push(cria-accao i '((T T nil)(nil T T))) lista)
		)
)

;(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))

(defun accao-s1(state)
	(setf lista nil)
		(loop for i from 0 to 8 do
			(push(cria-accao i '((nil T)(T T)(T nil))) lista)
		)
)
