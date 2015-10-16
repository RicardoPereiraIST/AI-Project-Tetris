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
	(let proxima_peca (car(estado-pecas-por-colocar state))
		(case proxima_peca 
			(i (accao-i state))
			(l (accao-l state))
			(j accao-j)
			(o accao-o)
			(s accao-s)
			(z accao-z)
			(otherwise accao-t)) 
	)
)

(defun accao-i(state)
	(if (eq (accao-peca (car(estado-pecas-por-colocar state))) ((T T)(T nil)(T nil)))
		(accao-i0 state)
		(accao-i1 state)
	)
) 
(defun accao-i0(state)
	(let lista
		(loop for i from 0 to 9 do
			(cons lista 
				(cria-accao 
					(+ (tabuleiro-altura-coluna (estado-tabuleiro state)) 1)
					((T T)(T nil)(T nil))
				)
			) 
		)
	)
)

(defun accao-i1(state)
	nil)