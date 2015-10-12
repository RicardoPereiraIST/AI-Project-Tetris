;;Estou a utilizar nomes em ingles para os argumentos das funcoes, e portugues para os campos
(defstruct estado
	:pontos
	:pecas-por-colocar
	:pecas-colocadas
	:tabuleiro)

(defun cria-estado (points pieces_to_place pieces_placed board)
	(make-estado 
		:pontos points
		:pecas-por-colocar pieces_to_place
		:pecas-colocadas pieces_placed
		:tabuleiro board)
)

(defun copia-estado(state)
	(cria-estado (estado-pontos state) 
		;;??
				(copy-seq(estado-pecas-por-colocar state))
				(copy-seq(estado-pecas-colocadas state))
				(copy-seq(estado-tabuleiro state))
	)	
)

)
(defun estados-iguais-p(state1 state2)
	(cond ((and (equal (estado-pontos state1)(estado-pontos state2))
		(equal (estado-pecas-por-colocar state1)(estado-pecas-por-colocar state2))
		(equal(estado-pecas-colocadas state1)(estado-pecas-colocadas state2))
		(equal(estado-tabuleiro state1)(estado-tabuleiro state2))))
		return t)
	(t nil)
)

(defun estado-final-p (state)
	(cond ((or (equal(estado-pecas-por-colocar state) 0)
		(tabuleiro-topo-preenchido (estado-tabuleiro state)))) 
	return t)
	(t nil)
)
