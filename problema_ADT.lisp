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
;;WTF?? CAMPOS = FUNCOES