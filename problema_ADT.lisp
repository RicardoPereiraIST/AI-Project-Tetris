(defstruct problema 
	:estado-inicial
	:solucao
	:accoes
	:resultado
	:custo-caminho)

(defun solucao (state)
	(cond ((and(not(tabuleiro-topo-preenchido(estado-tabuleiro state)))
		(equal((estado-pecas-por-colocar state) nil)))
		(return t))
	)
	(t nil)
)
;;WTF?? CAMPOS = FUNCOES