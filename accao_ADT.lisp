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

