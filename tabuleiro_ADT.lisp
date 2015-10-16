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
		(equal(aref tab i int) 1)
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


