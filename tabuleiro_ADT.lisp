(defun cria-tabuleiro()
	(make-array (list 18 10))
)

;;(defun copia-tabuleiro
;;	(cria-tabuleiro)

(defun tabuleiro-preenchido-p (tab int1 int2)
	(if 
	(equal(aref tab int1 int2) 1)
	t
	nil)
)

(defun tabuleiro-altura-coluna (tab int)
	(loop for i from 17 downto 0 do
		(if 
		(equal(aref tab i int) 1)
		(return-from tabuleiro-altura-coluna i)
		)	
	)
	return 0
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
		(setf (aref tab1 int1 int2) 1))
)