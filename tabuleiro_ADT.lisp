(defun cria-tabuleiro
	(make-array (list 18 10))
		)
)

;;(defun copia-tabuleiro
;;	(cria-tabuleiro)

(defun tabuleiro-preenchido-p (tab int1 int2)
	(if (equal(aref(tab int1 int2)) 1)
		return t)
	(else (return nil))
)

(defun tabuleiro-altura-coluna (tab int)
	(loop as i from 17 downto 0
		(if(equal(aref(tab i int)) 1)
			return i)
	)
	(t 0)
)

(defun tabuleiro-linha-completa (tab int)
	(dotimes(j 10)
		(if(equal(aref(tab int j)) 0)
			return 0
		)
	)
	(t 1)
)

(defun tabuleiro-preenche!(tab1 int1 int2)
	(cond(and(<= int1 17)(>= int1 0)(<= int2 9)(>= int2 0))
		(setf(aref(tab int1 int2) 1))
	)
)

