

;;-------------------------------Tipo ACCAO----------------------------------------
(defun cria-accao (int array) 
	(cons int array)
)

(defun accao-coluna (act)
	(first act)
)

(defun accao-peca (act)	
	(cdr act)
)

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
		(if (tabuleiro-preenchido-p tab i int)
			(return-from tabuleiro-altura-coluna (1+ i))
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

(defun tabuleiro-linha-completa-p (tab int)
	(loop for j from 0 to 9 do
		(if
		(equal(aref tab int j) nil)
		(return-from tabuleiro-linha-completa-p nil)  
		)
	)
	t
)

(defun tabuleiro-preenche!(tab1 int1 int2)
	(if(and(<= int1 17)(>= int1 0)(<= int2 9)(>= int2 0))
		(setf (aref tab1 int1 int2) t))
)	

(defun tabuleiro-remove-linha!(tab num_linha)
	(if (equal num_linha 17)
		(progn
			(copia-linha-abaixo tab num_linha)
			(preenche-linha tab num_linha 9 nil)
		)
		(progn
			(copia-linha-abaixo tab num_linha)
			(tabuleiro-remove-linha! tab (+ num_linha 1))	
		)
	)

)


(defun tabuleiro-topo-preenchido-p(tab)
	
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
(defun tabuleiro->array (tab)
	(let ((array (make-array (list 18 10))))


	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref array num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
	array
	)
)
	
(defun array->tabuleiro(array)
	(let ((tabuleiro (cria-tabuleiro)))

	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref array num_linha num_coluna))
		)
	)
	tabuleiro)
)

(defun copia-tabuleiro(tab)
	(let ((tabuleiro (cria-tabuleiro)))
	(loop for num_coluna from 0 to 9 do
		(loop for num_linha from 0 to 17 do
			(setf (aref tabuleiro num_linha num_coluna) 
				(aref tab num_linha num_coluna))
		)
	)
	tabuleiro)
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

(defun copia-linha-abaixo(tab num_linha)
	(if (>= (+ num_linha 1) 18)
		(return-from copia-linha-abaixo nil)
		(progn
			(loop for num_coluna from 0 to 9 do
				(setf (aref tab  num_linha num_coluna)
					(aref tab (+ num_linha 1) num_coluna))
			)
		)

	)
)
;-------------------------------------------------Tipo ESTADO-------------------------------
(defstruct estado
	pontos
	pecas-por-colocar
	pecas-colocadas
	tabuleiro)

(defun cria-estado (points pieces_to_place pieces_placed board)
	(make-estado 
		:pontos points
		:pecas-por-colocar (cdr(cons nil pieces_to_place))
		:pecas-colocadas (cdr(cons nil pieces_placed))
		:tabuleiro board)
)

(defun copia-estado(state)
	(let ((novo-tabuleiro nil)
		(novo-pecas-colocadas nil)
		(novo-pecas-por-colocar nil))

	(setf novo-pecas-por-colocar (copy-list (estado-pecas-por-colocar state) ))
	(setf novo-pecas-colocadas (copy-list (estado-pecas-colocadas state)))
	(setf novo-tabuleiro (copia-tabuleiro (estado-tabuleiro state)))

	(make-estado 
		:pontos (estado-pontos state)    	
		:pecas-por-colocar novo-pecas-por-colocar
		:pecas-colocadas novo-pecas-colocadas
		:tabuleiro novo-tabuleiro) 
	)
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
	solucao ; funcao
	accoes 	;funcao
	resultado ; funcao
	custo-caminho ;funcao
)

(defun solucao (state)              
	(if (and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro state)))
		(equal(estado-pecas-por-colocar state) nil))
	t
	nil)
)

(defun accoes(state)
	(if (estado-final-p state)
		nil
		(case (car(estado-pecas-por-colocar state)) 
			(I (accao-i))
			(L (accao-l))
			(J (accao-j))
			(O (accao-o))
			(S (accao-s))
			(Z (accao-z))
			(t (accao-t))
		)
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
		)
	(nreverse lista)
	)
)

(defun accao-l0 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-l0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-l1 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-l1) lista)
		)	
	(nreverse lista)
	)
)

(defun accao-l2 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push(cria-accao k peca-l2) lista)	
		)
	(nreverse lista)
	)
)

(defun accao-l3 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push(cria-accao k peca-l3) lista)
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
			(push (cria-accao k peca-t0) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t1 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push (cria-accao k peca-t1) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t2 ()
	(let ((lista (list)))
		(loop for k from 0 to 7 do
			(push (cria-accao k peca-t2) lista)
		)
	(nreverse lista)
	)
)

(defun accao-t3 ()
	(let ((lista (list)))
		(loop for k from 0 to 8 do
			(push (cria-accao k peca-t3) lista)
		)
	(nreverse lista)
	)
)




(defun resultado(state action)
	(let ((state_var (copia-estado state))
		(minimo_pivot 0)
		(altura_final_pivot 0)
		(peca_a_colocar nil)
		(tab nil)
		(altura_peca 0)
		(count 0))

		(setf peca_a_colocar (car(estado-pecas-por-colocar state_var)))
		(setf altura_peca (first(array-dimensions (accao-peca action))))
		(setf tab (estado-tabuleiro state_var))


		(setf minimo_pivot (calcula-max-altura tab action))

		(setf altura_final_pivot (calcula-onde-desenha tab minimo_pivot action))

		(setf tab (desenha state_var altura_final_pivot action))


		(pop(estado-pecas-por-colocar state_var))
		(push peca_a_colocar (estado-pecas-colocadas state_var))


		(if (tabuleiro-topo-preenchido-p tab)
			()
			(loop for k from altura_final_pivot to (+ altura_peca altura_final_pivot) do
				(if (tabuleiro-linha-completa tab k) 
					(progn
					 	(tabuleiro-remove-linha! tab k)
					 	(incf count 1)
					 	(decf k 1)
					)
				)
			)
		)
	(incf  (estado-pontos state_var) (soma-pontos count))
	state_var
	)
)

(defun calcula-max-altura(tabu action)						;Calcula a altura da maior coluna onde a peca se podera apoiar 
	(let ((largura (second (array-dimensions (accao-peca action))))
		(inicio (accao-coluna action))
		(maximo (tabuleiro-altura-coluna tabu (accao-coluna action))))

		(loop for k from inicio to (+ inicio (1- largura)) do
			(if (< maximo (tabuleiro-altura-coluna tabu k))
				(setf maximo (tabuleiro-altura-coluna tabu k))
			)
		)
	maximo
	)
)
(defun calcula-onde-desenha(tabu min action)		;Calcula a posicao mais abaixo onde pode ser desenhada a peca
	(let ((k min)
		)

		(loop while (and (>= k 0) (posso-desenhar tabu action k)) do
			(decf k 1)
		)
		(incf k 1)
	)
)

(defun posso-desenhar (tabu action linha)			;Devolve T caso seja possivel desenhar a accao recebida no tabuleiro, nil cc
	(let ((coluna (accao-coluna action))
		(nr-linhas (first(array-dimensions(accao-peca action))))
		(nr-colunas (second(array-dimensions(accao-peca action))))
		(x linha))

	(when (and(< x 18) (>= x 0)) 
		(loop for x from 0 to (- nr-linhas 1) do
			(loop for y from 0 to (- nr-colunas 1) do

				(if (aref (accao-peca action) x y)
					(if (tabuleiro-preenchido-p tabu (+ linha x) (+ coluna y))
						(return-from posso-desenhar nil)
					)
				)
			)
		)
	)
	)
	t
) 

(defun desenha (state altura action)			;Preenche as quadriculas do tabuleiro conforme a configuracao da peca
	(let ((coluna (accao-coluna action))
		(tabu (estado-tabuleiro state))
		(nr-linhas (first(array-dimensions(accao-peca action))))
		(nr-colunas (second(array-dimensions(accao-peca action))))
		(i altura)
		)


		(when (< i 18) 
			(loop for x from 0 to (- nr-linhas 1) do
				(loop for y from 0 to (- nr-colunas 1) do
					(if (aref (accao-peca action) x y)
						(tabuleiro-preenche! tabu (+ i x) (+ coluna y))
					)
				)
			)
		)
	tabu
	)
)

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

(defun soma-pontos (nr_linhas_removidas)
	(cond 
		((eq nr_linhas_removidas 0) 0)
		((eq nr_linhas_removidas 1) 100)
		((eq nr_linhas_removidas 2) 300)
		((eq nr_linhas_removidas 3) 500)
		((eq nr_linhas_removidas 4) 800)	 
	)
)


;------------------------------------------------2 Entrega--------------------------------------------

;(load "problema_ADT.lisp")
;(setf l2(cria-accao 3 peca-l2))
;(setf t2(cria-accao 1 peca-t2))
;(setf *a*(cria-tabuleiro))
;(setf *est1*(cria-estado 0 '(l) nil *a*))
;(calcula-max-altura *a* l2)
;(calcula-onde-desenha *a* 3 l2)

;(defstruct problema 
;	estado-inicial
;	solucao
;	accoes
;	resultado
;	custo-caminho)

;(defstruct node
;	estado
;	parent
;	accao
;	cost
;	depth)

;(defstruct estado
;	pontos 
;	pecas-por-colocar
;	pecas-colocadas
;	tabuleiro)
;----------------------------------NODE------------------------------------
(defstruct node
  ;Node for generic search.  A node contains a state, a domain-specific
  ;representation of a point in the search space.  A node also contains 
  ;bookkeeping information such as the cost so far (g-cost) and estimated cost 
  ;to go (h-cost).
  (state (required))        ; a state in the domain
  (parent nil)              ; the parent node of this node
  (action nil)              ; the domain action leading to state
  (depth 0)                 ; depth of node in tree (root = 0)
  (g-cost 0)                ; path cost from root to node
  (h-cost 0)                ; estimated distance from state to goal
  (f-cost 0)                ; g-cost + h-cost
  )

(defun create-start-node (problem)
  ;Make the starting node, corresponding to the problem's initial state.
  (let ((h (h-cost problem (problema-estado-inicial problem))))
    (make-node :state (problema-estado-inicial problem) 
    	:h-cost h 
    	:f-cost h)
   )
)

(defun h-cost (problem state)
	(ignore-value problem)
	(ignore-value state)
	0
)
;return heuristic-cost;

;---------------------------QUEUE---------------------------------
(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-q))

(defun empty-queue? (q)
  (= (length (q-elements q)) 0))

(defun make-initial-queue (problem queuing-fn)
  (let ((q (make-empty-queue)))
    (funcall queuing-fn q (list (create-start-node problem)))
    q))

(defun enqueue-at-front (q items)
  ;Add a list of items to the front of the queue.
  (setf (q-elements q) (nconc items (q-elements q))))


(defun enqueue-by-priority (q items key)
  ;Insert the items by priority according to the key function.
  ; First make sure the queue is in a consistent state
  (setf items (reverse items))       ;Para criterio de desempate queremos a que foi expandida depois
  (setf (q-key q) key)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ; Now insert the items
  (loop while items do    ;ERA UM FOR EACH, PODE TER MUDADO ALGUMA COISA, NAO SEI COMO E' FEITA A INSERCAO
  		(let ((item (pop items)))
       		(heap-insert (q-elements q) item key))))


(defun remove-front (q)
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))

;------------------------PROCURAS-------------------------------

(defun goal-test(problem node)  
	(funcall (problema-solucao problem) (node-state node))
)

(defun expand (node problem)  ;Devolve LIFO
  ;Generate a list of all the nodes that can be reached from a node.
    (let ((nodes nil)
    	(lista_accoes (funcall (problema-accoes problem) (node-state node))))
	    (loop while lista_accoes do
	    	(let ((this_action (pop lista_accoes))
	    		(new_state nil)
	    		(g 0)
	    		(h 0))

	    		(setf new_state (resultado (node-state node) this_action))
		    	(setf g (+ (node-g-cost node) (funcall (problema-custo-caminho problem) (node-state node))))  ;custo-caminho do node filho ou do pai? 
		    	(setf h (h-cost problem (node-state node)))										                     ;Faco a diferenca entre os dois?
		    	
	    		(push (make-node :action this_action 
			    				:state new_state
			    				:parent node
			    				:depth (1+ (node-depth node))
			    				:g-cost g
			    				:h-cost h
			    				:f-cost (max (node-f-cost node) (+ g h)))
			    nodes)	
			)
		)
	nodes
	)
)


(defun solution-actions (node &optional (actions-so-far nil))  ;BACKTRACKING FUNCTION
  ;Return a list of actions that will lead to the node's state.
  (cond ((null node) actions-so-far)
	((null (node-parent node)) actions-so-far)
	(t (solution-actions (node-parent node)
			     (cons (node-action node) actions-so-far)))))


(defun general-search (problem queuing-fn)
  (let ((nodes (make-initial-queue problem queuing-fn)) node)
    (loop (if (empty-queue? nodes) (RETURN nil))
	  (setq node (remove-front nodes))
	  (if (goal-test problem node) (RETURN node))
	  (funcall queuing-fn nodes (expand node problem))))                        ;FALTA DEVOLVER A LISTA DAS ACCOES QUE LEVARAM AO ESTADO
  )


;-------------------------------PROCURA-PP-------------------------------------

(defun procura-pp (problem)
	(solution-actions (general-search problem #'enqueue-at-front))   ;general-search returns final node or nil 
)

;-----------------------------PROCURA-A-----------------------------------------

(defun procura-A*(problem heuristic)
	(solution-actions (best-first-search problem heuristic))
)

(defun best-first-search (problem eval-fn)
  ;Search the nodes with the best evaluation first.
  (solution-actions (general-search problem #'(lambda (old-q nodes) 
			      (enqueue-by-priority old-q nodes eval-fn)))))


;-------------------------------HEAPS--------------------------------------------

(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  ;Assume that the children of i are heaps, but that heap[i] may be 
  ;larger than its children.  If it is, move heap[i] down where it belongs.
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
					 (heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  ;Pop the best (lowest valued) item off the heap.
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  ;Put an item into a heap.
  ; Note that ITEM is the value to be inserted, and KEY is a function
  ; that extracts the numeric value from the item.
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
	(val (funcall key item)))
    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val)) do 
    	(setf (aref heap i) (aref heap (heap-parent i))
	       i (heap-parent i)))
    (setf (aref heap i) item)))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))



; ------------------------------ Procura Best
(defun procura-best (array pecas-por-colocar)
	;procura-best : array x lista pecas -> accoes
	;Devolve sequencia de accoes que levam a maximiazar pontos

	(let state (make-estado
		:pontos 0
		:pecas-por-colocar pecas-por-colocar
		:pecas-colocadas nil
		:tabuleiro (array->tabuleiro array)))

	(let problem (make-problema
					:estado-inicial state))

	(best-first-search problem fn-heuristica)
)


;--------------------------------------------Heuristicas-----------------------------------



;Soma das alturas de todas as colunas     (minimizar)
(defun h1 (state)  		;Aggregate height
	;
	(let ((aggregate_height 0))

		(loop for i from 0 to 9 do
			(incf aggregate_height (tabuleiro-altura-coluna (estado-tabuleiro state) i))	
		)
	aggregate_height)
)

;heuristic complete lines??
; nr de buracos no tabuleiro      (minimizar)
(defun h3 (state) 		;buracos cobertos do lado de cima
	(let ((holes 0))

		(loop for i from 0 to 9 do
			(loop for j from (- (tabuleiro-altura-coluna (estado-tabuleiro state) i) 1) downto 0 do
				(if (tabuleiro-preenchido-p (estado-tabuleiro state) i j)
					()
					(incf holes 1)
				)
			)
		)
	holes
	)
)


(defun h4 (state)  		;sum dos modulos das diferencas de alturas aka slopes (minimizar)
	(let ((count 0))
		(loop for i from 0 to 8 do
			(incf count (abs(- (tabuleiro-altura-coluna (estado-tabuleiro state) i) (tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1)))))
		)
	count
	)
)


;nr de pecas colocadas    (minimizar)
(defun h5 (state)   	
	(let ((pecas 0))
		(loop for i from 0 to 9 do
			(loop for j from 0 to (- (tabuleiro-altura-coluna (estado-tabuleiro state) i) 1) do
				(if (tabuleiro-preenchido-p (estado-tabuleiro state) i j)
					(incf pecas 1)
				)
			)
		)
	pecas
	)
)

;Devolve maior slope
(defun h6 (state)  		;higher slope
	(let ((maior 0))
		(loop for i from 0 to 9 do
			(if (> (abs(- (tabuleiro-altura-coluna (estado-tabuleiro state) i) (tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1)))) maior)
				(setf maior (abs(- (tabuleiro-altura-coluna (estado-tabuleiro state) i) (tabuleiro-altura-coluna (estado-tabuleiro state) (+ i 1)))))
				()
			)
		)
	)
)


;(defstruct problema 
;	estado-inicial
;	solucao
;	accoes
;	resultado
;	custo-caminho)

;(defstruct estado
;	pontos 
;	pecas-por-colocar
;	pecas-colocadas
;	tabuleiro)

;----------------------------Alg Genetico-----------
(defstruct candidato
		constantes
		racio
)

;VAR GLOBAIS 
; heur_list
; const_strc

(setf heur_list '(h1 h3 h4 h5 h6))

(setf const_struc nil)

(defun genetic-alg (problem heur_list population first-time) 
	; 1 aplicar funcao de fitness a lista de constantes
	
	(cond (eq first-time T)
		(setf calculated_ppl population)
		(T (setf calulate_ppl '()) 
			(loop for const_list in calculated_ppl do
				(setf const_struc (make-candidato 
								:constantes const_list))
				;chamar fit fun
				(setf (candidato-racio const_struc) (fitness-fun problem))
				(append calculated_ppl 'temp)
			)

		)
	)
	; 2 Fazer CrossOver Ideia de escolher os n melhores tais que os 
	; CrossOver entre os n melhores geram o mesmo numero de 
	; filhos que os elementos da população actual 
	; Falta mixing rate 0.7 0.3

	(setf select_ppl '())
	(loop for i from 0 to (floor (/ (list-length population) 2)) do
		(append select_ppl '(nth (random (list-length population)) population))
	)

	;ordena pelo racio
	(sort select_ppl (lambda(struc1 struc2) (> (candidato-racio struc1) (candidato-racio struc2))))

	; Escolher melhores
	; Lista que vai receber a nova populacao
	(setf new_ppl '())
	; Lista de elementos retirados da populacao dos pais	
	(setf poped_const '())
	; guarda 1o  melhor elemento
	(setf poped_const (append poped_const (pop select_ppl)))
	
	(loop while (< (list-length new_ppl) 
				(list-length select_ppl))
		(setf const_list (pop select_ppl)) ; pop do elemento 
		(loop for i from 0 to (list-length poped_const) do
			;bater a const_list contra todas as const_list do poped_const e adicionar os  novos elementos a nova populacao
			(setf pai const_list)
			(setf mae (nth i poped_const))
			(setf new_ppl (append new_ppl '(crossOver problem pai mae)))
			(if (>= (list-length new_ppl) 
				(floor (/ (list-length population) 2)))
				(return)
			)
		)
	)
	; 3 Mutacao
	; Necessaria uma prob de mutação 0.05




	
	(funcall genetic-alg problem heur_list new_ppl nil)
)

; Função de fitness --> classifica cada proposta de solucao (calcula racio de pontos/max pontos)
; CrossOver -> pais com max racio
; Mutacao ->

; Nota: Ordenar a medida que se calcula fitness function
; estrutura passada por referencia (assim como todos os seus elementos)
; lista passada por valor



; calcular heuristica
(defun joinHeur (state)
	(setf heur 0)

	(if (null const_struc) (return-from joinHeur heur))

	; heur(state) = A * h1(state) + B * h2 state
	(loop for i in (list-length heur_list) do
		(setf heur (+ heur 
				(* (funcall (nth i heur_list) state) 
					(nth i const_struc))))
	)
	heur
)

(defun fitness-fun (problem)
	(let ((copiaProb (copy-structure problem)))
		; aplica a*
		(setf lista_ac (procura-A* copiaProb 'joinHeur))
		;aplica accoes ao estado do problema
		(loop for accao in lista_ac do
			(setf (problema-estado-inicial copiaProb) (resultado (problema-estado-inicial copiaProb) accao))
		)
		( / ( estado-pontos (problema-estado-inicial copiaProb)) (custo-oportunidade (problema-estado-inicial copiaProb)))
	)
)

(defun crossOver (problema pai mae)
	(setf copiaProb (copy-structure problema))
	(setf racio 0.7)
	; Esta a ser usado um racio de 70% para a geracao do melhor filho
	(setf const_list1 '())
	(setf const_list2 '())
	(loop for j from 0 to (list-length (candidato-constantes pai)) do
		(setf const_list1 (append const_list1 '(+ (* racio (nth j (candidato-constantes pai))) (* (- 1 racio) (nth j (candidato-constantes mae))))))
		(setf const_list2 (append const_list2 '(+ (* (- 1 racio) (nth j (candidato-constantes pai))) (* racio (nth j (candidato-constantes mae))))))



	)
	(setf primeiro (make-candidato 
		:constantes const_list1))
	; QUALQUER COISA MAL candidato é lista de constantes
	(setf segundo (make-candidato 
		:constantes const_list2))

	;calcula pontos do primeiro filho
	(setf const_struc primeiro)
	(setf (candidato-racio const_struc) (fitness-fun problema))
	(setf primeiro const_struc)

	;calcula pontos do segundo filho
	(setf const_struc segundo)
	(setf (candidato-racio const_struc) (fitness-fun problema))
	(setf segundo const_struc)

	(if (> (candidato-racio primeiro) (candidato-racio segundo))
		(return-from crossOver primeiro)
		(return-from crossOver segundo)	
	)
)


(load "utils.lisp")

