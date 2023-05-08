:- module(proylcc, 
	[  
		join/4,
		powerUp/3
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, RGrids):-

	/*
	Paso 1: Reemplazar el camino por ceros, calcular la suma
	*/
	reemplazarCeros(Grid,NumOfColumns, Path, GridEnCero),	
		
	/*
	Paso 2: Reemplazo la última coordenada
	*/
	ultimoElemento(Path,X),
	calcularPrimero(Grid, NumOfColumns, Path, PrimerValor),
	reemplazarValorCoordenada(GridEnCero, NumOfColumns, X, PrimerValor, GridReemplazado),
	
	/*
	Paso 3: Aplico el efecto gravedad y relleno
	*/
	length(Grid, Size),
	CantidadFilas is Size/NumOfColumns,
	iniciarGravedad(GridReemplazado, NumOfColumns, CantidadFilas, Gravedad1),

	rellenar(Gravedad1, Final),

	RGrids = [GridEnCero, GridReemplazado, Gravedad1, Final].

/*
	Retorna el ultimo elemento de la lista.
*/
ultimoElemento([_|Xs], Resultado):-
	ultimoElemento(Xs, Resultado).
ultimoElemento([X],X).

/*
	reemplazarValorCorrdenada(+Griila, +CantidadColumnas, +Coordenada, +Valor, -Resultado)
	
	Resultado es la grilla resultante de con la Coordenada reemplazada por el Valor.
*/
reemplazarValorCoordenada([X|Xs], CantidadColumnas, [Fila,Columna], Valor, Resultado):-
	Posicion is (Fila * CantidadColumnas) + Columna,
	reemplazarValorCoordenadaAux([X|Xs], Valor, Posicion, 0 , Resultado).

/*
	reemplazarValorCoordenadaAux(+Grilla,+Valor,+Posicion,+Posicion,-Resultado).
	Función recursiva auxiliar para reemplazarValorCoordenada/5
*/
% Caso Base
reemplazarValorCoordenadaAux([_|Xs],Valor,Posicion,Posicion,[Valor|Xs]).

% Caso Recursivo
reemplazarValorCoordenadaAux([X|Xs], Valor, Posicion, PosicionActual, [X|Resultado]):-
	PosicionSiguiente is PosicionActual+1,
	reemplazarValorCoordenadaAux(Xs, Valor, Posicion, PosicionSiguiente, Resultado).

/*
	valorEnCoordenada(+Grilla, +CantidadColumnas, +Coordenada, -Resultado)
		-Resultado es el velor almacenado en la Coordenada
*/
valorEnCoordenada([X|Xs], CantidadColumnas, Coordenada, Resultado):-
	Coordenada=[Fila,Columna],
	Posicion is (Fila * CantidadColumnas) + Columna,
	valorEnCoordenadaAux([X|Xs], Posicion, 0, Resultado).

/*
	valorCoordenadaAux(Grilla,Posicion,PosicionActual, Resultado)
		-Función recursiva auxiliar para valorEnCoordenada/5
*/
% Caso Base
valorEnCoordenadaAux([X|_],Posicion,Posicion,X).
% Caso Recursivo
valorEnCoordenadaAux([_|Xs], Posicion, PosicionActual, Resultado):-
	Posicion\=PosicionActual,
	PosicionSiguiente is PosicionActual+1,
	valorEnCoordenadaAux(Xs, Posicion, PosicionSiguiente, Resultado).
	


/*
	calcularPrimero(+Grilla,+CantidadColumnas,+Camino,-Resultado)
		-Resultado es el valor de la menor potencia de 2 mayor o igual a la suma de las coordenadas.
		-Camino es una lista de coordenadas de formato [Fila, Columna]
*/
calcularPrimero([X|Xs], CantidadColumnas, Camino, Resultado):-
	calcularPrimerValor([X|Xs], CantidadColumnas, Camino, Suma),
	menorPotenciaDe2(Suma, Resultado).

/*
	calcularPrimerValor(+Grilla,+CantidadColumnas,+Camino, Resultado)
		-Función recursiva auxiliar para calcularPrimero/4
		-Resultado es la suma de los valores del camino en la grilla.
*/
% Caso Base:
calcularPrimerValor(_,_,[],0).
% Caso Recursivo:
calcularPrimerValor(Grilla, CantidadColumnas,[CoordenadaActual|Resto],Suma):-
	valorEnCoordenada(Grilla,CantidadColumnas,CoordenadaActual, Valor),
	calcularPrimerValor(Grilla,CantidadColumnas,Resto,Suma1),
	Suma is Valor+Suma1.

/*
	menorPotenciaDe2(+Numero,-Resulado)
		Resultado es la menor potencai de 2 mayor o igual al numero dado.
*/
menorPotenciaDe2(Numero, Resultado) :-
    Resultado is 2**ceil(log(Numero)/log(2)).

/*	
	reemplazarCeros(+Grilla, +CantidadColumnas, +Camino, -Resultado)
		Resultado es la grilla con todas las coordenadas del camino reemplazadas por el valor '0'.
*/
% Caso Base:
reemplazarCeros(Grilla,_,[],Grilla).
% Caso Recursivo:
reemplazarCeros(Grilla,CantidadColumnas, [CoordenadaActual|Resto], Resultado):-
	reemplazarValorCoordenada(Grilla,CantidadColumnas,CoordenadaActual,0,Nueva),
	reemplazarCeros(Nueva,CantidadColumnas,Resto,Resultado).


/*
	iniciarGravedad(+Grilla,+CantidadColumnas,+CantidadFilas,-Resultado)
		-Asume las celdas en 0 como espacios vacíos, haciendo "Caer" las celdas que tengan vacío debajo.
		-Resultado es la grilla con la gravedad aplicada, con los ceros en las posiciones de superiores.
*/
iniciarGravedad(Grilla,CantidadColumnas,CantidadFilas,Resultado):-
	gravedad(Grilla, CantidadColumnas, CantidadFilas,0, Resultado).

/*
	gravedad(+Grilla, +CantidadColumnas, +CantidadFilas, +Contador, -Resultado)
		-Itera sobre la grilla CantidadFilas veces.
		-Resultado es la grilla con el efecto de gravedad aplicado.
		-Empieza por la segunda fila para que todas tengan una fila encima.
*/ 
%Caso Base:
gravedad([X|Xs], _CantidadColumnas,CantidadFilas,CantidadFilas, [X|Xs]).
%Caso Recursivo:
gravedad([X|Xs], CantidadColumnas,CantidadFilas,Contador, Resultado):-
	Contador\=CantidadFilas,
	gravedadAux([X|Xs], CantidadColumnas, [1,0], Resultado1),
	Contador1 is Contador+1,
	gravedad(Resultado1, CantidadColumnas,CantidadFilas, Contador1, Resultado).

/*
	GravedadAux(+Grilla,+CantidadColumnas,+Coordenada,-Resultado)
		Itera sobre la Grilla, cambiando de lugar las coordenadas si la de abajo tiene valor 0.
		Todos valores 0 suben una fila por cada ejecución.
*/
%Caso Recursivo: La coordenada tiene valor 0
gravedadAux(Grilla, CantidadColumnas, [Fila, Columna], Resultado):-
	Columna<CantidadColumnas,
	Fila>0,
	FilaArriba is Fila-1,
	
	valorEnCoordenada(Grilla, CantidadColumnas,[Fila,Columna], Valor),
	Valor=:=0,
	valorEnCoordenada(Grilla, CantidadColumnas, [FilaArriba, Columna], ValorArriba),
	
	reemplazarValorCoordenada(Grilla,CantidadColumnas, [Fila,Columna], ValorArriba, Grilla1),
	reemplazarValorCoordenada(Grilla1,CantidadColumnas, [FilaArriba, Columna], 0, Grilla2),

	ColumnaSiguiente is Columna+1,
	gravedadAux(Grilla2, CantidadColumnas, [Fila,ColumnaSiguiente], Resultado).

%Caso Recursivo: si la coordenada NO tiene valor 0
gravedadAux(Grilla, CantidadColumnas, [Fila, Columna], Resultado):-
	Columna<CantidadColumnas,
	Fila>0,
	
	valorEnCoordenada(Grilla, CantidadColumnas,[Fila,Columna], Valor),
	Valor\=0,

	ColumnaSiguiente is Columna+1,
	gravedadAux(Grilla, CantidadColumnas, [Fila,ColumnaSiguiente], Resultado).

%Caso Recutsivo: Se llegó hasta la última columna
gravedadAux(Grilla, CantidadColumnas, [Fila, CantidadColumnas], Resultado):-
	FilaSiguiente is Fila+1,
	gravedadAux(Grilla,CantidadColumnas,[FilaSiguiente,0], Resultado).

%Caso Base
gravedadAux(Grilla, CantidadColumnas, [_, CantidadColumnas], Grilla).

/*
	numeroAleatorio(-Numero)
		-Numero es una potencia de 2 aleatoria entre 2 y 64
*/
numeroAleatorio(Numero) :-
    random_between(1, 6, Exponente),
    Numero is 2**Exponente.

/*
	rellenar(+Lista,-Resultado)
		-Resultado es una lista con los ceros reemplazados por valores aleatorios válidos
*/
%Caso Base: La lista está vacía
rellenar([],[]).
%Caso Recursivo: La posicion es un 0
rellenar([0|Xs], [Nuevo|Resultado1]) :-
    numeroAleatorio(Nuevo),
    rellenar(Xs, Resultado1).
%Caso Recursivo: La Posicion no es 0
rellenar([X|Xs], [X|Resultado]) :-
    X \= 0,
    rellenar(Xs, Resultado).

/*
	powerUp(+Grid, +CantidadColumnas, -RGrids)
	RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas de todos los grupos de adyacentes
  	en la grilla Grid, con número de columnas CantidadColumnas. El número 0 representa que la celda está vacía. 
*/
powerUp(Grid, CantidadColumnas, RGrids):-
	/*
		Paso 1: Consigo la cantidad de Filas:
	*/
	length(Grid, Size),
	CantidadFilas is Size/CantidadColumnas,

	/*
		Paso 2:Consigo los sub-grupos de adyacentes iguales.
	*/
	clusters(Grid,CantidadFilas,CantidadColumnas,Grupos),

	/*
		Paso 3: Agrupo los sub-grupos y consigo el último estado.
	*/
	agruparTodos(Grupos,Grid,CantidadColumnas,Transiciones),
	limpiarLista(Transiciones, Transiciones2),
	last(Transiciones2, Nuevo),

	/*
		Paso 4: Aplico la gravedad al último estado y relleno con valores válidos.
	*/
	gravedad(Nuevo, CantidadColumnas,CantidadFilas,0,Resultado1),
	rellenar(Resultado1, ResultadoDefinitivo),
	
	/*
		Paso 5: Concateno las transiciones a la gravedad y la lista rellenada.
	*/
	append(Transiciones, [Resultado1], Resultado2),
	append(Resultado2, [ResultadoDefinitivo], RGrids).

/*
	agruparTodos(+ListadeClusters, +Grilla,+CantidadColumnas,Transiciones)
		Transiciones es la lista de Grillas que representa la animación de agrupar los Clusters
		Itera sobre la ListaDeClusters agrupando, seteando en 0 y sumando los valores para cada cluster
*/
%Caso Base
agruparTodos([], _, _, []).
%Caso Recursivo
agruparTodos([ListaActual|Resto], Grid, CantidadColumnas, Transiciones):-
	agrupar(ListaActual,Grid,CantidadColumnas, Grillas),
	Grillas=[_,GrillaEnCero],
	agruparTodos(Resto,GrillaEnCero,CantidadColumnas,Siguientes),
	append(Grillas,Siguientes, Transiciones).


/*
	agrupar(+ListaDeCoordenadas, +Grilla, +CantidadColumnas, -Resultado)
	Resultado es la lista que contiene las posiciones de la lista de coordenaddas
	reemplazas por "0" y la lista con el nuevo elemento a la derecha abajo de la coordenada.
*/
agrupar(Lista, Grilla, CantidadColumnas, [GridEnCero,Nuevo]):-
	reemplazarCeros(Grilla,CantidadColumnas,Lista,GridEnCero),

	calcularPrimero(Grilla, CantidadColumnas, Lista, Valor),
	abajoDerecha(Lista, Coordenada),
	reemplazarValorCoordenada(GridEnCero, CantidadColumnas, Coordenada, Valor, Nuevo).


/*
	abajoDerecha(+ListaCoordenadas, -Resultado)
		Resultado es la coordenada más abajo a la derecha de la ListaCoordenadas
*/
%Caso Base
abajoDerecha([[Fila,Columna]], [Fila,Columna]).
%Casos Recursivos: 
abajoDerecha([[Fila,Columna]|Resto], Coordenada) :-
    abajoDerecha(Resto, [Fila2,Columna2]),
    Fila >= Fila2,
    Columna >= Columna2,
    Coordenada = [Fila,Columna].
abajoDerecha([[Fila,_Columna]|Resto], Coordenada) :-
	abajoDerecha(Resto, [Fila2,Columna2]),
	Fila<Fila2,
	Coordenada=[Fila2,Columna2].
abajoDerecha([[_Fila,Columna]|Resto], Coordenada) :-
	abajoDerecha(Resto, [Fila2,Columna2]),
	Columna<Columna2,
	Coordenada=[Fila2,Columna2].
abajoDerecha([[Fila,Columna]|Resto], Coordenada) :-
	abajoDerecha(Resto, [Fila2,Columna2]),
	Fila<Fila2,
	Columna<Columna2,
	Coordenada=[Fila2,Columna2].

/*
	clusters(+Grilla, +CantidadFilas, +CantidadColumnas, -Grupos)
		-Grupos es una lista de listas de coordenadas, conteniendo cada uno de los clusters de 
		ítems iguales.
*/
%Caso Base:
clusters([],_,_,[]).

%Caso Recursivo:
clusters([X|Xs],CantidadFilas, CantidadColumnas, Grupos):-
	clustersAux([0,0], [X|Xs],CantidadFilas,CantidadColumnas,[], Grupos1),
	limpiarLista(Grupos1, Grupos).


/*
	clustersAux(+Coordenada, +Grilla, +CantidadFilas, +CantidadColumnas, +Visitados, -Resultado)
		-Resultado es la lista de todos los clusters en la grilla empezando por la coordenada dada.
		-Mantiene una lista visitados, que actualiza para verificar las siguientes coordenadas.
*/
%Caso Base:Pasé por todas las filas.
clustersAux([CantidadFilas,_], _, CantidadFilas,_,_, []).

%Caso Recursivo: La coordenada no fue visitada y es válida.
clustersAux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, [Lista2|Resultado]):-
	not(member([Fila,Columna],Visitados)),
	coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna]),

	visitar([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],Visitados,Cluster),
	%El nuevo Cluster se marca como visitado porque todas sus coordenadas ya fueron visitadas.
	append(Visitados,Cluster,Visitados1),
	eliminarRepetidos(Visitados1, VisitadosNuevo),
	
	filtrarLista(Cluster, Lista2),

	ColumnaSiguiente is Columna+1,
	clustersAux([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,VisitadosNuevo,Resultado).

% Caso Recursivo: La coordenada es válida pero fue Visitada previamente.
clustersAux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, Resultado):-
	member([Fila,Columna],Visitados),
	coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna]),
	
	ColumnaSiguiente is Columna+1,
	clustersAux([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,Visitados,Resultado).

% Caso Recursivo: La columna no es válida, empiezo por la siguiente fila.
clustersAux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, Resultado):-
	not(coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna])),
	Fila1 is Fila+1,
	clustersAux([Fila1,0],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, Resultado).




/*
	filtrarLista(+Lista,NuevaLista)
		-Si la lista tiene un solo elemento, devuelve la lista vacía.
		-Se usa porque Clusters() devuelve como subconjuntos las coordenadas que tienen un solo elemento,
		que no cuentan a la hora de agrupar
*/
filtrarLista([_], []).
filtrarLista([H|T], [H|T]).

/*
	limpiarLista(+Lista,-Resultado)
		-Resultado es la lista de listas original, pero eliminando las listas vacías en el medio.
*/
limpiarLista([], []).
limpiarLista([[]|T], Resultado) :-
    limpiarLista(T, Resultado).
limpiarLista([H|T], [H|Resultado]) :-
    H \= [],
    limpiarLista(T, Resultado).



/*
	visitar(+Coordenada, +CantidadFilas, +CantidadColumnas, +Grilla, +ListaVisitados, -Cluster)
		-Cluster es una lista de coordenadas adyacentes con el mismo valor.
		-Marca como visitados los nodos a los que se puede mover y los agrega al cluster
		-NO actualiza la lista de visitados, sino que se debe usar Append a visitados con el Cluster
		al salir de la sentencia.
*/
visitar([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs], Visitados,[[Fila,Columna]|GrupoNuevo]):-
	%Lista es una lista de coordenadas adyacentes que se pueden visitar
	puedoMover([X|Xs], CantidadFilas, CantidadColumnas,Visitados,[Fila,Columna],Lista),

	visitarAux(Lista,CantidadFilas,CantidadColumnas,[X|Xs], [[Fila,Columna]|Visitados], [],GrupoNuevo).


/*
	visitarAux(+ListaCoordenadas, +CantidadFilas, +CantidadColumnas, +Grilla, +Visitados, +GrupoActual,GrupoNuevo)
		- GrupoNuevo es el grupo formado por las coordenadas adyacentes del mismo valor.
*/
visitarAux([],_,_,_,_,Grupo,Grupo).

%Caso el nodo no fue visitado.
visitarAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,[CoordenadaActual|GrupoNuevo]):-
	not(member(CoordenadaActual, VisitadosAntes)),
	
	%Primero Visito la vecindad de la CoordenadaActual y  marco como visitados 
	puedoMover(Grilla, CantidadFilas, CantidadColumnas,VisitadosAntes,CoordenadaActual,Lista),
	visitarAux(Lista,CantidadFilas,CantidadColumnas,Grilla, [CoordenadaActual|VisitadosAntes], Grupo,GrupoNuevo1),
	
	append([CoordenadaActual|VisitadosAntes],GrupoNuevo1, VisitadosNuevo),
	%Después visito el resto de la lista
	visitarAux(Resto,CantidadFilas,CantidadColumnas,Grilla,VisitadosNuevo, GrupoNuevo1, GrupoNuevo).

visitarAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,GrupoNuevo):-
	member(CoordenadaActual, VisitadosAntes),
	visitarAux(Resto,CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,GrupoNuevo).

puedoMover([H|T], CantidadFilas,CantidadColumnas,Visitados,[Fila,Columna],Resultado):-
	Fila1 is Fila+1,
	Fila2 is Fila-1,
	Columna1 is Columna+1,
	Columna2 is Columna-1,
	
	%Arriba y abajo se llaman Norte y Sur para que empiecen con letras distintas y poder
	% armar las diagonales.
	Norte=[Fila2,Columna],
	Sur=[Fila1,Columna],
	Izquierda=[Fila,Columna2],
	Derecha=[Fila,Columna1],
	DNI=[Fila2,Columna2], %DNI -> Diagonal Norte Izquierda
	DND=[Fila2,Columna1], % Diagonal Norte Derecha
	DSI=[Fila1,Columna2], % Diagonal Sur Izquierda
	DSD=[Fila1,Columna1], % Diagonal Sur Derecha

	valorEnCoordenada([H|T], CantidadColumnas, [Fila,Columna], Valor),

	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, Norte, Valor, Resultado0),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, Sur, Valor, Resultado1),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, Izquierda, Valor, Resultado2),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, Derecha, Valor, Resultado3),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, DNI, Valor,Resultado4),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, DND, Valor,Resultado5),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, DSD, Valor,Resultado6),
	puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, DSI, Valor,ResultadoFinal),
	append([Resultado0,Resultado1,Resultado2,Resultado3,Resultado4,Resultado5,Resultado6,ResultadoFinal], Resultado).


puedoMoverAux([H|T], CantidadFilas, CantidadColumnas, Visitados, [Fila,Columna],Valor, [[Fila,Columna]]):-
	coordenadaValida(CantidadFilas,CantidadColumnas,[Fila,Columna]),
	not(member([Fila,Columna], Visitados)),
	valorEnCoordenada([H|T], CantidadColumnas, [Fila,Columna], Valor1),
	Valor=:=Valor1.

puedoMoverAux(_, CantidadFilas, CantidadColumnas, _Visitados, [Fila,Columna],_Valor, []):-
	not(coordenadaValida(CantidadFilas,CantidadColumnas,[Fila,Columna])).

puedoMoverAux(_, _CantidadFilas, _CantidadColumnas, Visitados, [Fila,Columna],_Valor, []):-
 	member([Fila,Columna], Visitados).

puedoMoverAux([H|T], _CantidadFilas, CantidadColumnas, _Visitados, [Fila,Columna],Valor, []):-
	valorEnCoordenada([H|T], CantidadColumnas, [Fila,Columna], Valor1),
	Valor=\=Valor1.

coordenadaValida(CantidadFilas,CantidadColumnas,[Fila,Columna]):-
	Fila>=0,
	Columna>=0,
	Fila<CantidadFilas,
	Columna<CantidadColumnas.



eliminarRepetidos([], []).
eliminarRepetidos([H|T], Resultado) :-
	member(H, T), eliminarRepetidos(T, Resultado).
eliminarRepetidos([H|T], [H|Resultado]) :-
	eliminarRepetidos(T, Resultado).

	
	
	



