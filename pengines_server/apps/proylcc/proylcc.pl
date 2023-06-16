:- module(proylcc, 
	[  
		join/4,
		powerUp/3,
		movidaMaxima/3,
		maximoAdyacente/3
	]).

/*
 * 
 * 
 *		PROYECTO 1
 * 
 * 
 */


/*
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
	last(Path, X),
	calcularUltimo(Grid, NumOfColumns, Path, PrimerValor),
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
	reemplazarValorCorrdenada(+Griila, +CantidadColumnas, +Coordenada, +Valor, -Resultado)
		Resultado es la grilla resultante de con la Coordenada reemplazada por el Valor.
*/
reemplazarValorCoordenada([X|Xs], CantidadColumnas, [Fila,Columna], Valor, Resultado):-
	Posicion is (Fila * CantidadColumnas) + Columna,
	reemplazarValorCoordenadaAux([X|Xs], Valor, Posicion, 0 , Resultado).

/*
	reemplazarValorCoordenadaAux(+Grilla,+Valor,+Posicion,+Posicion,-Resultado).
		Función recursiva auxiliar para reemplazarValorCoordenada/5
		Avanzo en la lista hasta que coincidan los índices y reemplazo el valor.
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
	calcularUltimo(+Grilla,+CantidadColumnas,+Camino,-Resultado)
		-Resultado es el valor de la menor potencia de 2 mayor o igual a la suma de las coordenadas.
		-Camino es una lista de coordenadas de formato [Fila, Columna]
*/
calcularUltimo([X|Xs], CantidadColumnas, Camino, Resultado):-
	calcularUltimoValor([X|Xs], CantidadColumnas, Camino, Suma),
	menorPotenciaDe2(Suma, Resultado),!.


/*
	calcularUltimoValor(+Grilla,+CantidadColumnas,+Camino, Resultado)
		-Función recursiva auxiliar para calcularPrimero/4
		-Resultado es la suma de los valores del camino en la grilla.
*/
% Caso Base:
calcularUltimoValor(_,_,[],0):-!.
% Caso Recursivo:
calcularUltimoValor(Grilla, CantidadColumnas,[CoordenadaActual|Resto],Suma):-
	valorEnCoordenada(Grilla,CantidadColumnas,CoordenadaActual, Valor),
	calcularUltimoValor(Grilla,CantidadColumnas,Resto,Suma1),
	Suma is Valor+Suma1,!.


/*
	menorPotenciaDe2(+Numero,-Resulado)
		Resultado es la menor potencia de 2 mayor o igual al numero dado.
*/
menorPotenciaDe2(0,0).
menorPotenciaDe2(Numero, Resultado) :-
    Resultado is 2^ceil(log(Numero)/log(2)).

/*	
	reemplazarCeros(+Grilla, +CantidadColumnas, +Camino, -Resultado)
		-Resultado es la grilla con todas las coordenadas del camino reemplazadas por el valor '0'.
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
		-Resultado es la grilla con la gravedad aplicada, con los ceros en las posiciones superiores.
*/
iniciarGravedad(Grilla,CantidadColumnas,CantidadFilas,Resultado):-
	gravedad(Grilla, CantidadColumnas, CantidadFilas,0, Resultado).

/*
	gravedad(+Grilla, +CantidadColumnas, +CantidadFilas, +Contador, -Resultado)
		-Itera sobre la grilla CantidadFilas veces.
		-Resultado es la grilla con el efecto de gravedad aplicado.
*/ 
%Caso Base: Se ejecutó gravedadAux cantidadFilas veces.
gravedad([X|Xs], _CantidadColumnas,CantidadFilas,CantidadFilas, [X|Xs]).
%Caso Recursivo:
gravedad([X|Xs], CantidadColumnas,CantidadFilas,Contador, Resultado):-
	Contador\=CantidadFilas,
	gravedadAux([X|Xs], CantidadColumnas, [1,0], Resultado1),
	Contador1 is Contador+1,
	gravedad(Resultado1, CantidadColumnas,CantidadFilas, Contador1, Resultado).

/*
	GravedadAux(+Grilla,+CantidadColumnas,+Coordenada,-Resultado)
		-Itera sobre la Grilla, cambiando de lugar las coordenadas si la de abajo tiene valor 0.
		-Todas las coordenadas en 0 suben una fila por cada ejecución de la sentencia.
		-Se tiene que empezar por la segunda ([1,0]) fila para que todas tengan una fila encima.
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

%Caso Recursivo: Se llegó hasta la última columna y paso de Fila
gravedadAux(Grilla, CantidadColumnas, [Fila, CantidadColumnas], Resultado):-
	FilaSiguiente is Fila+1,
	gravedadAux(Grilla,CantidadColumnas,[FilaSiguiente,0], Resultado).

%Caso Base: Llegué a la última fila.
gravedadAux(Grilla, CantidadColumnas, [_, CantidadColumnas], Grilla).



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
	numeroAleatorio(-Numero)
		-Numero es una potencia de 2 aleatoria entre 2 y 64
*/
numeroAleatorio(Numero) :-
    random_between(1, 6, Exponente),
    Numero is 2**Exponente.





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
	last(Transiciones, Nuevo),

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
	agruparTodos(+ListadeClusters, +Grilla,+CantidadColumnas,-Transiciones)
		-Transiciones es la lista de Grillas que representa la animación de agrupar los Clusters
		-Itera sobre la ListaDeClusters agrupando, seteando en 0 y sumando los valores para cada cluster.
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
		Resultado es la lista que contiene dos Grillas, con las coordenaddas de la ListadeCoordenadas
		reemplazas por "0" y la GrillaEnCero con el nuevo elemento a la derecha abajo de la coordenada.
*/
agrupar(Lista, Grilla, CantidadColumnas, [GridEnCero,Nuevo]):-
	reemplazarCeros(Grilla,CantidadColumnas,Lista,GridEnCero),

	calcularUltimo(Grilla, CantidadColumnas, Lista, Valor),
	abajoDerecha(Lista, Coordenada),
	reemplazarValorCoordenada(GridEnCero, CantidadColumnas, Coordenada, Valor, Nuevo).


/*
	abajoDerecha(+ListaCoordenadas, -Resultado)
		-Resultado es la coordenada más abajo a la derecha de la ListaCoordenadas
*/
abajoDerecha([[Fila, Columna]], [Fila, Columna]).
abajoDerecha([[Fila, Columna] | Resto], Coordenada) :-
    abajoDerecha(Resto, [Fila2, Columna2]),
    (Fila > Fila2 ; (Fila = Fila2, Columna > Columna2)),
    Coordenada = [Fila, Columna].
abajoDerecha([[Fila, Columna] | Resto], Coordenada) :-
    abajoDerecha(Resto, [Fila2, Columna2]),
    (Fila < Fila2 ; (Fila = Fila2, Columna < Columna2)),
    Coordenada = [Fila2, Columna2].


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
	delete(Grupos1, [], Grupos).

/*
	clustersAux(+Coordenada, +Grilla, +CantidadFilas, +CantidadColumnas, +Visitados, -Resultado)
		-Resultado es la lista de todos los clusters en la grilla empezando por la coordenada dada.
		-Mantiene una lista visitados, que actualiza para verificar las siguientes coordenadas.
*/
%Caso Base:Pasé por todas las filas.
clustersAux([CantidadFilas,_], _, CantidadFilas,_,_, []).

%Caso Recursivo: La coordenada no fue visitada y es válida.
clustersAux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, [ClusterLimpio|Resultado]):-
	not(member([Fila,Columna],Visitados)),
	coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna]),

	visitar([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],Visitados,Cluster),
	%El nuevo Cluster se marca como visitado porque todas sus coordenadas ya fueron visitadas.
	append(Visitados,Cluster,Visitados1),
	filtrarLista(Cluster, ClusterLimpio), %Si el Cluster es de una sola coordenada, lo dejamos vacío.

	ColumnaSiguiente is Columna+1,
	clustersAux([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,Visitados1,Resultado).

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
	visitar(+Coordenada, +CantidadFilas, +CantidadColumnas, +Grilla, +ListaVisitados, -Cluster)
		-Cluster es una lista de coordenadas adyacentes con el mismo valor.
		-Marca como visitados los nodos a los que se puede mover y los agrega al cluster
		-NO actualiza la lista de visitados, sino que se debe usar Append a visitados con el Cluster
		al salir de la sentencia.
*/
visitar([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs], Visitados,[[Fila,Columna]|GrupoNuevo]):-
	%Lista es una lista de coordenadas adyacentes que se pueden visitar
	puedoVisitar([X|Xs], CantidadFilas, CantidadColumnas,Visitados,[Fila,Columna],Lista),
	visitarAux(Lista,CantidadFilas,CantidadColumnas,[X|Xs], [[Fila,Columna]|Visitados], [],GrupoNuevo).


/*
	visitarAux(+ListaCoordenadas, +CantidadFilas, +CantidadColumnas, +Grilla, +Visitados, +GrupoActual,GrupoNuevo)
		- GrupoNuevo es el grupo formado por las coordenadas adyacentes del mismo valor.
		- Se inicia con GrupoActual vacío.

*/
%Caso base la lista a visitar está vacía:
visitarAux([],_,_,_,_,Grupo,Grupo).

%Caso Recursivo: el nodo no fue visitado.
visitarAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,[CoordenadaActual|GrupoNuevo]):-
	not(member(CoordenadaActual, VisitadosAntes)),
	%Primero Visito la vecindad de la CoordenadaActual y  marco como visitados 
	puedoVisitar(Grilla, CantidadFilas, CantidadColumnas,VisitadosAntes,CoordenadaActual,Lista),
	visitarAux(Lista,CantidadFilas,CantidadColumnas,Grilla, [CoordenadaActual|VisitadosAntes], Grupo,GrupoNuevo1),
	append([CoordenadaActual|VisitadosAntes],GrupoNuevo1, VisitadosNuevo),

	%Después visito el resto de la lista
	visitarAux(Resto,CantidadFilas,CantidadColumnas,Grilla,VisitadosNuevo, GrupoNuevo1, GrupoNuevo).

% Caso Recursivo: La coordenada ya fue visitada, sigo visitando las otras.
visitarAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,GrupoNuevo):-
	member(CoordenadaActual, VisitadosAntes),
	visitarAux(Resto,CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,GrupoNuevo).

/*
	puedoVisitar(+Grilla, +CantidadFilas, +CantidadColumnas, +Visitados, +Coordenada, -Resultado)
		-Resultado es la lista de las coordenadas adyacentes que puedo visitar.
		-Se evalúa si ya están visitadas, si se encuentran en un borde y si coinciden en el valor.
*/
puedoVisitar(Grilla, CantidadFilas,CantidadColumnas,Visitados,[Fila,Columna],Resultado):-
	adyacentes([Fila,Columna],Adyacentes),	
	valorEnCoordenada(Grilla, CantidadColumnas, [Fila,Columna], Valor),
	findall(
		[Fil,Col],
		(
			member([Fil,Col],Adyacentes), 
			puedoVisitarAux(Grilla, CantidadFilas, CantidadColumnas, Visitados, [Fil,Col], Valor)
		), 
		Resultado
	),!.


%Devuelve True si La coordenada no está, visitada, si no se encuentra en un borde y si coincide en el valor.
puedoVisitarAux(Grilla, CantidadFilas, CantidadColumnas, Visitados, [Fila,Columna],Valor):-
	coordenadaValida(CantidadFilas,CantidadColumnas,[Fila,Columna]),
	not(member([Fila,Columna], Visitados)),
	valorEnCoordenada(Grilla, CantidadColumnas, [Fila,Columna], Valor1),
	Valor=:=Valor1.


/*
	adyacentes(+Coordenada, -Adyacentes)
		-Adyacentes es la Lista de coordenadas adyacentes.
*/
adyacentes([Fila,Columna], Resultado):-
	Fila1 is Fila+1,
	Fila2 is Fila-1,
	Columna1 is Columna+1,
	Columna2 is Columna-1,
	Resultado=[[Fila2,Columna],[Fila2,Columna1],[Fila,Columna1],[Fila1,Columna1],[Fila1,Columna],[Fila1,Columna2],[Fila,Columna2],[Fila2,Columna2]].


/*
	coordenadaValida(+CantidadFilas,+CantidadColumnas,+Coordenada)
		-True si la coordenada es valida, False en caso contrario.
*/
coordenadaValida(CantidadFilas,CantidadColumnas,[Fila,Columna]):-
	Fila>=0,
	Columna>=0,
	Fila<CantidadFilas,
	Columna<CantidadColumnas.

/*
	filtrarLista(+Lista,NuevaLista)
		-Si la lista tiene un solo elemento, devuelve la lista vacía.
		-Se usa porque Clusters() devuelve como subconjuntos las coordenadas que tienen un solo elemento,
		que no cuentan a la hora de agrupar
*/
filtrarLista([_], []).
filtrarLista([H|T], [H|T]).


/*
 * 
 * 
 *		PROYECTO 2
 * 
 * 
 */


/*
	caminoMaximo(+Grilla, +CantidadColumnas, + CaminoMaximo)
		-CaminoMaximo es el Camino con el mayor valor posible. 
*/
movidaMaxima(Grilla, CantidadColumnas, CaminoMaximo):-
	/*
		Paso 1: Consigo la cantidad de Filas:
	*/
	length(Grilla, Size),
	CantidadFilas is Size/CantidadColumnas,

	/*
		Paso 2: Encuentro el camino más grande por cada coordenada
	*/
	encontrarCaminos([0,0], Grilla, CantidadFilas, CantidadColumnas, Caminos),

	/*
		Paso 3: Encuentro el camino más grande entre todos.
	*/
	caminoMasGrande(Grilla,CantidadColumnas,Caminos,CaminoMaximo1),

	/*
		Paso 4: Invierto el camino encontrado.
	*/
	reverse(CaminoMaximo1, CaminoMaximo).
	

/*
	CaminoMasGrande(+Grilla, +CantidadColumnas, +Grupos, -CaminoMaximo)
		-CaminoMáximo es el camino con mayor valor dentro de los Grupos.
*/
caminoMasGrande(Grilla, CantidadColumnas, Grupos, CaminoMaximo) :-
	calcularValoresCaminos(Grilla, CantidadColumnas, Grupos, ValoresCaminos),
	max_list(ValoresCaminos, ValorMaximo),
	nth1(IndiceMaximo, ValoresCaminos, ValorMaximo),
	nth1(IndiceMaximo, Grupos, CaminoMaximo).
	
/*
	calcularValoresCaminos(+Grilla, +CantidadColumnas, +Caminos, -Valores)
		-Valores es un arreglo de los valores de los caminos en el mismo orden del arreglo de caminos.
		Los valores se encuentran en el mismo índice que el arreglo original de Caminos.
*/
calcularValoresCaminos(_, _, [], []).
calcularValoresCaminos(Grilla, CantidadColumnas, [Camino|Resto], [Valor|ValoresResto]) :-
	calcularUltimoValor(Grilla, CantidadColumnas, Camino, Valor),
	calcularValoresCaminos(Grilla, CantidadColumnas, Resto,ValoresResto).


/*
	encontrarCaminos(+Coordenada, +Grilla, +CantidadFilas, +CantidadColumnas, -Resultado)
		-Resultado es la lista de todos los caminos máximos para cada coordenada. 
*/
%Caso Base:Pasé por todas las filas.
encontrarCaminos([CantidadFilas,_], _, CantidadFilas,_, []).

%Caso Recursivo: La coordenada no fue visitada y es válida.
encontrarCaminos([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, [ClusterLimpio|Resultado]):-
	coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna]),

	visitarCamino([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],Cluster),
	filtrarLista(Cluster, ClusterLimpio), %Si el Cluster es de una sola coordenada, lo dejamos vacío.

	ColumnaSiguiente is Columna+1,
	encontrarCaminos([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,Resultado),!.

% Caso Recursivo: La columna no es válida, empiezo por la siguiente fila.
encontrarCaminos([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Resultado):-
	not(coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna])),
	Fila1 is Fila+1,
	encontrarCaminos([Fila1,0],[X|Xs],CantidadFilas,CantidadColumnas, Resultado),!.


/*
	visitarCamino(+Coordenada, +CantidadFilas, +CantidadColumnas, +Grilla, +ListaVisitados, -Maximo)
		-Maximo es el camino más grande para la coordenada dada.
		-Análogo a la búsqueda de clusters, sin lista de visitados
*/
visitarCamino([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],Maximo):-
	%Lista es una lista de coordenadas adyacentes que se pueden visitar
	puedoVisitar([X|Xs], CantidadFilas, CantidadColumnas,[],[Fila,Columna],Lista),
	visitarCaminoAux(Lista,CantidadFilas,CantidadColumnas,[X|Xs],[[Fila,Columna]], [], ColeccionFinal),
	caminoMasGrande([X|Xs], CantidadColumnas, ColeccionFinal, Maximo).

/*
	visitarCaminoAux(+ListaCoordenadas, +CantidadFilas, +CantidadColumnas, +Grilla, +Visitados,GrupoNuevo)
		- Análogo a la búsqueda de Grupos, pero al final guarda el grupo como un camino aparte.
*/
%Caso base la lista a visitar está vacía:
visitarCaminoAux([],_,_,_,Grupo,Coleccion,[Grupo|Coleccion]).

%Caso Recursivo: el nodo no fue visitado.
visitarCaminoAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,Grupo,Coleccion, ColeccionFinal):-
	not(member(CoordenadaActual, Grupo)),

	% Analizo los adyacentes compatibles
	puedoVisitarCamino(Grilla, CantidadFilas, CantidadColumnas,Grupo,CoordenadaActual,Lista),

	% Visito los adyacentes.
	visitarCaminoAux(Lista,CantidadFilas,CantidadColumnas,Grilla, [CoordenadaActual|Grupo], Coleccion, ColeccionNueva1),

	% Visito los restantes.
	visitarCaminoAux(Resto,CantidadFilas,CantidadColumnas,Grilla, Grupo, ColeccionNueva1,ColeccionFinal).


% Caso Recursivo: La coordenada ya fue visitada, sigo visitando las otras.
visitarCaminoAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,Grupo, Coleccion, ColeccionFinal):-
	member(CoordenadaActual, Grupo),
	visitarCaminoAux(Resto,CantidadFilas,CantidadColumnas, Grilla,Grupo,Coleccion,ColeccionFinal),!.

/*
	puedoVisitarCamino(+Grilla, +CantidadFilas, +CantidadColumnas, +Visitados, +Coordenada, -Resultado)
		-Resultado es la lista de las coordenadas adyacentes que puedo visitar (Para el Proyecto 2).
		-Análgo a la implementación del proyecto 1, pero teniendo en cuenta el valor siguiente.
		-Se evalúa si ya están visitadas, si se encuentran en un borde y si el valor es compatible.
*/
puedoVisitarCamino(Grilla, CantidadFilas,CantidadColumnas,Grupo,[Fila,Columna],Resultado):-
	adyacentes([Fila,Columna],Adyacentes),	
	valorEnCoordenada(Grilla, CantidadColumnas, [Fila,Columna], Valor),
	findall(
		[Fil,Col],
		(
			member([Fil,Col],Adyacentes), 
			puedoVisitarCaminoAux(Grilla, CantidadFilas, CantidadColumnas, Grupo, [Fil,Col], Valor)
		), 
		Resultado
	).

%Devuelve True si La coordenada no está, visitada, si no se encuentra en un borde y si el valor es copatible.
puedoVisitarCaminoAux(Grilla, CantidadFilas, CantidadColumnas, Grupo, [Fila,Columna],Valor):-
	coordenadaValida(CantidadFilas,CantidadColumnas,[Fila,Columna]),
	not(member([Fila,Columna], Grupo)),
	valorEnCoordenada(Grilla, CantidadColumnas, [Fila,Columna], Valor1),
	(
		Valor=:=Valor1;
		(
			proximaPotenciaDe2(Valor, Siguiente),
			Valor1=:=Siguiente
		)
	),!.

/*
    proximaPotenciaDe2(+NumeroPotenciaDe2, -ProximaPotenciaDe2)
        ProximaPotenciaDe2 es la siguiente potencia de 2 mayor a NumeroPotenciaDe2.
*/
proximaPotenciaDe2(NumeroPotenciaDe2, ProximaPotenciaDe2) :-
	ProximaPotenciaDe2 is NumeroPotenciaDe2 * 2.



/*
	maximoAdyacente(+Grilla, +CantidadColumnas, -CaminoMaximo)
		-CaminoMaximo es el camino más grande que termina, post-ejecución,
		con una celda adyacente del mismo valor que el resultado.
*/
maximoAdyacente(Grilla, CantidadColumnas, CaminoMaximo):-
	/*
		Paso 1: Consigo la cantidad de Filas
	*/
	length(Grilla, Size),
	CantidadFilas is Size/CantidadColumnas,
	
	/*
		Paso 2: Consigo TODOS los caminos posibles (Incluyendo caminos parciales).
	*/
	encontrarTodosCaminos([0,0], Grilla, CantidadFilas, CantidadColumnas, Caminos),!,


	/*
		Paso 3: Consigo el máximo camino que cumpla con la condición.
	*/
	maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, Caminos ,[],CaminoMaximo).


/*
	maximoAdyacenteAux(+Grilla, +CantidadFilas, +CantidadColumnas, +Caminos, +MaximoActual, -CaminoMaximo)
		-CaminoMaximo es el camino más grande que, post-ejecución, cuente con una celda de su mismo valor.
		-Recorre linealmente los caminos y se queda con el que cumpla la condición.
*/
% Caso Base: Terminé la lista
maximoAdyacenteAux(_, _,_, [], Actual, Actual):-!.

% Caso Recursivo: El camino actual es más grande que el anterior y cumple con las condiciones.
maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, [Actual|Resto], MaximoActual, CaminoMaximo):-
	reverse(Actual,CaminoActual),
	
	% EncontrarTodosCaminos/5 devuelve también caminos de un solo elemento,
	% por lo que los descarto antes.
	length(CaminoActual,LargoActual),
	LargoActual>1,
	calcularUltimo(Grilla, CantidadColumnas, CaminoActual, ValorActual),
	calcularUltimo(Grilla, CantidadColumnas, MaximoActual, ValorMaximoActual),
	% Verifico que el actual sea más grande.
	
	ValorActual>ValorMaximoActual,
	% Ejecuto en la grilla el camino.
	joinVirtual(Grilla, CantidadColumnas, CaminoActual, GrillaProcesada, CoordenadaNueva),
	% Verifico que tenga adyacentes.
	puedoVisitar(GrillaProcesada, CantidadFilas,CantidadColumnas,[],CoordenadaNueva,Vecinos),
	length(Vecinos,CantidadVecinos),
	(CantidadVecinos=\=0),!,
	maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, Resto, CaminoActual, CaminoMaximo),!.
	
% Caso Recursivo: Actual es un camino de un solo elemento
maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, [Actual|Resto], MaximoActual,CaminoMaximo):-
	length(Actual,LargoActual),
	not(LargoActual>1),!,
	maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, Resto, MaximoActual, CaminoMaximo),!.

% Caso recursivo: El valor de Actual es menor o igual que el maximo anterior. 
maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, [CaminoActual|Resto], MaximoActual,CaminoMaximo):-
	% reverse(Actual,CaminoActual),
	calcularUltimo(Grilla, CantidadColumnas, CaminoActual, ValorActual),
	calcularUltimo(Grilla, CantidadColumnas, MaximoActual, ValorMaximoActual),
	not(ValorActual>ValorMaximoActual),!,
	maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, Resto, MaximoActual, CaminoMaximo),!.

% Caso recursivo: El camino actual no tiene vecinos post-ejecución.
maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, [Actual|Resto], MaximoActual,CaminoMaximo):-
	reverse(Actual,CaminoActual),
	joinVirtual(Grilla, CantidadColumnas, CaminoActual, GrillaProcesada, CoordenadaNueva),
	puedoVisitar(GrillaProcesada, CantidadFilas,CantidadColumnas,[],CoordenadaNueva,Vecinos),
	length(Vecinos,CantidadVecinos),
	not(CantidadVecinos=\=0),!,
	maximoAdyacenteAux(Grilla, CantidadFilas,CantidadColumnas, Resto, MaximoActual, CaminoMaximo),!.


/*
	encontrarTodosCaminos(+Coordenada, +Grilla, +CantidadFilas, +CantidadColumnas, -Resultado)
		-Resultado es la lista de todos los caminos posibles.
		-Itera sobre cada coordenada, y encuentra la colección de todos los caminos posibles.
		-Análogo a encontrarCaminos/5, pero no busca el camino mayor por coordenada.
*/
%Caso Base:Pasé por todas las filas.
encontrarTodosCaminos([CantidadFilas,_], _, CantidadFilas,_, []).
%Caso Recursivo: La coordenada no fue visitada y es válida.
encontrarTodosCaminos([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Res):-
	coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna]),

	visitarTodosCaminos([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],Cluster),
	filtrarLista(Cluster, ClusterLimpio), %Si el Cluster es de una sola coordenada, lo dejamos vacío.
	ColumnaSiguiente is Columna+1,
	encontrarTodosCaminos([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,Resultado),

	append(ClusterLimpio, Resultado, Res).
% Caso Recursivo: La columna no es válida, empiezo por la siguiente fila.
encontrarTodosCaminos([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Resultado):-
	not(coordenadaValida(CantidadFilas, CantidadColumnas,[Fila,Columna])),
	Fila1 is Fila+1,
	encontrarTodosCaminos([Fila1,0],[X|Xs],CantidadFilas,CantidadColumnas, Resultado).


/*
	visitarTodosCaminos(+Coordenada, +CantidadFilas, +CantidadColumnas, +Grilla, +ListaVisitados, -ColeccionFinal)
		-ColeccionFinal es la lista de todos los caminos posibles desde la coordenada dada.
		-Análogo a visitarCaminos, pero no devuelve el más grande sino una colección de todos.
		-Reusa visitarCaminoAux.
*/
visitarTodosCaminos([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],ColeccionFinal):-
	puedoVisitar([X|Xs], CantidadFilas, CantidadColumnas,[],[Fila,Columna],Lista),
	visitarCaminoAux(Lista,CantidadFilas,CantidadColumnas,[X|Xs],[[Fila,Columna]], [], ColeccionFinal).


/*
	joinVirtual(+Grid, +NumOFColumns, +Path, +RGrid, -CoordenadaNueva)
		-Análogo a join/4, pero tiene que tener rastro de la última coordenada del camino.
		-No reemplaza los valores en 0 después de aplicar la gravedad.
*/
joinVirtual(Grid, NumOfColumns, Path, RGrid, [NumeroFilaNuevo,Columna]):-
	length(Grid, Size),
	CantidadFilas is Size/NumOfColumns,
	
	% Consigo la grilla con los Ceros
	reemplazarCeros(Grid,NumOfColumns, Path, GridEnCero),	
	last(Path, [Fila,Columna]),

	% Calculo y reemplazo el último valor.
	calcularUltimo(Grid, NumOfColumns, Path, PrimerValor),
	reemplazarValorCoordenada(GridEnCero, NumOfColumns, [Fila,Columna], PrimerValor, GridReemplazado),
	
	% Aíslo la columna sóla y consigo a qué fila bajaría post-gravedad.
	conseguirColumna(GridReemplazado, CantidadFilas,NumOfColumns,0, Columna, ColumnaAislada),
	movimientoCoordenada(ColumnaAislada, Fila, NumeroFilaNuevo),
	
	iniciarGravedad(GridReemplazado, NumOfColumns, CantidadFilas, RGrid),!.

/*
	movimientoCoordenada(+Columna, +FilaCoordenada, -FilaCoordenadaNueva)
		- FilaCoordenadaNueva es la fila a la que va a caer la coordenada.
		- Calcula la cantidad de ceros después de la fila, que van a ser la cantidad
		de movimientos abajo.
*/
movimientoCoordenada(Columna, FilaCoordenada, FilaCoordenadaNueva):-
	cerosDespues(Columna,FilaCoordenada, Ceros),
	FilaCoordenadaNueva is FilaCoordenada + Ceros.

/*
	cerosDespues(+Columna, +Indice, -Ceros)
		-Ceros es la cantidad de ceros en el arreglo posteriores al índice (indice-0).
*/
% Caso base: Final de la lista
cerosDespues([], _, 0).
% Caso Recursivo: Avanzo hasta el índice.
cerosDespues([_|Resto], Indice, Ceros) :-
    Indice > 0,
    NuevoIndice is Indice - 1,
    cerosDespues(Resto, NuevoIndice, Ceros).
% Casos recursivos: Cuento los ceros.
cerosDespues([0|Resto], 0, Ceros) :-
    cerosDespues(Resto, 0, CerosRestantes),
    Ceros is CerosRestantes + 1.
cerosDespues([_|Resto], 0, Ceros) :-
    cerosDespues(Resto, 0, Ceros).


/*
	conseguirColumna(+Grilla, +CantidadFilas , +CantidadColumnas, +NumeroFila, +NumeroColumna, -Columna)
		Columna es la columna aislada en la grilla.
*/
% Caso Base: Llegué a la ultima fila.
conseguirColumna(_, CantidadFilas ,_,NumeroFila, _, []):-
	NumeroFila=:=CantidadFilas.
% Caso Recursivo: No llegué a la última fila.
conseguirColumna(Grilla, CantidadFilas ,CantidadColumnas,NumeroFila, NumeroColumna, [Valor|Columna]):-
	NumeroFila=\=CantidadFilas,
	valorEnCoordenada(Grilla, CantidadColumnas, [NumeroFila, NumeroColumna], Valor),
	NumeroFila1 is NumeroFila+1,
	conseguirColumna(Grilla,CantidadFilas,CantidadColumnas,NumeroFila1,NumeroColumna,Columna).