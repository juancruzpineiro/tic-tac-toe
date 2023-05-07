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
	Path=[X|_Xs],
	reemplazarCeros(Grid,NumOfColumns, Path, GridEnCero),	
	calcularPrimero(Grid, NumOfColumns, Path, PrimerValor),
	reemplazarValorCoordenada(GridEnCero, NumOfColumns, X, PrimerValor, GridReemplazado),
	
	length(Grid, Size),
	CantidadFilas is Size/NumOfColumns,
	gravedad(GridReemplazado, NumOfColumns, CantidadFilas,0, Gravedad1),

	rellenar(Gravedad1, Final),

	RGrids = [GridEnCero, GridReemplazado, Gravedad1, Final].

reemplazarValorCoordenada([X|Xs], CantidadColumnas, Coordenada, Valor, Resultado):-

	Coordenada=[Fila,Columna],
	Posicion is (Fila * CantidadColumnas) + Columna,
	reemplazarValorCoordenadaAux([X|Xs], Valor, Posicion, 0 , Resultado).

reemplazarValorCoordenadaAux([_|Xs],Valor,Posicion,Posicion,[Valor|Xs]).
reemplazarValorCoordenadaAux([X|Xs], Valor, Posicion, Pos, [X|Resultado]):-
	Pos1 is Pos+1,
	reemplazarValorCoordenadaAux(Xs, Valor, Posicion, Pos1, Resultado).

valorEnCoordenada([X|Xs], CantidadColumnas, Coordenada, Resultado):-
	Coordenada=[Fila,Columna],
	Posicion is (Fila * CantidadColumnas) + Columna,
	valorEnCoordenadaAux([X|Xs], Posicion, 0, Resultado).

valorEnCoordenadaAux([X|_],Posicion,Posicion,X).
valorEnCoordenadaAux([_|Xs], Posicion, Pos, Resultado):-
	Posicion\=Pos,
	Pos1 is Pos+1,
	valorEnCoordenadaAux(Xs, Posicion, Pos1, Resultado).
	


calcularPrimero([X|Xs], CantidadColumnas, Camino, Resultado):-
	calcularPrimerValor([X|Xs], CantidadColumnas, Camino, Suma, 0),
	menorPotenciaDe2(Suma, Resultado).


%Calcula la suma de todos los valores en el camino.
calcularPrimerValor([], _, _, 0, _).
calcularPrimerValor([X|Xs], CantidadColumnas, Camino, Suma, Pos) :-
	Pos1 is Pos + 1,
	(
		member([Fila, Columna], Camino),
		Posicion is (Fila * CantidadColumnas) + Columna,
		Posicion =:= Pos,
		calcularPrimerValor(Xs, CantidadColumnas, Camino, Suma1, Pos1),
		Suma is Suma1 + X ; 
		%Si la posicion actual es distinta a la coordenada, paso sin hacer la suma.
		calcularPrimerValor(Xs, CantidadColumnas, Camino, Suma, Pos1)
	).

menorPotenciaDe2(Numero, Resultado) :- 
	Resultado is 2^(floor(log(Numero)/log(2))+1).


% Reemplaza todos los valores en del camino especificado por ceros.
reemplazarCeros(Lista, CantidadColumnas, Camino, Resultado) :-
    reemplazarCerosAux(Lista, CantidadColumnas, Camino, 0, Resultado).
% Auxiliar para reemplazar los valores en las Camino dadas por ceros.
% Se usa porque es necesario Saber la posición.
reemplazarCerosAux([], _, _, _, []).
reemplazarCerosAux([_|Xs], CantidadColumnas, Camino, Pos, [0|Ys]) :-
    member([Fila, Columna], Camino), %Aseguro que las Coordenadas estén bien formateadas.
    Posicion is (Fila * CantidadColumnas) + Columna, 
    Posicion =:= Pos, %¿Verifico estar en la misma posicion de[X|Xs] la corrdenada.
    Pos1 is Pos + 1,
    reemplazarCerosAux(Xs, CantidadColumnas, Camino, Pos1, Ys).
reemplazarCerosAux([X|Xs], CantidadColumnas, Camino, Pos, [X|Ys]) :-
    Pos1 is Pos + 1,
    reemplazarCerosAux(Xs, CantidadColumnas, Camino, Pos1, Ys).

/*Reemplza los ceros por el valor de la celda de arriba, hasta la primera fila*/
% gravedad([],CantidadColumnas,Camino,[]).

gravedad([X|Xs], CantidadColumnas,CantidadFilas,Contador, Resultado):-

	Contador\=CantidadFilas,
	gravedadAux([X|Xs], CantidadColumnas, CantidadFilas, [1,0], Resultado1),
	Contador1 is Contador+1,
	gravedad(Resultado1, CantidadColumnas,CantidadFilas, Contador1, Resultado).

gravedad([X|Xs], _CantidadColumnas,CantidadFilas,CantidadFilas, [X|Xs]).

gravedadAux(Grilla, CantidadColumnas, CantidadFilas, [Fila, Columna], Resultado):-
	Columna<CantidadColumnas,
	Fila>0,
	FilaArriba is Fila-1,
	
	valorEnCoordenada(Grilla, CantidadColumnas,[Fila,Columna], Valor),
	Valor=:=0,
	valorEnCoordenada(Grilla, CantidadColumnas, [FilaArriba, Columna], ValorArriba),
	
	reemplazarValorCoordenada(Grilla,CantidadColumnas, [Fila,Columna], ValorArriba, Grilla1),
	reemplazarValorCoordenada(Grilla1,CantidadColumnas, [FilaArriba, Columna], 0, Grilla2),

	ColumnaSiguiente is Columna+1,
	gravedadAux(Grilla2, CantidadColumnas,CantidadFilas, [Fila,ColumnaSiguiente], Resultado).

gravedadAux(Grilla, CantidadColumnas, CantidadFilas, [Fila, Columna], Resultado):-
	Columna<CantidadColumnas,
	Fila>0,
	
	valorEnCoordenada(Grilla, CantidadColumnas,[Fila,Columna], Valor),
	Valor\=0,
	

	ColumnaSiguiente is Columna+1,
	gravedadAux(Grilla, CantidadColumnas,CantidadFilas, [Fila,ColumnaSiguiente], Resultado).

gravedadAux(Grilla, CantidadColumnas, CantidadFilas, [Fila, CantidadColumnas], Resultado):-
	Fila1 is Fila+1,
	gravedadAux(Grilla,CantidadColumnas,CantidadFilas,[Fila1,0], Resultado).
gravedadAux(Grilla, _CantidadColumnas, CantidadFilas, [CantidadFilas, _Columna], Grilla).
gravedadAux(Grilla, CantidadColumnas, CantidadFilas, [CantidadFilas, CantidadColumnas], Grilla).


numeroAleatorio(P) :-
    random_between(1, 6, Exp),
    P is 2^Exp.


/*Reemplaza los valores que quedaron nulos con valores aleatorios*/
rellenar([0|Xs], [Nuevo|Resultado1]) :-
    numeroAleatorio(Nuevo),
    rellenar(Xs, Resultado1).
rellenar([X|Xs], [X|Resultado]) :-
    X \= 0,
    rellenar(Xs, Resultado).
rellenar([],[]).

/**/
powerUp(Grid, CantidadColumnas, RGrids):-
	length(Grid, Size),
	CantidadFilas is Size/CantidadColumnas,

	clusters(Grid,CantidadFilas,CantidadColumnas,Grupos),
	ceros(Grupos, Grid,CantidadColumnas, GridEnCero),
	calculo(Grupos,Grid,GridEnCero,CantidadColumnas,Nuevo),

	gravedad(Nuevo, CantidadColumnas,CantidadFilas,0,Resultado1),
	rellenar(Resultado1, ResultadoDefinitivo),

	RGrids=[GridEnCero,Nuevo,Resultado1, ResultadoDefinitivo].

ceros([ListaActual|Resto], Grid,CantidadColumnas, GridEnCero):-
	reemplazarCeros(Grid,CantidadColumnas,ListaActual,GridNuevo),
	ceros(Resto, GridNuevo,CantidadColumnas,GridEnCero).

ceros([],Grid,_,Grid).


calculo([ListaActual|Resto], Grid, GridEnCero , CantidadColumnas, Resultado):-
	calcularPrimero(Grid, CantidadColumnas, ListaActual, Valor),
	abajoDerecha(ListaActual, Coordenada),
	reemplazarValorCoordenada(GridEnCero, CantidadColumnas, Coordenada, Valor, GridNuevo),
	calculo(Resto, Grid, GridNuevo, CantidadColumnas, Resultado).

calculo([],_,GridEnCero,_,GridEnCero).


abajoDerecha([[Fila,Columna]], [Fila,Columna]).
abajoDerecha([[Fila,Columna]|Resto], Coordenada) :-
    abajoDerecha(Resto, [Fila2,Columna2]),
    Fila >= Fila2,
    Columna >= Columna2,
    Coordenada = [Fila,Columna].
abajoDerecha([[Fila,Columna]|Resto], Coordenada) :-
    abajoDerecha(Resto, [Fila2,Columna2]),
    (Fila < Fila2 ; Columna < Columna2),
    Coordenada = [Fila2,Columna2].


clusters([],_,_,[]).
clusters([X|Xs],CantidadFilas, CantidadColumnas, Grupos):-
	clusters_aux([0,0], [X|Xs],CantidadFilas,CantidadColumnas,[], Grupos1),
	limpiar_lista(Grupos1, Grupos).


filtrar_lista([_], []).
filtrar_lista([H|T], [H|T]).

limpiar_lista([], []).
limpiar_lista([[]|T], Resultado) :-
    limpiar_lista(T, Resultado).
limpiar_lista([H|T], [H|Resultado]) :-
    H \= [],
    limpiar_lista(T, Resultado).


clusters_aux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, [Lista2|Resultado]):-
	not(member([Fila,Columna],Visitados)),
	Columna<CantidadColumnas,
	Fila>=0,
	Fila<CantidadFilas,
	
	visitar([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs],Visitados,Lista1),
	append(Visitados,Lista1,Visitados1),
	eliminar_repetidos(Visitados1, VisitadosNuevo),
	
	filtrar_lista(Lista1, Lista2),

	ColumnaSiguiente is Columna+1,
	clusters_aux([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,VisitadosNuevo,Resultado).





clusters_aux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, Resultado):-
	member([Fila,Columna],Visitados),
	Columna<CantidadColumnas,
	Fila>=0,
	Fila<CantidadFilas,
	
	ColumnaSiguiente is Columna+1,
	clusters_aux([Fila,ColumnaSiguiente],[X|Xs],CantidadFilas,CantidadColumnas,Visitados,Resultado).



clusters_aux([Fila,Columna],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, Resultado):-
	Columna>=CantidadColumnas,
	Fila1 is Fila+1,
	clusters_aux([Fila1,0],[X|Xs],CantidadFilas,CantidadColumnas, Visitados, Resultado).


clusters_aux([Fila,_], _, CantidadFilas,_,_, []):-
	Fila=:=CantidadFilas.



	

%HAY QUE ACTUALIZAR LA LISTA DE VISITADOS DESDE EL CLIENTE, CON APPEND/3
visitar([Fila,Columna], CantidadFilas,CantidadColumnas,[X|Xs], Visitados,[[Fila,Columna]|GrupoNuevo]):-
	puedo_mover([X|Xs], CantidadFilas, CantidadColumnas,Visitados,[Fila,Columna],Lista),
	
	visitarAux(Lista,CantidadFilas,CantidadColumnas,[X|Xs], [[Fila,Columna]|Visitados], [],GrupoNuevo).




visitarAux([],_,_,_,_,Grupo,Grupo).

%Caso el nodo no fue visitado.
visitarAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,[CoordenadaActual|GrupoNuevo]):-
	not(member(CoordenadaActual, VisitadosAntes)),
	
	puedo_mover(Grilla, CantidadFilas, CantidadColumnas,VisitadosAntes,CoordenadaActual,Lista),
	visitarAux(Lista,CantidadFilas,CantidadColumnas,Grilla, [CoordenadaActual|VisitadosAntes], Grupo,GrupoNuevo1),
	
	append([CoordenadaActual|VisitadosAntes],GrupoNuevo1, VisitadosNuevo),

	visitarAux(Resto,CantidadFilas,CantidadColumnas,Grilla,VisitadosNuevo, GrupoNuevo1, GrupoNuevo).

visitarAux([CoordenadaActual|Resto],CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,GrupoNuevo):-
	member(CoordenadaActual, VisitadosAntes),
	visitarAux(Resto,CantidadFilas,CantidadColumnas, Grilla,VisitadosAntes,Grupo,GrupoNuevo).

puedo_mover([H|T], CantidadFilas,CantidadColumnas,Visitados,[Fila,Columna],Resultado):-
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

	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, Norte, Valor, Resultado0),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, Sur, Valor, Resultado1),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, Izquierda, Valor, Resultado2),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, Derecha, Valor, Resultado3),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, DNI, Valor,Resultado4),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, DND, Valor,Resultado5),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, DSD, Valor,Resultado6),
	puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, DSI, Valor,ResultadoFinal),
	append([Resultado0,Resultado1,Resultado2,Resultado3,Resultado4,Resultado5,Resultado6,ResultadoFinal], Resultado).


puedo_mover_aux([H|T], CantidadFilas, CantidadColumnas, Visitados, [Fila,Columna],Valor, [[Fila,Columna]]):-
	coordenada_valida(CantidadFilas,CantidadColumnas,[Fila,Columna]),
	not(member([Fila,Columna], Visitados)),
	valorEnCoordenada([H|T], CantidadColumnas, [Fila,Columna], Valor1),
	Valor=:=Valor1.

puedo_mover_aux(_, CantidadFilas, CantidadColumnas, _Visitados, [Fila,Columna],_Valor, []):-
	not(coordenada_valida(CantidadFilas,CantidadColumnas,[Fila,Columna])).

puedo_mover_aux(_, _CantidadFilas, _CantidadColumnas, Visitados, [Fila,Columna],_Valor, []):-
 	member([Fila,Columna], Visitados).

puedo_mover_aux([H|T], _CantidadFilas, CantidadColumnas, _Visitados, [Fila,Columna],Valor, []):-
	valorEnCoordenada([H|T], CantidadColumnas, [Fila,Columna], Valor1),
	Valor=\=Valor1.


coordenada_valida(CantidadFilas,CantidadColumnas,[Fila,Columna]):-
	Fila>=0,
	Columna>=0,
	Fila<CantidadFilas,
	Columna<CantidadColumnas.


eliminar_repetidos([], []).
eliminar_repetidos([H|T], Resultado) :-
	member(H, T), eliminar_repetidos(T, Resultado).
eliminar_repetidos([H|T], [H|Resultado]) :-
	eliminar_repetidos(T, Resultado).

	
	
	



