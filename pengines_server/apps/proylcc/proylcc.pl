:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

join(Grid, NumOfColumns, Path, RGrids):-
	Path=[X|Xs],
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

menorPotenciaDe2(Numero, Resultado) :- Resultado is 2^(ceiling(log(Numero)/log(2))).


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


numeroAleatorio(N,R) :-
	random_between(0, N, X),
	R is 2^floor(log(X)/log(2)).


/*Reemplaza los valores que quedaron nulos con valores aleatorios*/
rellenar([0|Xs], [Nuevo|Resultado1]) :-
    numeroAleatorio(70, Nuevo),
    rellenar(Xs, Resultado1).
rellenar([X|Xs], [X|Resultado]) :-
    X \= 0,
    rellenar(Xs, Resultado).
rellenar([],[]).