-----------------------------------------------------------------------
-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Gabriel Szwarcberg 41009532
-- INTEGRANTE 2: Ignacio Ezequiel Arce - 556/14
-- INTEGRANTE 3: Cristian Villafanez - 319/14
-----------------------------------------------------------------------

data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Conjunto a = [a]
type Camino = [Desplazamiento]
type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type CampoMinado = Tablero Bool
type TableroAF = Tablero Desplazamiento

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j

-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t

-- Funciones de ejemplo, solo para ilustrar cómo usar los tipos definidos arriba.
-- Determina si un desplazamiento es vertical (Arriba o Abajo).
esVertical :: Desplazamiento -> Bool
esVertical Arriba = True
esVertical Abajo = True
esVertical _ = False

-- Cuenta la cantidad de Desplazamientos verticales en un Camino.
contarDesplazamientosVerticales :: Camino -> Integer
contarDesplazamientosVerticales [] = 0
contarDesplazamientosVerticales (x:xs) | esVertical x = 1 + resto
                                       | otherwise    = resto
  where resto = contarDesplazamientosVerticales xs

-- Caminos de prueba.
camino1 = [Derecha, Abajo, Izquierda, Arriba, Abajo, Abajo, Derecha, Derecha]
camino2 = [Derecha, Abajo, Derecha, Abajo]
camino3 = [Derecha, Abajo, Derecha, Izquierda, Derecha, Abajo]
camino4 = [Derecha, Abajo, Derecha, Derecha, Derecha, Derecha,Derecha, Derecha]
camino5 = [Derecha, Abajo, Derecha, Arriba]



-- CampoMinado de prueba.
campo1 :: CampoMinado
campo1 = [ [False, False, True],
           [True,  False, False],
           [True,  True,  False] ]

-- TableroAF de prueba, sin ciclos.
taf1 :: TableroAF
taf1 = [ [Derecha,  Derecha, Abajo],
         [Arriba, Izquierda, Abajo],
         [Arriba, Izquierda, Abajo] ]

-- TableroAF de prueba, con ciclos.
taf2 :: TableroAF
taf2 = [ [Derecha,       Abajo, Abajo],
         [Arriba,    Izquierda, Abajo],
         [Izquierda, Izquierda, Izquierda] ]
taf3 :: TableroAF
taf3 = [ [Derecha,Derecha,Derecha,Abajo],
         [Arriba, Abajo, Arriba,  Abajo],
         [Arriba,  Abajo, Arriba, Abajo],
         [Arriba,Izquierda,Arriba,Izquierda] ]



-----------------------------------------------------------------------
-- Parte A. Campos minados
-----------------------------------------------------------------------

-- Determina si un camino se mantiene dentro de los límites del tablero a lo largo de su trayectoria,
-- Asumiendo que se comenzará por la posición (1, 1).
caminoValido :: Tablero a -> Camino -> Bool
caminoValido _ [] = True
caminoValido t (x:xs) = auxMov t (1,1) (x:xs)

-- Recibe un tablero, un camino por recorrer, una posicion inicial y devuelve un booleano.
-- Evalua las posiciones validas a medida que recorre el tablero.
auxMov :: Tablero a -> Posicion -> Camino -> Bool
auxMov t _ [] = True
auxMov t (i,j) (x:xs) = posValida t (posicion) && auxMov t (posicion) xs
    where posicion = (movimiento (i,j) x)

-- Segun el desplazamiento altera la posicion, es decir, se desplaza desde una posicion hacia otra.
movimiento :: Posicion -> Desplazamiento -> Posicion
movimiento (i,j) Arriba = (i-1,j)
movimiento (i,j) Abajo = (i+1,j)
movimiento (i,j) Izquierda = (i,j-1)
movimiento (i,j) Derecha = (i,j+1)

-- Determina si un RAE, comenzando en la posición (1, 1), al seguir el camino dado, llega a la posición
-- (n, n) sin pisar ninguna mina.
---------------------------------------------------------------------------------------------

-- Ve si el camino es valido, si lo es ve si hay una mina en ese camino.
caminoDeSalida :: CampoMinado -> Camino -> Bool
caminoDeSalida _ [] = True
caminoDeSalida (x:xs) (y:ys) = caminoValido (x:xs) (y:ys) && buscaMina (x:xs) (y:ys) (1,1)

-- Recorre el camino y se fija si hay una bomba (Campo Minado -> Camino -> Posicion INICIAL)
buscaMina :: CampoMinado -> Camino -> Posicion -> Bool
buscaMina (x:xs) [] (i,j) = (tamano (x:xs) == i) && (tamano (x:xs) == j)
buscaMina (x:xs) (y:ys) (i,j) = (valor (x:xs) (i,j) /= True && buscaMina (x:xs) (ys) (movimiento (i,j) y))


--Determina si un RAE, comenzando en la posición (1; 1), al seguir el camino dado, llega a la posición
--(n; n) sin pisar ninguna mina y sin pasar dos veces por una misma posición.
---------------------------------------------------------------------------------------------

--caminoDeSalidaSinRepetidos busca que se cumpla la condicion de que no hayan minas en el camino.  
caminoDeSalidaSinRepetidos :: CampoMinado -> Camino -> Bool
caminoDeSalidaSinRepetidos cs xs
    | caminoDeSalida cs xs == False = False
    | otherwise = sinPosicionesRepetidas (listaDePosiciones xs (1,1))
    

--Si esto se comprueba, evalua entonces las posiciones
--Para eso creamos una lista de posiciones:

listaDePosiciones :: Camino -> Posicion -> Conjunto Posicion
listaDePosiciones [] (i,j)= [(i,j)]
listaDePosiciones (x:xs) (i,j) = (i,j):listaDePosiciones xs proximaPosicion
    where proximaPosicion = movimiento (i,j) x

--Si hay repeticiones, la funcion caminoDeSalidaSinRepetidos debe devolver False
--Entonces usamos la funcion siguente:

sinPosicionesRepetidas :: Conjunto Posicion -> Bool
sinPosicionesRepetidas xs
    | hayPosicionesRepetidas xs == True = False
    | otherwise = True

--Que cambia el valor Bool de la funcion que comprueba las repeticiones:    

hayPosicionesRepetidas :: Conjunto Posicion -> Bool
hayPosicionesRepetidas [] = False
hayPosicionesRepetidas (x:xs)
    | elem x xs = True
    | otherwise = hayPosicionesRepetidas xs

-- Dados un campo minado y un número natural k, devuelve el conjunto de todos los caminos de
-- longitud k que lleven a un RAE desde (1, 1) hasta (n, n), sin pisar ninguna mina.
---------------------------------------------------------------------------------------------
salidasEnKDesp :: CampoMinado -> Integer -> Conjunto Camino
salidasEnKDesp _ 0 = []
salidasEnKDesp cm k = salidasCorrectas cm (movimientoPosible [Arriba,Abajo,Izquierda,Derecha] k)

-- Se fija cual de los caminos posibles son validos y terminan en (n,n), si son validos entonces los mete a una lista.
salidasCorrectas :: CampoMinado -> Conjunto Camino -> Conjunto Camino
salidasCorrectas cm [] = []
salidasCorrectas cm (x:xs)
    | caminoDeSalida cm x == True = (x:salidasCorrectas cm xs)
    | otherwise = (salidasCorrectas cm xs)

-- Crea una lista de desplazamientos de de largo l con todos los movimientos posibles
movimientoPosible :: Camino -> Integer -> Conjunto Camino
movimientoPosible _ 0 = [[]]
movimientoPosible xs l = agregarCamino xs (movimientoPosible xs (l-1))

-- A un camino dado lo introduce en un conjunto de caminos
agregarCamino :: Camino -> Conjunto Camino -> Conjunto Camino
agregarCamino [] _ = []
agregarCamino (x:xs) ys = (agregarDesplazamiento x ys) ++ (agregarCamino xs ys)

-- Agrega un desplazamiento a un Conjunto Camino
agregarDesplazamiento :: Desplazamiento -> Conjunto Camino -> Conjunto Camino
agregarDesplazamiento _ [] = []
agregarDesplazamiento d (x:xs) = (d:x) : (agregarDesplazamiento d xs)

-----------------------------------------------------------------------
-- Parte B. Siga la Flecha
-----------------------------------------------------------------------
---------------TABLEROS ESTÁTICOS

-- Dado un tablero y una posición p, devuelve una lista que contiene las posiciones por las que pasará
-- un AF si se lo coloca inicialmente sobre p. Tener en cuenta que puede tratarse de una lista infinita,
-- cuya head es p.

recorrido :: TableroAF -> Posicion -> [Posicion]
recorrido [] (i,j)= [(i,j)]
recorrido xs (i,j)
    | posValida xs (i,j) == False = []
    | otherwise = (i,j):recorrido xs proximaPosicion
    where proximaPosicion = movimiento (i,j) (valor xs (i,j))

--Dado un tablero y una posición p, determina si al colocar un AF en p, el AF escapará del tablero o
--entrará en un loop infinito.

escapaDelTablero :: TableroAF -> Posicion -> Bool
escapaDelTablero xs (x,y) = escapaAux (recorrido xs (x,y)) 1

--En el caso de los tableros estáticos, un AF repite una posición si y solo si entra en un loop

escapaAux :: [Posicion] -> Integer -> Bool
escapaAux [] _ = True
escapaAux xs i
| length (quitar i xs) == 0 = True
| elem (head (quitar i xs)) (agarrar i xs) == True = False
| otherwise = escapaAux xs (i+1)

---------------TABLEROS DINÁMICOS

-- Dado un tablero y una posición p, devuelve cuántas veces tiene que desplazarse un AF para escapar
-- del tablero si inicialmente lo colocamos en p. Esto incluye al último desplazamiento.

cantidadDePasosParaSalir :: TableroAF -> Posicion -> Integer
cantidadDePasosParaSalir ts (x,y) = contador (recorridoDinamico ts (x,y))

-- Función contador para contar las posiciones del recorrido: posiciones = pasos

contador :: [Posicion] -> Integer
contador [] = 0
contador (x:xs) = 1+(contador xs)

-- El recorrido dinámico modifica el tablero cada posición que pasa

recorridoDinamico :: TableroAF -> Posicion -> [Posicion]
recorridoDinamico [] (i,j)= [(i,j)]
recorridoDinamico xs (i,j)
    | posValida xs (i,j) == False = []
    | otherwise = (i,j):recorridoDinamico (nuevoTablero xs (i,j)) proximaPosicion where proximaPosicion = movimiento (i,j) (valor xs (i,j))
    
-- Para esto hay que modificar el elemento de la posición en la lista que representa al tablero
    
nuevoTablero :: TableroAF -> Posicion -> TableroAF
nuevoTablero xs (x,y) = (agarrar (x-1) xs)++ auxTablero (quitar (x-1) xs) y

auxTablero :: TableroAF -> Integer -> TableroAF
auxTablero (t:ts) n =(rotacion t n):ts

-- Las funciones de arriba separan la lista para trabajar con el desplazamiento buscado

rotacion :: Camino -> Integer -> Camino
rotacion cs n = agarrar (n-1) cs ++ auxRotacion (quitar (n-1) cs)

auxRotacion :: Camino -> Camino
auxRotacion (c:cs) = (cambio c):cs

quitar :: Integer -> [a] -> [a]
quitar _ [] = []
quitar 0 (x:xs) = (x:xs)
quitar 1 (x:xs) = xs
quitar n (x:xs) = quitar (n-1) xs

agarrar :: Integer -> [a] -> [a]
agarrar _ [] = []
agarrar 0 (x:xs) = []
agarrar 1 (x:xs) = [x]
agarrar n (x:xs) = [x]++agarrar (n-1) xs

-- Cambia las flechas como las agujas del reloj

cambio :: Desplazamiento -> Desplazamiento
cambio Arriba = Derecha
cambio Derecha = Abajo
cambio Abajo = Izquierda
cambio Izquierda = Arriba
