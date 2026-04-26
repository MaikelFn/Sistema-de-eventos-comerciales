module OpcionesEventos
  ( Evento(..)
  , ListaIdsUsados
  , generarFecha
  , generarEvento
  , eventoId
  , opcionTransformacion
  , opcionAnalisisDatos
  , opcionAnalisisTemporal
  , opcionBusqueda
  , opcionEstadisticas
  ) where

import System.Random (randomRIO)

categorias :: [String]
categorias =
    [ "Visualizacion"
    , "Apartado"
    , "Compra"
    , "Devolucion"
    , "Seguimiento"
    ]

impuestoCompra :: Double
impuestoCompra = 0.13

generarValor :: IO Double
generarValor = randomRIO (500, 75000)

generarCategoria :: IO String
generarCategoria = do
  indice <- randomRIO (0, length categorias - 1)
  return (categorias !! indice)

generarAño :: IO Int
generarAño = randomRIO (2026, 2028)

generarMes :: IO Int
generarMes = randomRIO (1, 12)

generarDia :: IO Int
generarDia = randomRIO (1, 31)

generarFecha :: IO String
generarFecha = do
  año <- generarAño
  mes <- generarMes
  dia <- generarDia
  return (show año ++ "-" ++ show mes ++ "-" ++ show dia)

data Evento = Evento
  { eventoId :: Int
  , categoria :: String
  , valor     :: Double
  , fecha     :: String
  } deriving (Eq, Show, Read)

type ListaEventos = [Evento]

type ListaIdsUsados = [Int]

generarId :: ListaIdsUsados -> IO Int
generarId usados = do
  candidato <- randomRIO (0, 9000000)
  if candidato `elem` usados
    then generarId usados
    else return candidato

generarEvento :: ListaIdsUsados -> IO Evento
generarEvento usados = do
  nuevoId    <- generarId usados
  nuevaCategoria <- generarCategoria
  nuevoValor <- generarValor
  nuevaFecha <- generarFecha
  return Evento
    { eventoId = nuevoId
    , categoria = nuevaCategoria
    , valor     = nuevoValor
    , fecha     = nuevaFecha
    }

opcionTransformacion :: [Evento] -> IO ()
opcionTransformacion eventos = do
  putStrLn "[Pendiente] Transformacion de eventos"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

opcionAnalisisDatos :: [Evento] -> IO ()
opcionAnalisisDatos eventos = do
  putStrLn "[Pendiente] Analisis de datos"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

opcionAnalisisTemporal :: [Evento] -> IO ()
opcionAnalisisTemporal eventos = do
  putStrLn "[Pendiente] Analisis temporal"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

opcionBusqueda :: [Evento] -> IO ()
opcionBusqueda eventos = do
  putStrLn "[Pendiente] Busqueda por rango de fechas"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

opcionEstadisticas :: [Evento] -> IO ()
opcionEstadisticas eventos = do
  putStrLn "[Pendiente] Estadisticas"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))
