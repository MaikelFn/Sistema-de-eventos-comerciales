module OpcionesEventos
  ( opcionTransformacion
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


data Evento = Evento
  { eventoId :: Int
  , categoria :: String
  , valor :: Double
  , timestamp :: Integer
  }
  deriving (Eq, Show, Read)

type ListaEventos = [Evento]

type ListaIdsUsados = [Int]

generarId :: ListaIdsUsados -> IO Int
generarId usados = do
  candidato <- randomRIO (0, 9000000)
  if candidato `elem` usados
    then generarId usados
    else return candidato

opcionTransformacion :: IO ()
opcionTransformacion = do
  putStrLn "[Pendiente] Transformacion de eventos"

opcionAnalisisDatos :: IO ()
opcionAnalisisDatos = do
  putStrLn "[Pendiente] Analisis de datos"

opcionAnalisisTemporal :: IO ()
opcionAnalisisTemporal = do
  putStrLn "[Pendiente] Analisis temporal"

opcionBusqueda :: IO ()
opcionBusqueda = do
  putStrLn "[Pendiente] Busqueda por rango de fechas"

opcionEstadisticas :: IO ()
opcionEstadisticas = do
  putStrLn "[Pendiente] Estadisticas"
