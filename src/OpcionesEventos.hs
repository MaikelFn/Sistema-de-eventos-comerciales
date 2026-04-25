module OpcionesEventos
  ( Evento(..)
  , ListaIdsUsados
  , generarFecha
  , generarEvento
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
generarFecha  = do
  año <- generarAño
  mes <- generarMes
  dia <- generarDia
  return (show año ++ "-" ++  show mes ++ "-" ++  show dia)

data Evento = Evento
  { eventoId :: Int
  , categoria :: String
  , valor :: Double
  , fecha :: String
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

generarEvento :: ListaIdsUsados -> IO Evento
generarEvento usados = do
  nuevoId <- generarId usados
  nuevaCategoria <- generarCategoria
  nuevoValor <- generarValor
  nuevaFecha <- generarFecha
  return
    Evento
      { eventoId = nuevoId
      , categoria = nuevaCategoria
      , valor = nuevoValor
      , fecha = nuevaFecha
      }

opcionTransformacion :: IO ()
opcionTransformacion = do
  evento <- generarEvento []
  putStrLn "Evento generado:"
  putStrLn ("ID: " ++ show (eventoId evento))
  putStrLn ("Categoria: " ++ categoria evento)
  putStrLn ("Valor: " ++ show (valor evento))
  putStrLn ("Fecha: " ++ fecha evento)

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
