module OpcionesEventos
  ( opcionTransformacion
  , opcionAnalisisDatos
  , opcionAnalisisTemporal
  , opcionBusqueda
  , opcionEstadisticas
  ) where

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


data Evento = Evento
  { eventoId :: Int
  , categoria :: String
  , valor :: Double
  , timestamp :: Integer
  }
  deriving (Eq, Show, Read)

type EstadoEventos = [Evento]

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
