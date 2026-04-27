module OpcionesEventos
  ( Evento(..)
  , ListaEventos
  , ListaIdsUsados
  , generarCantidadEventos
  , generarFecha
  , generarEvento
  , generarNEventos
  , opcionTransformacion
  , opcionAnalisisDatos
  , opcionAnalisisTemporal
  , opcionBusqueda
  , opcionEstadisticas
  ) where

import System.Random (randomRIO)

-- =====================
-- Datos base
-- =====================

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

-- =====================
-- Generadores
-- =====================

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

generarCantidadEventos :: IO Int
generarCantidadEventos = randomRIO (10, 15)

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
  , esAltoValor :: Bool
  , impuestoAplicado :: Bool
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
  nuevoId        <- generarId usados
  nuevaCategoria <- generarCategoria
  nuevoValor     <- generarValor
  nuevaFecha     <- generarFecha
  return Evento
    { eventoId = nuevoId
    , categoria = nuevaCategoria
    , valor     = nuevoValor
    , fecha     = nuevaFecha
    , esAltoValor = False
    , impuestoAplicado = False
    }

generarNEventos :: Int -> ListaIdsUsados -> IO ListaEventos
generarNEventos 0 usados = return []
generarNEventos cantidad usados = do
  evento <- generarEvento usados
  resto  <- generarNEventos (cantidad - 1) (eventoId evento : usados)
  return (evento : resto)


extraerAño :: Evento -> Int
extraerAño evento = extraerAñoDeFecha (fecha evento)

extraerAñoDeFecha :: String -> Int
extraerAñoDeFecha fecha = read (take 4 fecha)

añosAnalisis :: [Int]
añosAnalisis = [2026, 2027, 2028]

sumarCategoriaPorAño :: [Evento] -> String -> Int -> Double
sumarCategoriaPorAño eventos categoriaBuscada añoBuscado =
  sum
    [ valor evento
    | evento <- eventos
    , categoria evento == categoriaBuscada
    , extraerAño evento == añoBuscado
    ]

sumasPorCategoriaYAño :: [Evento] -> [(String, Int, Double)]
sumasPorCategoriaYAño eventos =
  [ (categoriaActual, añoActual, sumarCategoriaPorAño eventos categoriaActual añoActual)
  | añoActual <- añosAnalisis
  , categoriaActual <- categorias
  ]

imprimirSumaCategoriaYAño :: (String, Int, Double) -> IO ()
imprimirSumaCategoriaYAño (categoriaActual, añoActual, sumaActual) =
  putStrLn (categoriaActual ++ " " ++ show añoActual ++ ": " ++ show sumaActual)
-- =====================
-- Transformacion de eventos
-- =====================

opcionTransformacion :: [Evento] -> IO ()
opcionTransformacion eventos = do
  putStrLn ""
  putStrLn "--- Transformacion de eventos ---"
  putStrLn "1) Aplicar impuesto a compras"
  putStrLn "2) Etiquetar eventos de alto valor"
  putStrLn "3) Volver"
  putStr "> "
  opcion <- getLine
  case opcion of
    "1" -> aplicarImpuesto eventos
    "2" -> etiquetarAltoValor eventos
    "3" -> return ()
    _   -> do
      putStrLn "Opcion invalida."
      opcionTransformacion eventos

aplicarImpuesto :: [Evento] -> IO ()
aplicarImpuesto eventos = do
  putStrLn "[Pendiente] Aplicar impuesto del 13% a compras"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

etiquetarAltoValor :: [Evento] -> IO ()
etiquetarAltoValor eventos = do
  putStrLn "[Pendiente] Etiquetar eventos de alto valor"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

-- =====================
-- Analisis de datos
-- =====================

opcionAnalisisDatos :: [Evento] -> IO ()
opcionAnalisisDatos eventos = do
  putStrLn ""
  putStrLn "--- Analisis de datos ---"
  putStrLn "1) Monto total"
  putStrLn "2) Promedio de monto por categoria por año"
  putStrLn "3) Volver"
  putStr "> "
  opcion <- getLine
  case opcion of
    "1" -> montoTotal eventos
    "2" -> promedioPorCategoria eventos
    "3" -> return ()
    _   -> do
      putStrLn "Opcion invalida."
      opcionAnalisisDatos eventos

montoTotal :: [Evento] -> IO ()
montoTotal eventos = do
  let totalEventos = length eventos
      sumaTotal    = sum (map valor eventos)
  putStrLn "--- Monto total ---"
  putStrLn ("Cantidad total de eventos: " ++ show totalEventos)
  putStrLn ("Suma total de montos: " ++ show sumaTotal)

promedioPorCategoria :: [Evento] -> IO ()
promedioPorCategoria eventos = do
  let resultados = sumasPorCategoriaYAño eventos
  putStrLn "--- Suma de montos por categoria y año ---"
  mapM_ imprimirSumaCategoriaYAño resultados

-- =====================
-- Analisis temporal
-- =====================

opcionAnalisisTemporal :: [Evento] -> IO ()
opcionAnalisisTemporal eventos = do
  putStrLn ""
  putStrLn "--- Analisis temporal ---"
  putStrLn "1) Mes con mayor monto y dia de la semana mas activo"
  putStrLn "2) Evento mas antiguo y mas reciente"
  putStrLn "3) Resumen de montos por intervalo"
  putStrLn "4) Volver"
  putStr "> "
  opcion <- getLine
  case opcion of
    "1" -> mesMayorMonto eventos
    "2" -> eventoAntiguoReciente eventos
    "3" -> resumenPorIntervalo eventos
    "4" -> return ()
    _   -> do
      putStrLn "Opcion invalida."
      opcionAnalisisTemporal eventos

mesMayorMonto :: [Evento] -> IO ()
mesMayorMonto eventos = do
  putStrLn "[Pendiente] Mes con mayor monto y dia mas activo"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

eventoAntiguoReciente :: [Evento] -> IO ()
eventoAntiguoReciente eventos = do
  putStrLn "[Pendiente] Evento mas antiguo y mas reciente"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

resumenPorIntervalo :: [Evento] -> IO ()
resumenPorIntervalo eventos = do
  putStrLn "[Pendiente] Resumen de montos por intervalo"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

-- =====================
-- Busqueda
-- =====================

opcionBusqueda :: [Evento] -> IO ()
opcionBusqueda eventos = do
  putStrLn "[Pendiente] Busqueda por rango de fechas"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

-- =====================
-- Estadisticas
-- =====================

opcionEstadisticas :: [Evento] -> IO ()
opcionEstadisticas eventos = do
  putStrLn ""
  putStrLn "--- Estadisticas ---"
  putStrLn "A) Resumen general"
  putStrLn "B) Volver"
  putStr "> "
  opcion <- getLine
  case opcion of
    "A" -> resumenGeneral eventos
    "a" -> resumenGeneral eventos
    "B" -> return ()
    "b" -> return ()
    _   -> do
      putStrLn "Opcion invalida."
      opcionEstadisticas eventos

resumenGeneral :: [Evento] -> IO ()
resumenGeneral eventos = do
  putStrLn ""
  putStrLn "--- Resumen general ---"
  putStrLn "[Pendiente] Cantidad de eventos por categoria"
  putStrLn "[Pendiente] Evento con monto mas alto y mas bajo"
  putStrLn "[Pendiente] Dia con mayor cantidad de eventos"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))
  putStrLn ""
  putStrLn "Exportar reporte:"
  putStrLn "1) CSV"
  putStrLn "2) JSON"
  putStrLn "3) No exportar"
  putStr "> "
  opcion <- getLine
  case opcion of
    "1" -> putStrLn "[Pendiente] Exportar a CSV"
    "2" -> putStrLn "[Pendiente] Exportar a JSON"
    "3" -> return ()
    _   -> putStrLn "Opcion invalida."
