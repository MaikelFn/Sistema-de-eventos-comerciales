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
import Data.List (nub, sortOn)

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
impuestoCompra = 1.13

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

sumarCategoria :: [Evento] -> String -> Double
sumarCategoria eventos categoriaBuscada =
  sum
    [ valor evento
    | evento <- eventos
    , categoria evento == categoriaBuscada
    ]

contarCategoria :: [Evento] -> String -> Int
contarCategoria eventos categoriaBuscada =
  length
    [ evento
    | evento <- eventos
    , categoria evento == categoriaBuscada
    ]

promedioCategoria :: [Evento] -> String -> (String, Double)
promedioCategoria eventos categoriaBuscada =
  let sumaTotal = sumarCategoria eventos categoriaBuscada
      cantidad  = contarCategoria eventos categoriaBuscada
      promedio  = sumaTotal / fromIntegral cantidad
  in (categoriaBuscada, promedio)

promediosPorCategoria :: [Evento] -> [(String, Double)]
promediosPorCategoria eventos =
  [ promedioCategoria eventos categoriaActual
  | categoriaActual <- categorias
  ]

-- =====================
-- Estadisticas: cantidad por categoria
-- =====================

cantidadEventosPorCategoria :: [Evento] -> [(String, Int)]
cantidadEventosPorCategoria eventos =
  [ (cat, length [ evento | evento <- eventos, categoria evento == cat ])
  | cat <- categorias
  ]

imprimirCantidadEvento :: (String, Int) -> IO ()
imprimirCantidadEvento (categoriaStr, numero) =
  putStrLn (categoriaStr ++ ": " ++ show numero)

-- Obtener evento con monto maximo y minimo (asume lista no vacia)
eventoMaxMin :: [Evento] -> (Evento, Evento)
eventoMaxMin eventos =
  let eventosOrdenados = sortOn valor eventos
      minimo = head eventosOrdenados
      maximo = last eventosOrdenados
  in (maximo, minimo)

-- Imprime un evento en formato legible
imprimirEvento :: Evento -> IO ()
imprimirEvento evento = putStrLn $
  "ID: " ++ show (eventoId evento) ++ " | Categoria: " ++ categoria evento ++
  " | Valor: " ++ show (valor evento) ++ " | Fecha: " ++ fecha evento

imprimirMaxMinEventos :: [Evento] -> IO ()
imprimirMaxMinEventos eventos =
  let (maxE, minE) = eventoMaxMin eventos
  in do
    putStrLn "--- Evento con monto maximo ---"
    imprimirEvento maxE
    putStrLn "--- Evento con monto minimo ---"
    imprimirEvento minE

asignarAltoValor :: [Evento] -> [(String, Double)] -> [Evento]
asignarAltoValor eventos promedios =
  [ evento { esAltoValor = valor evento > promedio }
  | (cat, promedio) <- promedios
  , evento <- eventos
  , categoria evento == cat
  ]

aplicarImpuestoEventos :: [Evento] -> [Evento]
aplicarImpuestoEventos eventos =
  [ if categoria evento == "Compra" && not (impuestoAplicado evento)
      then evento
        { valor = valor evento * impuestoCompra
        , impuestoAplicado = True
        }
      else evento
  | evento <- eventos
  ]

imprimirComprasConImpuesto :: [Evento] -> IO ()
imprimirComprasConImpuesto eventos = do
  putStrLn "--- Eventos de compra con impuesto aplicado ---"
  mapM_ print [ evento | evento <- eventos, impuestoAplicado evento ]


imprimirAltosValores :: [Evento] -> IO ()
imprimirAltosValores eventos = do
  putStrLn "--- Eventos de alto valor ---"
  mapM_ print [ evento | evento <- eventos, esAltoValor evento ]
-- =====================
-- Transformacion de eventos
-- =====================

opcionTransformacion :: [Evento] -> IO [Evento]
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
    "3" -> return eventos
    _   -> do
      putStrLn "Opcion invalida."
      opcionTransformacion eventos

aplicarImpuesto :: [Evento] -> IO [Evento]
aplicarImpuesto eventos = do
  let eventosActualizados = aplicarImpuestoEventos eventos
  imprimirComprasConImpuesto eventosActualizados
  return eventosActualizados

etiquetarAltoValor :: [Evento] -> IO [Evento]
etiquetarAltoValor eventos = do
  let promedios = promediosPorCategoria eventos
  let eventosActualizados = asignarAltoValor eventos promedios
  imprimirAltosValores eventosActualizados
  return eventosActualizados 
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

fechaConMasEventos :: [Evento] -> (String, Int)
fechaConMasEventos eventos =
  let fechasRegistradas = map fecha eventos
      fechasSinRepetir = nub fechasRegistradas
      conteosPorFecha =
        [ (fechaActual
        , length [eventoActual
        | eventoActual <- eventos
        , fecha eventoActual == fechaActual])
        | fechaActual <- fechasSinRepetir
        ]
      conteosOrdenados = sortOn snd conteosPorFecha
  in last conteosOrdenados

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
  putStrLn "1) Resumen general"
  putStrLn "2) Volver"
  putStr "> "
  opcion <- getLine
  case opcion of
    "1" -> resumenGeneral eventos
    "2" -> return ()
    _   -> do
      putStrLn "Opcion invalida."
      opcionEstadisticas eventos

resumenGeneral :: [Evento] -> IO ()
resumenGeneral eventos = do
  putStrLn ""
  putStrLn "--- Resumen general ---"
  let conteos = cantidadEventosPorCategoria eventos
      (fechaMayorActividad, cantidadMayorActividad) = fechaConMasEventos eventos
  putStrLn "--- Cantidad de eventos por categoria ---"
  mapM_ imprimirCantidadEvento conteos
  imprimirMaxMinEventos eventos
  putStrLn "--- Dia con mayor cantidad de eventos ---"
  putStrLn ("Fecha: " ++ fechaMayorActividad)
  putStrLn ("Cantidad de eventos: " ++ show cantidadMayorActividad)
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
