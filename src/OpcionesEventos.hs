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
import Data.Time.Calendar (toGregorian, Day)
import Data.Time.Clock (addUTCTime, getCurrentTime, utctDay)

añosAnalisis :: IO [Int]
añosAnalisis = do
  hoy <- getCurrentTime
  let (año, mes, dia) = toGregorian (utctDay hoy)
  return [fromInteger año .. fromInteger año + 2]

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

generarValor :: IO Double
generarValor = randomRIO (500, 75000)

generarCategoria :: IO String
generarCategoria = do
  indice <- randomRIO (0, length categorias - 1)
  return (categorias !! indice)

generarCantidadEventos :: IO Int
generarCantidadEventos = randomRIO (10, 15)

generarFecha :: IO Integer
generarFecha = do
  ahora <- getCurrentTime
  let dosañosEnSegundos = 63072000 :: Integer
  segundosExtra <- randomRIO (0, dosañosEnSegundos)
  let fechaAleatoria = addUTCTime (fromIntegral segundosExtra) ahora
  return (formatearDia (utctDay fechaAleatoria))

formatearDia :: Day -> Integer
formatearDia dia =
  let (año, mes, diaMes) = toGregorian dia
  in año * 10000 + fromIntegral mes * 100 + fromIntegral diaMes

formatearFecha :: Integer -> String
formatearFecha timestamp =
  let año = timestamp `div` 10000
      mes = (timestamp `div` 100) `mod` 100
      dia = timestamp `mod` 100
  in show año ++ "-" ++ show mes ++ "-" ++ show dia

data Evento = Evento
  { eventoId :: Int
  , categoria :: String
  , valor     :: Double
  , fecha     :: Integer
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
  timestampFecha <- generarFecha
  return Evento
    { eventoId = nuevoId
    , categoria = nuevaCategoria
    , valor     = nuevoValor
    , fecha     = timestampFecha
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
extraerAño evento =
  let (año, mes, dia) = parseFecha (formatearFecha (fecha evento))
  in año

extraerMes :: Evento -> Int
extraerMes evento =
  let (año, mes, dia) = parseFecha (formatearFecha (fecha evento))
  in mes

extraerDia :: Evento -> Int
extraerDia evento =
  let (año, mes, dia) = parseFecha (formatearFecha (fecha evento))
  in dia

split :: String -> [String]
split "" = [""]
split (x:xs)
  | x == '-' = "" : resto
  | otherwise = (x : head resto) : tail resto
  where
    resto = split xs
parseFecha :: String -> (Int, Int, Int)
parseFecha fecha =
  let [añoStr, mesStr, diaStr] = split fecha
  in (read añoStr, read mesStr, read diaStr)
eventosAFechaTupla :: [Evento] -> [(Evento, (Int, Int, Int))]
eventosAFechaTupla eventos = [ (evento, parseFecha (formatearFecha (fecha evento))) | evento <- eventos ]

ordenarEventosPorFecha :: [(Evento, (Int, Int, Int))] -> [(Evento, (Int, Int, Int))]
ordenarEventosPorFecha = sortOn snd

eventoMasAntiguoYReciente :: [(Evento, (Int, Int, Int))] -> (Evento, Evento)
eventoMasAntiguoYReciente eventosConFecha =
  let eventosOrdenados = ordenarEventosPorFecha eventosConFecha
      (eventoMasAntiguo, fechaAntigua) = head eventosOrdenados
      (eventoMasReciente, fechaReciente) = last eventosOrdenados
  in (eventoMasAntiguo, eventoMasReciente)

sumarCategoriaPorAño :: [Evento] -> String -> Int -> Double
sumarCategoriaPorAño eventos categoriaBuscada añoBuscado =
  sum
    [ valor evento
    | evento <- eventos
    , categoria evento == categoriaBuscada
    , extraerAño evento == añoBuscado
    ]

sumasPorCategoriaYAño :: [Evento] -> IO [(String, Int, Double)]
sumasPorCategoriaYAño eventos = do
  años <- añosAnalisis
  return
    [ (categoriaActual, añoActual, sumarCategoriaPorAño eventos categoriaActual añoActual)
    | añoActual <- años
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

cantidadEventosPorCategoria :: [Evento] -> [(String, Int)]
cantidadEventosPorCategoria eventos =
  [ (categoriaActual, length [ evento | evento <- eventos, categoria evento == categoriaActual ])
  | categoriaActual <- categorias
  ]
imprimirCantidadEvento :: (String, Int) -> IO ()
imprimirCantidadEvento (categoriaNombre, cantidad) =
  putStrLn (categoriaNombre ++ ": " ++ show cantidad)
eventoMaxMin :: [Evento] -> (Evento, Evento)
eventoMaxMin eventos =
  let eventosOrdenados = sortOn valor eventos
      eventoMinimo = head eventosOrdenados
      eventoMaximo = last eventosOrdenados
  in (eventoMaximo, eventoMinimo)
imprimirEvento :: Evento -> IO ()
imprimirEvento evento = putStrLn $
  "ID: " ++ show (eventoId evento) ++ " | Categoria: " ++ categoria evento ++
  " | Valor: " ++ show (valor evento) ++ " | Fecha: " ++ formatearFecha (fecha evento)

imprimirMaxMinEventos :: [Evento] -> IO ()
imprimirMaxMinEventos eventos =
  let (eventoMaximo, eventoMinimo) = eventoMaxMin eventos
  in do
    putStrLn "--- Evento con monto maximo ---"
    imprimirEvento eventoMaximo
    putStrLn "--- Evento con monto minimo ---"
    imprimirEvento eventoMinimo

asignarAltoValor :: [Evento] -> [(String, Double)] -> [Evento]
asignarAltoValor eventos promedios =
  [ evento { esAltoValor = valor evento > promedio }
  | (categoriaActual, promedio) <- promedios
  , evento <- eventos
  , categoria evento == categoriaActual
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
  resultados <- sumasPorCategoriaYAño eventos
  putStrLn "--- Suma de montos por categoria y año ---"
  mapM_ imprimirSumaCategoriaYAño resultados

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
  let eventosConFecha = eventosAFechaTupla eventos
      (eventoMasAntiguo, eventoMasReciente) = eventoMasAntiguoYReciente eventosConFecha
  putStrLn "--- Evento mas antiguo ---"
  imprimirEvento eventoMasAntiguo
  putStrLn "--- Evento mas reciente ---"
  imprimirEvento eventoMasReciente

fechaConMasEventos :: [Evento] -> (String, Int)
fechaConMasEventos eventos =
  let fechasRegistradas = map (formatearFecha . fecha) eventos
      fechasSinRepetir = nub fechasRegistradas
      conteosPorFecha =
        [ (fechaActual
        , length [eventoActual
        | eventoActual <- eventos
        , formatearFecha (fecha eventoActual) == fechaActual])
        | fechaActual <- fechasSinRepetir
        ]
      conteosOrdenados = sortOn snd conteosPorFecha
  in last conteosOrdenados
resumenPorIntervalo :: [Evento] -> IO ()
resumenPorIntervalo eventos = do
  putStrLn "[Pendiente] Resumen de montos por intervalo"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

opcionBusqueda :: [Evento] -> IO ()
opcionBusqueda eventos = do
  putStrLn "[Pendiente] Busqueda por rango de fechas"
  putStrLn ("Eventos disponibles: " ++ show (length eventos))

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
