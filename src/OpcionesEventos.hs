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
  , exportarEventosCSV
  ) where

-- ============================================================
-- IMPORTS
-- ============================================================

import System.Random (randomRIO)
import Data.List (nub, sortOn)
import Data.Time.Calendar (toGregorian, Day, addDays)
import Data.Time.Clock (addUTCTime, getCurrentTime, utctDay)
import Archivos (ResumenGeneral(..), exportarResumenCSV)

-- ============================================================
-- ESTRUCTURAS DE DATOS
-- ============================================================

data Evento = Evento
  { eventoId         :: Int
  , categoria        :: String
  , valor            :: Double
  , fecha            :: Integer
  , esAltoValor      :: Bool
  , impuestoAplicado :: Bool
  } deriving (Eq, Show, Read)

type ListaEventos    = [Evento]
type ListaIdsUsados  = [Int]
type FechaDesglosada = (Int, Int, Int)

-- ============================================================
-- CONSTANTES
-- ============================================================

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

-- ============================================================
-- FUNCIONES AUXILIARES: FECHAS
-- ============================================================

añosAnalisis :: IO [Int]
añosAnalisis = do
  hoy <- getCurrentTime
  let (año, _, _) = toGregorian (utctDay hoy)
  return [fromInteger año .. fromInteger año + 2]

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

split :: String -> [String]
split "" = [""]
split (caracter:restoCadena)
  | caracter == '-'  = "" : partesRestantes
  | otherwise = (caracter : head partesRestantes) : tail partesRestantes
  where
    partesRestantes = split restoCadena

parseFecha :: String -> (Int, Int, Int)
parseFecha fechaTexto =
  let [añoStr, mesStr, diaStr] = split fechaTexto
  in (read añoStr, read mesStr, read diaStr)

diaAFechaDesglosada :: Day -> FechaDesglosada
diaAFechaDesglosada dia =
  let (año, mes, diaMes) = toGregorian dia
  in (fromInteger año, mes, diaMes)

-- ============================================================
-- FUNCIONES AUXILIARES: EXTRACCIÓN DE CAMPOS
-- ============================================================

extraerAño :: Evento -> Int
extraerAño evento =
  let (año, _, _) = parseFecha (formatearFecha (fecha evento))
  in año

extraerMes :: Evento -> Int
extraerMes evento =
  let (_, mes, _) = parseFecha (formatearFecha (fecha evento))
  in mes

extraerDia :: Evento -> Int
extraerDia evento =
  let (_, _, dia) = parseFecha (formatearFecha (fecha evento))
  in dia

extraerDiaSemana :: Evento -> Int
extraerDiaSemana evento =
  let (año, mes, dia) = parseFecha (formatearFecha (fecha evento))
  in diaDeSemana año mes dia

-- ============================================================
-- FUNCIONES AUXILIARES: DIAS DE LA SEMANA
-- ============================================================

diaDeSemana :: Int -> Int -> Int -> Int
diaDeSemana año mes dia =
  let t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
      a = if mes < 3 then año - 1 else año
  in (a + a `div` 4 - a `div` 100 + a `div` 400 + (t !! (mes - 1)) + dia) `mod` 7

nombreDiaSemana :: Int -> String
nombreDiaSemana 0 = "Domingo"
nombreDiaSemana 1 = "Lunes"
nombreDiaSemana 2 = "Martes"
nombreDiaSemana 3 = "Miercoles"
nombreDiaSemana 4 = "Jueves"
nombreDiaSemana 5 = "Viernes"
nombreDiaSemana 6 = "Sabado"
nombreDiaSemana _ = "Desconocido"

-- ============================================================
-- FUNCIONES AUXILIARES: GENERACIÓN DE EVENTOS
-- ============================================================

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
  let dosAñosEnSegundos = 63072000 :: Integer
  segundosExtra <- randomRIO (0, dosAñosEnSegundos)
  let fechaAleatoria = addUTCTime (fromIntegral segundosExtra) ahora
  return (formatearDia (utctDay fechaAleatoria))

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
    { eventoId         = nuevoId
    , categoria        = nuevaCategoria
    , valor            = nuevoValor
    , fecha            = timestampFecha
    , esAltoValor      = False
    , impuestoAplicado = False
    }

generarNEventos :: Int -> ListaIdsUsados -> IO ListaEventos
generarNEventos 0 _      = return []
generarNEventos cantidad usados = do
  evento <- generarEvento usados
  resto  <- generarNEventos (cantidad - 1) (eventoId evento : usados)
  return (evento : resto)

-- ============================================================
-- FUNCIONES AUXILIARES: CÁLCULOS POR CATEGORÍA
-- ============================================================

sumarCategoria :: [Evento] -> String -> Double
sumarCategoria eventos categoriaBuscada =
  sum [ valor eventoActual | eventoActual <- eventos, categoria eventoActual == categoriaBuscada ]

contarCategoria :: [Evento] -> String -> Int
contarCategoria eventos categoriaBuscada =
  length [ eventoActual | eventoActual <- eventos, categoria eventoActual == categoriaBuscada ]

promedioCategoria :: [Evento] -> String -> (String, Double)
promedioCategoria eventos categoriaBuscada =
  let total    = sumarCategoria eventos categoriaBuscada
      cantidad = contarCategoria eventos categoriaBuscada
  in (categoriaBuscada, total / fromIntegral cantidad)

promediosPorCategoria :: [Evento] -> [(String, Double)]
promediosPorCategoria eventos =
  [ promedioCategoria eventos categoriaActual | categoriaActual <- categorias ]

cantidadEventosPorCategoria :: [Evento] -> [(String, Int)]
cantidadEventosPorCategoria eventos =
  [ (categoriaActual, length [ eventoActual | eventoActual <- eventos, categoria eventoActual == categoriaActual ]) | categoriaActual <- categorias ]

sumarCategoriaPorAño :: [Evento] -> String -> Int -> Double
sumarCategoriaPorAño eventos categoriaBuscada añoBuscado =
  sum
    [ valor eventoActual
    | eventoActual <- eventos
    , categoria eventoActual == categoriaBuscada
    , extraerAño eventoActual == añoBuscado
    ]

sumasPorCategoriaYAño :: [Evento] -> IO [(String, Int, Double)]
sumasPorCategoriaYAño eventos = do
  añosAnalizados <- añosAnalisis
  return
    [ (categoriaActual, añoActual, sumarCategoriaPorAño eventos categoriaActual añoActual)
    | añoActual <- añosAnalizados
    , categoriaActual <- categorias
    ]

-- ============================================================
-- FUNCIONES AUXILIARES: CÁLCULOS TEMPORALES
-- ============================================================

paresAñoMesUnicos :: [Evento] -> [(Int, Int)]
paresAñoMesUnicos eventos =
  nub [ (extraerAño eventoActual, extraerMes eventoActual) | eventoActual <- eventos ]

sumarAñoMes :: [Evento] -> Int -> Int -> Double
sumarAñoMes eventos añoBuscado mesBuscado =
  sum
    [ valor eventoActual
    | eventoActual <- eventos
    , extraerAño eventoActual == añoBuscado
    , extraerMes eventoActual == mesBuscado
    ]

montosPorAñoMes :: [Evento] -> [(Int, Int, Double)]
montosPorAñoMes eventos =
  [ (añoActual, mesActual, sumarAñoMes eventos añoActual mesActual)
  | (añoActual, mesActual) <- paresAñoMesUnicos eventos
  ]

paresAñoDiaSemanaUnicos :: [Evento] -> [(Int, Int)]
paresAñoDiaSemanaUnicos eventos =
  nub [ (extraerAño eventoActual, extraerDiaSemana eventoActual) | eventoActual <- eventos ]

contarAñoDiaSemana :: [Evento] -> Int -> Int -> Int
contarAñoDiaSemana eventos añoBuscado diaSemanaBuscado =
  length
    [ eventoActual
    | eventoActual <- eventos
    , extraerAño eventoActual == añoBuscado
    , extraerDiaSemana eventoActual == diaSemanaBuscado
    ]

conteosPorAñoDiaSemana :: [Evento] -> [(Int, Int, Int)]
conteosPorAñoDiaSemana eventos =
  [ (añoActual, diaSemanaActual, contarAñoDiaSemana eventos añoActual diaSemanaActual)
  | (añoActual, diaSemanaActual) <- paresAñoDiaSemanaUnicos eventos
  ]

eventosAFechaTupla :: [Evento] -> [(Evento, FechaDesglosada)]
eventosAFechaTupla eventos =
  [ (eventoActual, parseFecha (formatearFecha (fecha eventoActual))) | eventoActual <- eventos ]

ordenarEventosPorFecha :: [(Evento, FechaDesglosada)] -> [(Evento, FechaDesglosada)]
ordenarEventosPorFecha = sortOn snd

eventoMasAntiguoYReciente :: [(Evento, FechaDesglosada)] -> (Evento, Evento)
eventoMasAntiguoYReciente eventosConFecha =
  let ordenados = ordenarEventosPorFecha eventosConFecha
  in (fst (head ordenados), fst (last ordenados))

fechaConMasEventos :: [Evento] -> (String, Int)
fechaConMasEventos eventos =
  let fechas       = map (formatearFecha . fecha) eventos
      fechasUnicas = nub fechas
      conteos      =
        [ (fechaActual, length [ eventoActual | eventoActual <- eventos, formatearFecha (fecha eventoActual) == fechaActual ])
        | fechaActual <- fechasUnicas
        ]
  in last (sortOn snd conteos)

eventoMaxMin :: [Evento] -> (Evento, Evento)
eventoMaxMin eventos =
  let ordenados = sortOn valor eventos
  in (last ordenados, head ordenados)

-- ============================================================
-- FUNCIONES AUXILIARES: INTERVALOS
-- ============================================================

intervalosDeNDiasDesdeHoy :: Int -> IO [(Day, Day)]
intervalosDeNDiasDesdeHoy n = do
  ahora <- getCurrentTime
  let dosAñosEnSegundos = 63072000 :: Integer
      fechaMaxima = utctDay (addUTCTime (fromIntegral dosAñosEnSegundos) ahora)
      hoy  = utctDay ahora
      paso = max 1 n
  return (construirIntervalos paso fechaMaxima hoy)


construirIntervalos :: Int -> Day -> Day -> [(Day, Day)]
construirIntervalos pasoActual fechaMaxima inicio
  | inicio > fechaMaxima = []
  | otherwise =
      let fin = min (addDays (fromIntegral (pasoActual - 1)) inicio) fechaMaxima
      in (inicio, fin) : construirIntervalos pasoActual fechaMaxima (addDays (fromIntegral pasoActual) inicio)

estaEnIntervalo :: FechaDesglosada -> (Day, Day) -> Bool
estaEnIntervalo fechaEvento (inicio, fin) =
  fechaEvento >= diaAFechaDesglosada inicio && fechaEvento <= diaAFechaDesglosada fin

eventosPorIntervalos :: [Evento] -> Int -> [(Evento, FechaDesglosada)]
eventosPorIntervalos eventos _tamanoIntervalo =
  sortOn snd [ (eventoActual, parseFecha (formatearFecha (fecha eventoActual))) | eventoActual <- eventos ]

eventosDentroDeIntervalo :: (Day, Day) -> [(Evento, FechaDesglosada)] -> [(Evento, FechaDesglosada)]
eventosDentroDeIntervalo intervalo eventosConFecha =
  [ eventoConFecha | eventoConFecha@(_, fechaEvento) <- eventosConFecha, estaEnIntervalo fechaEvento intervalo ]

eventosAgrupadosPorIntervalo :: [(Day, Day)] -> [(Evento, FechaDesglosada)] -> [((Day, Day), [(Evento, FechaDesglosada)])]
eventosAgrupadosPorIntervalo intervalos eventosConFecha =
  [ (intervaloActual, eventosDentroDeIntervalo intervaloActual eventosConFecha) | intervaloActual <- intervalos ]

eventosDentroDeRango :: Integer -> Integer -> [Evento] -> [Evento]
eventosDentroDeRango inicio fin eventos =
  [ eventoActual | eventoActual <- eventos, fecha eventoActual >= inicio, fecha eventoActual <= fin ]

-- ============================================================
-- FUNCIONES AUXILIARES: TRANSFORMACIONES
-- ============================================================

aplicarImpuestoEventos :: [Evento] -> [Evento]
aplicarImpuestoEventos eventos =
  [ if categoria eventoActual == "Compra" && not (impuestoAplicado eventoActual)
      then eventoActual { valor = valor eventoActual * impuestoCompra, impuestoAplicado = True }
      else eventoActual
  | eventoActual <- eventos
  ]

asignarAltoValor :: [Evento] -> [(String, Double)] -> [Evento]
asignarAltoValor eventos promedios =
  [ eventoActual { esAltoValor = valor eventoActual > promedioCategoriaActual }
  | (categoriaActual, promedioCategoriaActual) <- promedios
  , eventoActual <- eventos
  , categoria eventoActual == categoriaActual
  ]

-- ============================================================
-- FUNCIONES DE IMPRESIÓN
-- ============================================================

imprimirEvento :: Evento -> IO ()
imprimirEvento eventoActual = putStrLn $
  "ID: "         ++ show (eventoId eventoActual)  ++
  " | Categoria: " ++ categoria eventoActual       ++
  " | Valor: "     ++ show (valor eventoActual)    ++
  " | Fecha: "     ++ formatearFecha (fecha eventoActual)

imprimirCantidadEvento :: (String, Int) -> IO ()
imprimirCantidadEvento (categoriaActual, cantidad) =
  putStrLn (categoriaActual ++ ": " ++ show cantidad)

imprimirSumaCategoriaYAño :: (String, Int, Double) -> IO ()
imprimirSumaCategoriaYAño (categoriaActual, año, suma) =
  putStrLn (categoriaActual ++ " " ++ show año ++ ": " ++ show suma)

imprimirMaxMinEventos :: [Evento] -> IO ()
imprimirMaxMinEventos eventos =
  let (eventoMaximo, eventoMinimo) = eventoMaxMin eventos
  in do
    putStrLn "--- Evento con monto maximo ---"
    imprimirEvento eventoMaximo
    putStrLn "--- Evento con monto minimo ---"
    imprimirEvento eventoMinimo

imprimirComprasConImpuesto :: [Evento] -> IO ()
imprimirComprasConImpuesto eventos = do
  putStrLn "--- Eventos de compra con impuesto aplicado ---"
  mapM_ print [ eventoActual | eventoActual <- eventos, impuestoAplicado eventoActual ]

imprimirAltosValores :: [Evento] -> IO ()
imprimirAltosValores eventos = do
  putStrLn "--- Eventos de alto valor ---"
  mapM_ print [ eventoActual | eventoActual <- eventos, esAltoValor eventoActual ]

imprimirIntervaloAgrupado :: ((Day, Day), [(Evento, FechaDesglosada)]) -> IO ()
imprimirIntervaloAgrupado ((inicio, fin), eventosDelIntervalo) = do
  let cantidad    = length eventosDelIntervalo
      montoTotal  = sum [ valor eventoActual | (eventoActual, _) <- eventosDelIntervalo ]
  putStrLn ("Intervalo: " ++ show inicio ++ " a " ++ show fin)
  putStrLn ("Cantidad de eventos: " ++ show cantidad)
  putStrLn ("Monto total en intervalo: " ++ show montoTotal)

-- ============================================================
-- FUNCIONES DEL MENÚ: TRANSFORMACIÓN
-- ============================================================

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
  let actualizados = aplicarImpuestoEventos eventos
  imprimirComprasConImpuesto actualizados
  return actualizados

etiquetarAltoValor :: [Evento] -> IO [Evento]
etiquetarAltoValor eventos = do
  let promedios    = promediosPorCategoria eventos
      actualizados = asignarAltoValor eventos promedios
  imprimirAltosValores actualizados
  return actualizados

-- ============================================================
-- FUNCIONES DEL MENÚ: ANÁLISIS DE DATOS
-- ============================================================

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
  putStrLn "--- Monto total ---"
  putStrLn ("Cantidad total de eventos: " ++ show (length eventos))
  putStrLn ("Suma total de montos: " ++ show (sum (map valor eventos)))

promedioPorCategoria :: [Evento] -> IO ()
promedioPorCategoria eventos = do
  resultados <- sumasPorCategoriaYAño eventos
  putStrLn "--- Suma de montos por categoria y año ---"
  mapM_ imprimirSumaCategoriaYAño resultados

-- ============================================================
-- FUNCIONES DEL MENÚ: ANÁLISIS TEMPORAL
-- ============================================================

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
  let (añoMayor, mesMayor, montoMayor) = last (sortOn (\(_, _, montoActual) -> montoActual) (montosPorAñoMes eventos))
      (añoDia,  diaMayor, cantMayor)   = last (sortOn (\(_, _, cantidadActual) -> cantidadActual) (conteosPorAñoDiaSemana eventos))
  putStrLn "--- Mes con mayor monto total ---"
  putStrLn ("Año: " ++ show añoMayor ++ " | Mes: " ++ show mesMayor)
  putStrLn ("Monto acumulado: " ++ show montoMayor)
  putStrLn ""
  putStrLn "--- Dia de la semana mas activo ---"
  putStrLn ("Año: " ++ show añoDia ++ " | Dia: " ++ nombreDiaSemana diaMayor)
  putStrLn ("Cantidad de eventos: " ++ show cantMayor)

eventoAntiguoReciente :: [Evento] -> IO ()
eventoAntiguoReciente eventos = do
  let (antiguo, reciente) = eventoMasAntiguoYReciente (eventosAFechaTupla eventos)
  putStrLn "--- Evento mas antiguo ---"
  imprimirEvento antiguo
  putStrLn "--- Evento mas reciente ---"
  imprimirEvento reciente

resumenPorIntervalo :: [Evento] -> IO ()
resumenPorIntervalo eventos = do
  putStrLn "--- Resumen de montos por intervalo ---"
  putStrLn "Ingrese la cantidad de dias por intervalo (ej: 50):"
  entrada <- getLine
  let diasPorIntervalo = read entrada :: Int
  intervalos <- intervalosDeNDiasDesdeHoy diasPorIntervalo
  let eventosConFecha = eventosPorIntervalos eventos diasPorIntervalo
      agrupados       = eventosAgrupadosPorIntervalo intervalos eventosConFecha
  putStrLn ("Intervalo aceptado: " ++ show diasPorIntervalo ++ " dias")
  putStrLn ("Cantidad de intervalos generados: " ++ show (length intervalos))
  mapM_ imprimirIntervaloAgrupado agrupados

-- ============================================================
-- FUNCIONES DEL MENÚ: BÚSQUEDA
-- ============================================================

opcionBusqueda :: [Evento] -> IO ()
opcionBusqueda eventos = do
  putStrLn ""
  putStrLn "--- Busqueda por rango de fechas ---"
  putStrLn "Ingrese fecha de inicio (formato AAAAMMDD, ej: 20260101):"
  putStr "> "
  entradaInicio <- getLine
  putStrLn "Ingrese fecha de fin (formato AAAAMMDD, ej: 20261231):"
  putStr "> "
  entradaFin <- getLine
  let inicio    = read entradaInicio :: Integer
      fin       = read entradaFin   :: Integer
      resultado = eventosDentroDeRango inicio fin eventos
  if inicio > fin
    then putStrLn "Error: la fecha de inicio no puede ser mayor a la fecha de fin."
    else do
      putStrLn ("Eventos encontrados entre " ++ formatearFecha inicio ++ " y " ++ formatearFecha fin ++ ": " ++ show (length resultado))
      mapM_ imprimirEvento resultado

-- ============================================================
-- FUNCIONES DEL MENÚ: ESTADÍSTICAS
-- ============================================================

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
  let conteos                        = cantidadEventosPorCategoria eventos
      (fechaMayorActividad, cantMax) = fechaConMasEventos eventos
      (eventoMax, eventoMin)         = eventoMaxMin eventos
  putStrLn "--- Cantidad de eventos por categoria ---"
  mapM_ imprimirCantidadEvento conteos
  imprimirMaxMinEventos eventos
  putStrLn "--- Dia con mayor cantidad de eventos ---"
  putStrLn ("Fecha: " ++ fechaMayorActividad)
  putStrLn ("Cantidad de eventos: " ++ show cantMax)
  putStrLn ""
  putStrLn "Exportar reporte:"
  putStrLn "1) CSV"
  putStrLn "2) No exportar"
  putStr "> "
  opcion <- getLine
  case opcion of
    "1" -> do
      let resumen = ResumenGeneral
            conteos
            (eventoId eventoMax, categoria eventoMax, valor eventoMax, formatearFecha (fecha eventoMax))
            (eventoId eventoMin, categoria eventoMin, valor eventoMin, formatearFecha (fecha eventoMin))
            (fechaMayorActividad, cantMax)
      exportarResumenCSV "reporte_resumen.csv" resumen
    "2" -> return ()
    _   -> putStrLn "Opcion invalida."

-- ============================================================
-- EXPORTACIÓN CSV
-- ============================================================

exportarEventosCSV :: FilePath -> [Evento] -> IO ()
exportarEventosCSV ruta eventos = do
  let encabezado = "EventoId,Categoria,Valor,Fecha,EsAltoValor,ImpuestoAplicado"
      filas      = map eventoACSV eventos
      contenido  = encabezado ++ "\n" ++ unlines filas
  writeFile ruta contenido
  putStrLn ("Archivo exportado exitosamente: " ++ ruta)

eventoACSV :: Evento -> String
eventoACSV eventoActual =
  show (eventoId eventoActual)          ++ "," ++
  categoria eventoActual                ++ "," ++
  show (valor eventoActual)             ++ "," ++
  formatearFecha (fecha eventoActual)   ++ "," ++
  show (esAltoValor eventoActual)       ++ "," ++
  show (impuestoAplicado eventoActual)