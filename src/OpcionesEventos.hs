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
  , guardarTodosEventos
  , cargarEventosIniciales
  ) where

-- ============================================================
-- IMPORTS
-- ============================================================

import System.Random (randomRIO)
import Data.List (nub, sortOn)
import Data.Time.Calendar (toGregorian, Day, addDays)
import Data.Time.Clock (addUTCTime, getCurrentTime, utctDay)
import System.Directory (doesFileExist)   -- <-- NUEVO
import Archivos (ResumenGeneral(..), exportarResumenCSV)

-- ============================================================
-- ESTRUCTURAS DE DATOS
-- ============================================================

-- | Estructura: Evento
-- Descripcion: Representa un evento comercial con identificador,
-- categoria, valor, fecha y banderas de transformacion.
data Evento = Evento
  { eventoId         :: Int
  , categoria        :: String
  , valor            :: Double
  , fecha            :: Integer
  , esAltoValor      :: Bool
  , impuestoAplicado :: Bool
  } deriving (Eq, Show, Read)

-- | Estructura: ListaEventos
-- Descripcion: Alias para una coleccion de eventos.
type ListaEventos    = [Evento]

-- | Estructura: ListaIdsUsados
-- Descripcion: Alias para IDs ya asignados durante la generacion.
type ListaIdsUsados  = [Int]

-- | Estructura: FechaDesglosada
-- Descripcion: Fecha representada como tupla (año, mes, dia).
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

archivoEventos :: FilePath
archivoEventos = "eventos.csv"

-- ============================================================
-- FUNCIONES AUXILIARES: FECHAS
-- ============================================================

-- | Nombre: añosAnalisis
-- Entrada: No recibe parametros.
-- Funcionalidad o Salida: Devuelve una lista con el año actual
-- y los dos años siguientes para analisis agregados.
añosAnalisis :: IO [Int]
añosAnalisis = do
  hoy <- getCurrentTime
  let (año, _, _) = toGregorian (utctDay hoy)
  return [fromInteger año .. fromInteger año + 2]

-- | Nombre: formatearDia
-- Entrada: Fecha de tipo Day.
-- Funcionalidad o Salida: Convierte Day al formato numerico AAAAMMDD.
formatearDia :: Day -> Integer
formatearDia dia =
  let (año, mes, diaMes) = toGregorian dia
  in año * 10000 + fromIntegral mes * 100 + fromIntegral diaMes

-- | Nombre: formatearFecha
-- Entrada: Fecha numerica en formato AAAAMMDD.
-- Funcionalidad o Salida: Convierte la fecha numerica a texto AAAA-M-D.
formatearFecha :: Integer -> String
formatearFecha timestamp =
  let año = timestamp `div` 10000
      mes = (timestamp `div` 100) `mod` 100
      dia = timestamp `mod` 100
  in show año ++ "-" ++ show mes ++ "-" ++ show dia

-- | Nombre: split
-- Entrada: Cadena separada por guiones.
-- Funcionalidad o Salida: Divide la cadena en partes usando '-'.
split :: String -> [String]
split "" = [""]
split (caracter:restoCadena)
  | caracter == '-'  = "" : partesRestantes
  | otherwise = (caracter : head partesRestantes) : tail partesRestantes
  where
    partesRestantes = split restoCadena

-- | Nombre: parseFecha
-- Entrada: Fecha en texto con formato AAAA-M-D.
-- Funcionalidad o Salida: Retorna la tupla (año, mes, dia).
parseFecha :: String -> (Int, Int, Int)
parseFecha fechaTexto =
  let [añoStr, mesStr, diaStr] = split fechaTexto
  in (read añoStr, read mesStr, read diaStr)

-- | Nombre: diaAFechaDesglosada
-- Entrada: Fecha de tipo Day.
-- Funcionalidad o Salida: Convierte Day a tupla (año, mes, dia).
diaAFechaDesglosada :: Day -> FechaDesglosada
diaAFechaDesglosada dia =
  let (año, mes, diaMes) = toGregorian dia
  in (fromInteger año, mes, diaMes)

-- ============================================================
-- FUNCIONES AUXILIARES: EXTRACCIÓN DE CAMPOS
-- ============================================================

-- | Nombre: extraerAño
-- Entrada: Un evento.
-- Funcionalidad o Salida: Obtiene el año de la fecha del evento.
extraerAño :: Evento -> Int
extraerAño evento =
  let (año, _, _) = parseFecha (formatearFecha (fecha evento))
  in año

-- | Nombre: extraerMes
-- Entrada: Un evento.
-- Funcionalidad o Salida: Obtiene el mes de la fecha del evento.
extraerMes :: Evento -> Int
extraerMes evento =
  let (_, mes, _) = parseFecha (formatearFecha (fecha evento))
  in mes

-- | Nombre: extraerDia
-- Entrada: Un evento.
-- Funcionalidad o Salida: Obtiene el dia de la fecha del evento.
extraerDia :: Evento -> Int
extraerDia evento =
  let (_, _, dia) = parseFecha (formatearFecha (fecha evento))
  in dia

-- | Nombre: extraerDiaSemana
-- Entrada: Un evento.
-- Funcionalidad o Salida: Calcula el indice numerico del dia de semana.
extraerDiaSemana :: Evento -> Int
extraerDiaSemana evento =
  let (año, mes, dia) = parseFecha (formatearFecha (fecha evento))
  in diaDeSemana año mes dia

-- ============================================================
-- FUNCIONES AUXILIARES: DIAS DE LA SEMANA
-- ============================================================

-- | Nombre: diaDeSemana
-- Entrada: año, mes y dia.
-- Funcionalidad o Salida: Calcula el dia de la semana en rango 0..6.
diaDeSemana :: Int -> Int -> Int -> Int
diaDeSemana año mes dia =
  let t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
      a = if mes < 3 then año - 1 else año
  in (a + a `div` 4 - a `div` 100 + a `div` 400 + (t !! (mes - 1)) + dia) `mod` 7

-- | Nombre: nombreDiaSemana
-- Entrada: Indice de dia de semana (0..6).
-- Funcionalidad o Salida: Retorna el nombre del dia en espanol.
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

-- | Nombre: generarValor
-- Entrada: No recibe parametros.
-- Funcionalidad o Salida: Genera un valor aleatorio para un evento.
generarValor :: IO Double
generarValor = randomRIO (500, 75000)

-- | Nombre: generarCategoria
-- Entrada: No recibe parametros.
-- Funcionalidad o Salida: Elige una categoria aleatoria del catalogo.
generarCategoria :: IO String
generarCategoria = do
  indice <- randomRIO (0, length categorias - 1)
  return (categorias !! indice)

-- | Nombre: generarCantidadEventos
-- Entrada: No recibe parametros.
-- Funcionalidad o Salida: Genera una cantidad aleatoria de eventos.
generarCantidadEventos :: IO Int
generarCantidadEventos = randomRIO (10, 15)

-- | Nombre: generarFecha
-- Entrada: No recibe parametros.
-- Funcionalidad o Salida: Genera una fecha aleatoria en los proximos dos años.
generarFecha :: IO Integer
generarFecha = do
  ahora <- getCurrentTime
  let dosAñosEnSegundos = 63072000 :: Integer
  segundosExtra <- randomRIO (0, dosAñosEnSegundos)
  let fechaAleatoria = addUTCTime (fromIntegral segundosExtra) ahora
  return (formatearDia (utctDay fechaAleatoria))

-- | Nombre: generarId
-- Entrada: Lista de IDs ya usados.
-- Funcionalidad o Salida: Devuelve un ID aleatorio no repetido.
generarId :: ListaIdsUsados -> IO Int
generarId usados = do
  candidato <- randomRIO (0, 9000000)
  if candidato `elem` usados
    then generarId usados
    else return candidato

-- | Nombre: generarEvento
-- Entrada: Lista de IDs ya usados.
-- Funcionalidad o Salida: Crea un evento aleatorio con banderas iniciales en False.
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

-- | Nombre: generarNEventos
-- Entrada: Cantidad a generar y lista de IDs usados.
-- Funcionalidad o Salida: Genera N eventos aleatorios sin repetir ID.
generarNEventos :: Int -> ListaIdsUsados -> IO ListaEventos
generarNEventos 0 _      = return []
generarNEventos cantidad usados = do
  evento <- generarEvento usados
  resto  <- generarNEventos (cantidad - 1) (eventoId evento : usados)
  return (evento : resto)

-- ============================================================
-- FUNCIONES AUXILIARES: CÁLCULOS POR CATEGORÍA
-- ============================================================

-- | Nombre: sumarCategoria
-- Entrada: Lista de eventos y categoria objetivo.
-- Funcionalidad o Salida: Suma los montos de la categoria indicada.
sumarCategoria :: [Evento] -> String -> Double
sumarCategoria eventos categoriaBuscada =
  sum [ valor eventoActual | eventoActual <- eventos, categoria eventoActual == categoriaBuscada ]

-- | Nombre: contarCategoria
-- Entrada: Lista de eventos y categoria objetivo.
-- Funcionalidad o Salida: Cuenta cuantos eventos pertenecen a la categoria.
contarCategoria :: [Evento] -> String -> Int
contarCategoria eventos categoriaBuscada =
  length [ eventoActual | eventoActual <- eventos, categoria eventoActual == categoriaBuscada ]

-- | Nombre: promedioCategoria
-- Entrada: Lista de eventos y categoria objetivo.
-- Funcionalidad o Salida: Retorna categoria y promedio de montos.
promedioCategoria :: [Evento] -> String -> (String, Double)
promedioCategoria eventos categoriaBuscada =
  let total    = sumarCategoria eventos categoriaBuscada
      cantidad = contarCategoria eventos categoriaBuscada
  in (categoriaBuscada, total / fromIntegral cantidad)

-- | Nombre: promediosPorCategoria
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Calcula promedios para todas las categorias.
promediosPorCategoria :: [Evento] -> [(String, Double)]
promediosPorCategoria eventos =
  [ promedioCategoria eventos categoriaActual | categoriaActual <- categorias ]

-- | Nombre: cantidadEventosPorCategoria
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Devuelve cantidad de eventos por categoria.
cantidadEventosPorCategoria :: [Evento] -> [(String, Int)]
cantidadEventosPorCategoria eventos =
  [ (categoriaActual, length [ eventoActual | eventoActual <- eventos, categoria eventoActual == categoriaActual ]) | categoriaActual <- categorias ]

-- | Nombre: sumarCategoriaPorAño
-- Entrada: Eventos, categoria objetivo y año objetivo.
-- Funcionalidad o Salida: Suma montos filtrando por categoria y año.
sumarCategoriaPorAño :: [Evento] -> String -> Int -> Double
sumarCategoriaPorAño eventos categoriaBuscada añoBuscado =
  sum
    [ valor eventoActual
    | eventoActual <- eventos
    , categoria eventoActual == categoriaBuscada
    , extraerAño eventoActual == añoBuscado
    ]

-- | Nombre: sumasPorCategoriaYAño
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Genera la suma por categoria para cada año analizado.
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

-- | Nombre: paresAñoMesUnicos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Obtiene pares unicos de (año, mes).
paresAñoMesUnicos :: [Evento] -> [(Int, Int)]
paresAñoMesUnicos eventos =
  nub [ (extraerAño eventoActual, extraerMes eventoActual) | eventoActual <- eventos ]

-- | Nombre: sumarAñoMes
-- Entrada: Eventos, año y mes objetivo.
-- Funcionalidad o Salida: Suma montos del año y mes indicados.
sumarAñoMes :: [Evento] -> Int -> Int -> Double
sumarAñoMes eventos añoBuscado mesBuscado =
  sum
    [ valor eventoActual
    | eventoActual <- eventos
    , extraerAño eventoActual == añoBuscado
    , extraerMes eventoActual == mesBuscado
    ]

-- | Nombre: montosPorAñoMes
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Devuelve montos agregados por (año, mes).
montosPorAñoMes :: [Evento] -> [(Int, Int, Double)]
montosPorAñoMes eventos =
  [ (añoActual, mesActual, sumarAñoMes eventos añoActual mesActual)
  | (añoActual, mesActual) <- paresAñoMesUnicos eventos
  ]

-- | Nombre: paresAñoDiaSemanaUnicos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Obtiene pares unicos de (año, diaSemana).
paresAñoDiaSemanaUnicos :: [Evento] -> [(Int, Int)]
paresAñoDiaSemanaUnicos eventos =
  nub [ (extraerAño eventoActual, extraerDiaSemana eventoActual) | eventoActual <- eventos ]

-- | Nombre: contarAñoDiaSemana
-- Entrada: Eventos, año y dia de semana objetivo.
-- Funcionalidad o Salida: Cuenta eventos para ese año y dia semanal.
contarAñoDiaSemana :: [Evento] -> Int -> Int -> Int
contarAñoDiaSemana eventos añoBuscado diaSemanaBuscado =
  length
    [ eventoActual
    | eventoActual <- eventos
    , extraerAño eventoActual == añoBuscado
    , extraerDiaSemana eventoActual == diaSemanaBuscado
    ]

-- | Nombre: conteosPorAñoDiaSemana
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Devuelve conteos por año y dia de semana.
conteosPorAñoDiaSemana :: [Evento] -> [(Int, Int, Int)]
conteosPorAñoDiaSemana eventos =
  [ (añoActual, diaSemanaActual, contarAñoDiaSemana eventos añoActual diaSemanaActual)
  | (añoActual, diaSemanaActual) <- paresAñoDiaSemanaUnicos eventos
  ]

-- | Nombre: eventosAFechaTupla
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Asocia cada evento con su fecha desglosada.
eventosAFechaTupla :: [Evento] -> [(Evento, FechaDesglosada)]
eventosAFechaTupla eventos =
  [ (eventoActual, parseFecha (formatearFecha (fecha eventoActual))) | eventoActual <- eventos ]

-- | Nombre: ordenarEventosPorFecha
-- Entrada: Lista de eventos con fecha desglosada.
-- Funcionalidad o Salida: Ordena eventos de menor a mayor fecha.
ordenarEventosPorFecha :: [(Evento, FechaDesglosada)] -> [(Evento, FechaDesglosada)]
ordenarEventosPorFecha = sortOn snd

-- | Nombre: eventoMasAntiguoYReciente
-- Entrada: Eventos con fecha desglosada.
-- Funcionalidad o Salida: Devuelve el evento mas antiguo y el mas reciente.
eventoMasAntiguoYReciente :: [(Evento, FechaDesglosada)] -> (Evento, Evento)
eventoMasAntiguoYReciente eventosConFecha =
  let ordenados = ordenarEventosPorFecha eventosConFecha
  in (fst (head ordenados), fst (last ordenados))

-- | Nombre: fechaConMasEventos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Retorna la fecha con mayor cantidad de eventos.
fechaConMasEventos :: [Evento] -> (String, Int)
fechaConMasEventos eventos =
  let fechas       = map (formatearFecha . fecha) eventos
      fechasUnicas = nub fechas
      conteos      =
        [ (fechaActual, length [ eventoActual | eventoActual <- eventos, formatearFecha (fecha eventoActual) == fechaActual ])
        | fechaActual <- fechasUnicas
        ]
  in last (sortOn snd conteos)

-- | Nombre: eventoMaxMin
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Devuelve evento de mayor y menor valor.
eventoMaxMin :: [Evento] -> (Evento, Evento)
eventoMaxMin eventos =
  let ordenados = sortOn valor eventos
  in (last ordenados, head ordenados)

-- ============================================================
-- FUNCIONES AUXILIARES: INTERVALOS
-- ============================================================

-- | Nombre: intervalosDeNDiasDesdeHoy
-- Entrada: Tamano del intervalo en dias.
-- Funcionalidad o Salida: Genera intervalos desde hoy hasta dos años hacia adelante.
intervalosDeNDiasDesdeHoy :: Int -> IO [(Day, Day)]
intervalosDeNDiasDesdeHoy n = do
  ahora <- getCurrentTime
  let dosAñosEnSegundos = 63072000 :: Integer
      fechaMaxima = utctDay (addUTCTime (fromIntegral dosAñosEnSegundos) ahora)
      hoy  = utctDay ahora
      paso = max 1 n
  return (construirIntervalos paso fechaMaxima hoy)

-- | Nombre: construirIntervalos
-- Entrada: Paso en dias, fecha maxima y fecha de inicio.
-- Funcionalidad o Salida: Construye recursivamente los intervalos de fechas.
construirIntervalos :: Int -> Day -> Day -> [(Day, Day)]
construirIntervalos pasoActual fechaMaxima inicio
  | inicio > fechaMaxima = []
  | otherwise =
      let fin = min (addDays (fromIntegral (pasoActual - 1)) inicio) fechaMaxima
      in (inicio, fin) : construirIntervalos pasoActual fechaMaxima (addDays (fromIntegral pasoActual) inicio)

-- | Nombre: estaEnIntervalo
-- Entrada: Fecha desglosada de evento e intervalo (inicio, fin).
-- Funcionalidad o Salida: Indica si la fecha cae dentro del intervalo.
estaEnIntervalo :: FechaDesglosada -> (Day, Day) -> Bool
estaEnIntervalo fechaEvento (inicio, fin) =
  fechaEvento >= diaAFechaDesglosada inicio && fechaEvento <= diaAFechaDesglosada fin

-- | Nombre: eventosPorIntervalos
-- Entrada: Lista de eventos y tamano de intervalo.
-- Funcionalidad o Salida: Devuelve eventos ordenados por fecha desglosada.
eventosPorIntervalos :: [Evento] -> Int -> [(Evento, FechaDesglosada)]
eventosPorIntervalos eventos _tamanoIntervalo =
  sortOn snd [ (eventoActual, parseFecha (formatearFecha (fecha eventoActual))) | eventoActual <- eventos ]

-- | Nombre: eventosDentroDeIntervalo
-- Entrada: Intervalo y eventos con fecha desglosada.
-- Funcionalidad o Salida: Filtra solo los eventos del intervalo.
eventosDentroDeIntervalo :: (Day, Day) -> [(Evento, FechaDesglosada)] -> [(Evento, FechaDesglosada)]
eventosDentroDeIntervalo intervalo eventosConFecha =
  [ eventoConFecha | eventoConFecha@(_, fechaEvento) <- eventosConFecha, estaEnIntervalo fechaEvento intervalo ]

-- | Nombre: eventosAgrupadosPorIntervalo
-- Entrada: Lista de intervalos y eventos con fecha.
-- Funcionalidad o Salida: Asocia a cada intervalo los eventos contenidos.
eventosAgrupadosPorIntervalo :: [(Day, Day)] -> [(Evento, FechaDesglosada)] -> [((Day, Day), [(Evento, FechaDesglosada)])]
eventosAgrupadosPorIntervalo intervalos eventosConFecha =
  [ (intervaloActual, eventosDentroDeIntervalo intervaloActual eventosConFecha) | intervaloActual <- intervalos ]

-- | Nombre: eventosDentroDeRango
-- Entrada: Fecha inicio, fecha fin y lista de eventos.
-- Funcionalidad o Salida: Filtra eventos cuyo timestamp cae en el rango.
eventosDentroDeRango :: Integer -> Integer -> [Evento] -> [Evento]
eventosDentroDeRango inicio fin eventos =
  [ eventoActual | eventoActual <- eventos, fecha eventoActual >= inicio, fecha eventoActual <= fin ]

-- ============================================================
-- FUNCIONES AUXILIARES: TRANSFORMACIONES
-- ============================================================

-- | Nombre: aplicarImpuestoEventos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Aplica impuesto a compras no procesadas.
aplicarImpuestoEventos :: [Evento] -> [Evento]
aplicarImpuestoEventos eventos =
  [ if categoria eventoActual == "Compra" && not (impuestoAplicado eventoActual)
      then eventoActual { valor = valor eventoActual * impuestoCompra, impuestoAplicado = True }
      else eventoActual
  | eventoActual <- eventos
  ]

-- | Nombre: asignarAltoValor
-- Entrada: Lista de eventos y promedios por categoria.
-- Funcionalidad o Salida: Marca eventos cuyo valor supera su promedio.
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

-- | Nombre: imprimirEvento
-- Entrada: Un evento.
-- Funcionalidad o Salida: Muestra en consola los campos principales del evento.
imprimirEvento :: Evento -> IO ()
imprimirEvento eventoActual = putStrLn $
  "ID: "         ++ show (eventoId eventoActual)  ++
  " | Categoria: " ++ categoria eventoActual       ++
  " | Valor: "     ++ show (valor eventoActual)    ++
  " | Fecha: "     ++ formatearFecha (fecha eventoActual)

-- | Nombre: imprimirCantidadEvento
-- Entrada: Par (categoria, cantidad).
-- Funcionalidad o Salida: Imprime la cantidad de eventos por categoria.
imprimirCantidadEvento :: (String, Int) -> IO ()
imprimirCantidadEvento (categoriaActual, cantidad) =
  putStrLn (categoriaActual ++ ": " ++ show cantidad)

-- | Nombre: imprimirSumaCategoriaYAño
-- Entrada: Tupla (categoria, año, suma).
-- Funcionalidad o Salida: Imprime la suma anual por categoria.
imprimirSumaCategoriaYAño :: (String, Int, Double) -> IO ()
imprimirSumaCategoriaYAño (categoriaActual, año, suma) =
  putStrLn (categoriaActual ++ " " ++ show año ++ ": " ++ show suma)

-- | Nombre: imprimirMaxMinEventos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime evento de mayor y menor monto.
imprimirMaxMinEventos :: [Evento] -> IO ()
imprimirMaxMinEventos eventos =
  let (eventoMaximo, eventoMinimo) = eventoMaxMin eventos
  in do
    putStrLn "--- Evento con monto maximo ---"
    imprimirEvento eventoMaximo
    putStrLn "--- Evento con monto minimo ---"
    imprimirEvento eventoMinimo

-- | Nombre: imprimirComprasConImpuesto
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime solo compras con impuesto aplicado.
imprimirComprasConImpuesto :: [Evento] -> IO ()
imprimirComprasConImpuesto eventos = do
  putStrLn "--- Eventos de compra con impuesto aplicado ---"
  mapM_ print [ eventoActual | eventoActual <- eventos, impuestoAplicado eventoActual ]

-- | Nombre: imprimirAltosValores
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime eventos marcados como alto valor.
imprimirAltosValores :: [Evento] -> IO ()
imprimirAltosValores eventos = do
  putStrLn "--- Eventos de alto valor ---"
  mapM_ print [ eventoActual | eventoActual <- eventos, esAltoValor eventoActual ]

-- | Nombre: imprimirIntervaloAgrupado
-- Entrada: Par de intervalo y eventos contenidos.
-- Funcionalidad o Salida: Imprime cantidad y monto total del intervalo.
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

-- | Nombre: opcionTransformacion
-- Entrada: Lista actual de eventos.
-- Funcionalidad o Salida: Muestra submenu de transformacion y retorna eventos actualizados.
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

-- | Nombre: aplicarImpuesto
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Aplica impuesto a compras, imprime resultado y retorna lista actualizada.
aplicarImpuesto :: [Evento] -> IO [Evento]
aplicarImpuesto eventos = do
  let actualizados = aplicarImpuestoEventos eventos
  imprimirComprasConImpuesto actualizados
  return actualizados

-- | Nombre: etiquetarAltoValor
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Etiqueta eventos de alto valor e imprime los marcados.
etiquetarAltoValor :: [Evento] -> IO [Evento]
etiquetarAltoValor eventos = do
  let promedios    = promediosPorCategoria eventos
      actualizados = asignarAltoValor eventos promedios
  imprimirAltosValores actualizados
  return actualizados

-- ============================================================
-- FUNCIONES DEL MENÚ: ANÁLISIS DE DATOS
-- ============================================================

-- | Nombre: opcionAnalisisDatos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Muestra submenu de analisis de datos y ejecuta la opcion elegida.
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

-- | Nombre: montoTotal
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime cantidad total de eventos y suma de montos.
montoTotal :: [Evento] -> IO ()
montoTotal eventos = do
  putStrLn "--- Monto total ---"
  putStrLn ("Cantidad total de eventos: " ++ show (length eventos))
  putStrLn ("Suma total de montos: " ++ show (sum (map valor eventos)))

-- | Nombre: promedioPorCategoria
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime sumas por categoria y año analizado.
promedioPorCategoria :: [Evento] -> IO ()
promedioPorCategoria eventos = do
  resultados <- sumasPorCategoriaYAño eventos
  putStrLn "--- Suma de montos por categoria y año ---"
  mapM_ imprimirSumaCategoriaYAño resultados

-- ============================================================
-- FUNCIONES DEL MENÚ: ANÁLISIS TEMPORAL
-- ============================================================

-- | Nombre: opcionAnalisisTemporal
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Muestra submenu temporal y ejecuta la opcion seleccionada.
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

-- | Nombre: mesMayorMonto
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime el mes con mayor monto y el dia semanal mas activo.
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

-- | Nombre: eventoAntiguoReciente
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime el evento mas antiguo y el mas reciente.
eventoAntiguoReciente :: [Evento] -> IO ()
eventoAntiguoReciente eventos = do
  let (antiguo, reciente) = eventoMasAntiguoYReciente (eventosAFechaTupla eventos)
  putStrLn "--- Evento mas antiguo ---"
  imprimirEvento antiguo
  putStrLn "--- Evento mas reciente ---"
  imprimirEvento reciente

-- | Nombre: resumenPorIntervalo
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Solicita tamano de intervalo y muestra resumen por cada tramo.
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

-- | Nombre: opcionBusqueda
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Solicita rango de fechas y muestra los eventos encontrados.
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

-- | Nombre: opcionEstadisticas
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Muestra submenu de estadisticas y ejecuta la opcion elegida.
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

-- | Nombre: resumenGeneral
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Imprime resumen consolidado y opcionalmente exporta a CSV.
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
-- LECTURA DE EVENTOS DESDE ARCHIVO AL INICIAR
-- ============================================================

-- | Nombre: dividirCSV
-- Entrada: Linea de texto CSV simple separada por comas.
-- Funcionalidad o Salida: Divide la linea en campos sin manejar comillas escapadas.
dividirCSV :: String -> [String]
dividirCSV "" = [""]
dividirCSV (',':resto) = "" : dividirCSV resto
dividirCSV (c:resto) =
  let (primero:siguientes) = dividirCSV resto
  in (c : primero) : siguientes

-- | Nombre: parsearLineaEvento
-- Entrada: Linea de texto con campos de evento.
-- Funcionalidad o Salida: Convierte la linea a tupla tipada o Nothing si no coincide.
parsearLineaEvento :: String -> Maybe (Int, String, Double, String, Bool, Bool)
parsearLineaEvento linea =
  case dividirCSV linea of
    [idStr, cat, valStr, fec, altoStr, impStr] ->
      Just (read idStr, cat, read valStr, fec, read altoStr, read impStr)
    _ -> Nothing

-- | Nombre: lineaAEvento
-- Entrada: Tupla con campos parseados de un evento.
-- Funcionalidad o Salida: Construye un valor Evento desde la tupla.
lineaAEvento :: (Int, String, Double, String, Bool, Bool) -> Evento
lineaAEvento (eId, eCat, eVal, eFechaStr, eAlto, eImp) =
  let (año, mes, dia) = parseFecha eFechaStr
  in Evento
      { eventoId         = eId
      , categoria        = eCat
      , valor            = eVal
      , fecha            = fromIntegral año * 10000 + fromIntegral mes * 100 + fromIntegral dia
      , esAltoValor      = eAlto
      , impuestoAplicado = eImp
      }

-- | Nombre: lineasAEventos
-- Entrada: Lista de lineas CSV.
-- Funcionalidad o Salida: Parsea y convierte lineas validas a eventos.
lineasAEventos :: [String] -> [Evento]
lineasAEventos lineas =
  [ lineaAEvento tupla | linea <- lineas, Just tupla <- [parsearLineaEvento linea] ]

-- | Nombre: cargarEventosIniciales
-- Entrada: No recibe parametros.
-- Funcionalidad o Salida: Lee eventos desde archivo CSV si existe; si no, retorna lista vacia.
cargarEventosIniciales :: IO [Evento]
cargarEventosIniciales = do
  existe <- doesFileExist archivoEventos
  if not existe
    then return []
    else do
      contenido <- readFile archivoEventos
      let lineas = lines contenido
          lineasDatos = case lineas of
                          (h:t) | not (null h) && head h `notElem` "0123456789" -> t
                          _ -> lineas
      return (lineasAEventos lineasDatos)

-- ============================================================
-- GUARDAR EVENTOS AL SALIR
-- ============================================================

-- | Nombre: eventoACSV
-- Entrada: Un evento.
-- Funcionalidad o Salida: Convierte el evento a una linea CSV.
eventoACSV :: Evento -> String
eventoACSV eventoActual =
  show (eventoId eventoActual)          ++ "," ++
  categoria eventoActual                ++ "," ++
  show (valor eventoActual)             ++ "," ++
  formatearFecha (fecha eventoActual)   ++ "," ++
  show (esAltoValor eventoActual)       ++ "," ++
  show (impuestoAplicado eventoActual)

-- | Nombre: guardarTodosEventos
-- Entrada: Lista de eventos.
-- Funcionalidad o Salida: Escribe todos los eventos en archivo CSV con encabezado.
guardarTodosEventos :: [Evento] -> IO ()
guardarTodosEventos eventos = do
  let encabezado = "EventoId,Categoria,Valor,Fecha,EsAltoValor,ImpuestoAplicado"
      filas      = map eventoACSV eventos
      contenido  = encabezado ++ "\n" ++ unlines filas
  writeFile archivoEventos contenido