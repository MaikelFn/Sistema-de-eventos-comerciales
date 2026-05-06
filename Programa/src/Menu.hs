module Menu (menuPrincipal) where

import OpcionesEventos
  ( Evento(..)
  , generarCantidadEventos
  , generarNEventos
  , opcionTransformacion
  , opcionAnalisisDatos
  , opcionAnalisisTemporal
  , opcionBusqueda
  , opcionEstadisticas
  , guardarTodosEventos
  , cargarEventosIniciales
  )

-- | Nombre: menuPrincipal
-- Entrada: Lista de eventos previos en memoria.
-- Funcionalidad o Salida: Carga eventos iniciales desde archivo,
-- los combina con los recibidos y arranca el menu principal.
menuPrincipal :: [Evento] -> IO ()
menuPrincipal eventosPrevios = do
  eventosArchivo <- cargarEventosIniciales
  let eventosIniciales = eventosPrevios ++ eventosArchivo
  if null eventosArchivo
    then putStrLn "Sin eventos previos en archivo. Iniciando sesion nueva."
    else putStrLn ("Eventos cargados del archivo: " ++ show (length eventosArchivo))
  menu eventosIniciales

-- | Nombre: menu
-- Entrada: Lista actual de eventos del sistema.
-- Funcionalidad o Salida: Muestra opciones en consola, procesa la opcion
-- seleccionada y repite hasta que el usuario decida salir.
menu :: [Evento] -> IO ()
menu eventos = do
  putStrLn ""
  putStrLn "========================================="
  putStrLn " Sistema de Eventos Comerciales"
  putStrLn "========================================="
  putStrLn ("Eventos en sistema: " ++ show (length eventos))
  putStrLn ""
  putStrLn "Seleccione una opcion:"
  putStrLn "1) Transformacion de eventos"
  putStrLn "2) Analisis de datos"
  putStrLn "3) Analisis temporal"
  putStrLn "4) Busqueda"
  putStrLn "5) Estadisticas"
  putStrLn "6) Salir"
  putStr "> "

  opcion <- getLine
  (continuar, eventosActualizados) <- opcionElegida opcion eventos

  if continuar
    then menu eventosActualizados
    else do
      guardarTodosEventos eventosActualizados
      putStrLn "Eventos guardados. Finalizando sistema..."

-- | Nombre: opcionElegida
-- Entrada: Opcion ingresada por el usuario y lista de eventos actual.
-- Funcionalidad o Salida: Ejecuta la accion correspondiente a la opcion
-- y devuelve si el menu debe continuar junto con la lista actualizada.
opcionElegida :: String -> [Evento] -> IO (Bool, [Evento])
opcionElegida opcion eventos =
  case opcion of
    "1" -> do
      eventosActualizados <- actualizarEventosEnAcceso eventos
      nuevosEventos <- opcionTransformacion eventosActualizados
      return (True, nuevosEventos)

    "2" -> do
      eventosActualizados <- actualizarEventosEnAcceso eventos
      opcionAnalisisDatos eventosActualizados
      return (True, eventosActualizados)

    "3" -> do
      eventosActualizados <- actualizarEventosEnAcceso eventos
      opcionAnalisisTemporal eventosActualizados
      return (True, eventosActualizados)

    "4" -> do
      eventosActualizados <- actualizarEventosEnAcceso eventos
      opcionBusqueda eventosActualizados
      return (True, eventosActualizados)

    "5" -> do
      eventosActualizados <- actualizarEventosEnAcceso eventos
      opcionEstadisticas eventosActualizados
      return (True, eventosActualizados)

    "6" -> return (False, eventos)

    _ -> do
      putStrLn "Opcion invalida. Intente de nuevo."
      return (True, eventos)

-- | Nombre: actualizarEventosEnAcceso
-- Entrada: Lista de eventos actual.
-- Funcionalidad o Salida: Genera una cantidad aleatoria de nuevos eventos,
-- evita IDs repetidos y retorna la lista ampliada.
actualizarEventosEnAcceso :: [Evento] -> IO [Evento]
actualizarEventosEnAcceso eventos = do
  cantidadNuevos <- generarCantidadEventos
  let idsUsados = map eventoId eventos
  nuevosEventos <- generarNEventos cantidadNuevos idsUsados
  return (eventos ++ nuevosEventos)