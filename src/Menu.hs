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

menuPrincipal :: [Evento] -> IO ()
menuPrincipal eventosPrevios = do
  eventosArchivo <- cargarEventosIniciales
  let eventosIniciales = eventosPrevios ++ eventosArchivo
  if null eventosArchivo
    then putStrLn "Sin eventos previos en archivo. Iniciando sesion nueva."
    else putStrLn ("Eventos cargados del archivo: " ++ show (length eventosArchivo))
  menu eventosIniciales

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

actualizarEventosEnAcceso :: [Evento] -> IO [Evento]
actualizarEventosEnAcceso eventos = do
  cantidadNuevos <- generarCantidadEventos
  let idsUsados = map eventoId eventos
  nuevosEventos <- generarNEventos cantidadNuevos idsUsados
  return (eventos ++ nuevosEventos)