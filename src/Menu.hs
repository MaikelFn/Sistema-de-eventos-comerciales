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
  )

-- Menu principal recurrente
-- Entrada: lista de eventos actuales en memoria
-- Salida: IO () — termina al elegir Salir
menuPrincipal :: [Evento] -> IO ()
menuPrincipal eventos = do
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
    then menuPrincipal eventosActualizados
    else putStrLn "Finalizando sistema..."

-- Ejecuta la opcion elegida
-- Retorna (continuar, lista actualizada)
opcionElegida :: String -> [Evento] -> IO (Bool, [Evento])
opcionElegida opcion eventos =
  case opcion of
    "1" -> do
      eventosActualizados <- actualizarEventosEnAcceso eventos
      opcionTransformacion eventosActualizados
      return (True, eventosActualizados)
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
    _   -> do
      putStrLn "Opcion invalida. Intente de nuevo."
      return (True, eventos)

actualizarEventosEnAcceso :: [Evento] -> IO [Evento]
actualizarEventosEnAcceso eventos = do
  cantidadNuevos <- generarCantidadEventos
  let idsUsados = map eventoId eventos
  nuevosEventos <- generarNEventos cantidadNuevos idsUsados
  return (eventos ++ nuevosEventos)
