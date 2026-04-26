module Menu (menuPrincipal) where

import OpcionesEventos
  ( Evento
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
  resultado <- opcionElegida opcion eventos

  case resultado of
    Nothing              -> putStrLn "Finalizando sistema..."
    Just eventosActuales -> menuPrincipal eventosActuales

-- Ejecuta la opcion elegida y retorna la lista de eventos sin cambios
-- Retorna Nothing si se elige Salir
opcionElegida :: String -> [Evento] -> IO (Maybe [Evento])
opcionElegida opcion eventos =
  case opcion of
    "1" -> do
      opcionTransformacion eventos
      return (Just eventos)
    "2" -> do
      opcionAnalisisDatos eventos
      return (Just eventos)
    "3" -> do
      opcionAnalisisTemporal eventos
      return (Just eventos)
    "4" -> do
      opcionBusqueda eventos
      return (Just eventos)
    "5" -> do
      opcionEstadisticas eventos
      return (Just eventos)
    "6" -> return Nothing
    _   -> do
      putStrLn "Opcion invalida. Intente de nuevo."
      return (Just eventos)
