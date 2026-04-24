module Menu (menuPrincipal) where

import OpcionesEventos
  ( opcionTransformacion
  , opcionAnalisisDatos
  , opcionAnalisisTemporal
  , opcionBusqueda
  , opcionEstadisticas
  )

menuPrincipal :: IO ()
menuPrincipal = do
  putStrLn "========================================="
  putStrLn " Sistema de Eventos Comerciales"
  putStrLn "========================================="
  putStrLn ""
  putStrLn "Seleccione una opcion:"
  putStrLn "1) Transformacion de eventos"
  putStrLn "2) Analisis de datos"
  putStrLn "3) Analisis temporal"
  putStrLn "4) Busqueda"
  putStrLn "5) Estadisticas"
  putStrLn "6) Salir"
  putStr "> "

  option <- getLine
  shouldExit <- opcionElegida option

  if shouldExit
    then putStrLn "Finalizando sistema..."
    else menuPrincipal

opcionElegida :: String -> IO Bool
opcionElegida option =
  case option of
    "1" -> do
      opcionTransformacion
      return False
    "2" -> do
      opcionAnalisisDatos
      return False
    "3" -> do
      opcionAnalisisTemporal
      return False
    "4" -> do
      opcionBusqueda
      return False
    "5" -> do
      opcionEstadisticas
      return False
    "6" -> return True
    _ -> do
      putStrLn "Opcion invalida. Intente de nuevo."
      return False
