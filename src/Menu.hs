module Menu (menuPrincipal) where

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
      putStrLn "[Pendiente] Transformacion de eventos"
      return False
    "2" -> do
      putStrLn "[Pendiente] Analisis de datos"
      return False
    "3" -> do
      putStrLn "[Pendiente] Analisis temporal"
      return False
    "4" -> do
      putStrLn "[Pendiente] Busqueda por rango de fechas"
      return False
    "5" -> do
      putStrLn "[Pendiente] Estadisticas"
      return False
    "6" -> return True
    _ -> do
      putStrLn "Opcion invalida. Intente de nuevo."
      return False
