module Archivos
  ( ResumenGeneral(..)
  , exportarResumenCSV
  ) where

data ResumenGeneral = ResumenGeneral
  { conteosCategorias :: [(String, Int)]
  , eventoMaximo :: (Int, String, Double, String)
  , eventoMinimo :: (Int, String, Double, String)
  , fechaMasActividad :: (String, Int)
  } deriving (Show)

exportarResumenCSV :: FilePath -> ResumenGeneral -> IO ()
exportarResumenCSV rutaArchivo resumen = do
  let contenido = formatearCSV resumen
  writeFile rutaArchivo contenido
  putStrLn ("Archivo exportado exitosamente: " ++ rutaArchivo)

formatearCSV :: ResumenGeneral -> String
formatearCSV resumen =
  let categoriasCsv = formatearCategoriesCSV (conteosCategorias resumen)
      maxEventoCsv = formatearEventoCSV "Evento con Monto Maximo" (eventoMaximo resumen)
      minEventoCsv = formatearEventoCSV "Evento con Monto Minimo" (eventoMinimo resumen)
      fechaCsv = formatearFechaCSV (fechaMasActividad resumen)
  in categoriasCsv ++ "\n" ++ maxEventoCsv ++ "\n\n" ++ minEventoCsv ++ "\n\n" ++ fechaCsv

formatearCategoriesCSV :: [(String, Int)] -> String
formatearCategoriesCSV categorias =
  let encabezado = "Categoria,Cantidad de Eventos"
      filas = map (\(categoria, cantidad) -> categoria ++ "," ++ show cantidad) categorias
  in encabezado ++ "\n" ++ unlines filas

formatearEventoCSV :: String -> (Int, String, Double, String) -> String
formatearEventoCSV etiqueta (id, categoria, valor, fecha) =
  etiqueta ++ "\n" ++
  "ID,Categoria,Valor,Fecha\n" ++
  show id ++ "," ++
  categoria ++ "," ++
  show valor ++ "," ++
  fecha

formatearFechaCSV :: (String, Int) -> String
formatearFechaCSV (fecha, cantidad) =
  "Dia con Mayor Actividad,Cantidad de Eventos\n" ++
  fecha ++ "," ++ show cantidad
