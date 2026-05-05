module Archivos
  ( ResumenGeneral(..)
  , exportarResumenCSV
  ) where

-- | Estructura: ResumenGeneral
-- Descripcion: Agrupa los datos consolidados del analisis
-- de eventos para poder exportarlos en formato CSV.
data ResumenGeneral = ResumenGeneral
  { conteosCategorias :: [(String, Int)]
  , eventoMaximo :: (Int, String, Double, String)
  , eventoMinimo :: (Int, String, Double, String)
  , fechaMasActividad :: (String, Int)
  } deriving (Show)

-- | Nombre: exportarResumenCSV
-- Entrada: Ruta del archivo destino y estructura 'ResumenGeneral'.
-- Funcionalidad o Salida: Genera el contenido CSV del resumen,
-- lo escribe en disco y muestra un mensaje de confirmacion.
exportarResumenCSV :: FilePath -> ResumenGeneral -> IO ()
exportarResumenCSV rutaArchivo resumen = do
  let contenido = formatearCSV resumen
  writeFile rutaArchivo contenido
  putStrLn ("Archivo exportado exitosamente: " ++ rutaArchivo)

-- | Nombre: formatearCSV
-- Entrada: Estructura 'ResumenGeneral'.
-- Funcionalidad o Salida: Combina en un solo String CSV las secciones
-- de categorias, evento maximo, evento minimo y fecha con mayor actividad.
formatearCSV :: ResumenGeneral -> String
formatearCSV resumen =
  let categoriasCsv = formatearCategoriesCSV (conteosCategorias resumen)
      maxEventoCsv = formatearEventoCSV "Evento con Monto Maximo" (eventoMaximo resumen)
      minEventoCsv = formatearEventoCSV "Evento con Monto Minimo" (eventoMinimo resumen)
      fechaCsv = formatearFechaCSV (fechaMasActividad resumen)
  in categoriasCsv ++ "\n" ++ maxEventoCsv ++ "\n\n" ++ minEventoCsv ++ "\n\n" ++ fechaCsv

-- | Nombre: formatearCategoriesCSV
-- Entrada: Lista de pares (categoria, cantidad).
-- Funcionalidad o Salida: Devuelve texto CSV con encabezado y filas
-- para el conteo de eventos por categoria.
formatearCategoriesCSV :: [(String, Int)] -> String
formatearCategoriesCSV categorias =
  let encabezado = "Categoria,Cantidad de Eventos"
      filas = map (\(categoria, cantidad) -> categoria ++ "," ++ show cantidad) categorias
  in encabezado ++ "\n" ++ unlines filas

-- | Nombre: formatearEventoCSV
-- Entrada: Etiqueta descriptiva y una tupla (id, categoria, valor, fecha).
-- Funcionalidad o Salida: Devuelve el bloque CSV de un evento destacado,
-- incluyendo etiqueta, encabezado y fila de datos.
formatearEventoCSV :: String -> (Int, String, Double, String) -> String
formatearEventoCSV etiqueta (id, categoria, valor, fecha) =
  etiqueta ++ "\n" ++
  "ID,Categoria,Valor,Fecha\n" ++
  show id ++ "," ++
  categoria ++ "," ++
  show valor ++ "," ++
  fecha

-- | Nombre: formatearFechaCSV
-- Entrada: Tupla (fecha, cantidad).
-- Funcionalidad o Salida: Devuelve una linea CSV con la fecha
-- que tuvo mayor actividad y su cantidad de eventos.
formatearFechaCSV :: (String, Int) -> String
formatearFechaCSV (fecha, cantidad) =
  "Dia con Mayor Actividad,Cantidad de Eventos\n" ++
  fecha ++ "," ++ show cantidad