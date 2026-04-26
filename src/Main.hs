module Main where

import Menu (menuPrincipal)
import OpcionesEventos (Evento(..))

eventosPrueba :: [Evento]
eventosPrueba =
  [ Evento 1001 "Compra" 15000.0 "2026-01-15" False
  , Evento 1002 "Visualizacion" 0.0 "2026-01-16" False
  , Evento 1003 "Apartado" 5000.0 "2026-02-10" False
  , Evento 1004 "Compra" 25000.0 "2026-02-12" False
  , Evento 1005 "Devolucion"  (-8000.0)  "2026-03-01" False
  , Evento 1006 "Seguimiento" 0.0 "2026-03-05" False
  , Evento 1007 "Compra" 72000.0 "2027-04-20" False
  , Evento 1008 "Apartado" 12000.0 "2027-04-22" False
  , Evento 1009 "Compra" 30000.0 "2027-05-10" False
  , Evento 1010 "Devolucion" (-15000.0) "2027-05-11" False
  , Evento 1011 "Visualizacion" 0.0 "2028-06-01" False
  , Evento 1012 "Compra" 45000.0 "2028-06-03" False
  ]

main :: IO ()
main = menuPrincipal eventosPrueba
