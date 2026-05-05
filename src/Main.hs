{-# OPTIONS_GHC -Wno-tabs #-}
module Main where

import Menu (menuPrincipal)

-- | Nombre: main
-- Entrada: No recibe parametros directos.
-- Funcionalidad o Salida: Inicia la aplicacion ejecutando el
-- menu principal con una lista vacia de eventos.
main :: IO ()
main = do
	menuPrincipal []