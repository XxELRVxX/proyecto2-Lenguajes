module Main where

import System.Random   -- para generar números aleatorios
import Data.List       -- para operaciones con listas

-- ============================================================
-- TIPO DE DATO: Evento
-- Representa una acción ocurrida en la plataforma de comercio.
-- Campos:
--   eventoId  : identificador único del evento (entero)
--   categoria : tipo de evento (texto)
--   valor     : monto asociado al evento (decimal)
--   timestamp : momento en que ocurrió el evento (entero)
-- ============================================================
data Evento = Evento
    { eventoId  :: Int
    , categoria :: String
    , valor     :: Float
    , timestamp :: Int
    } deriving (Show, Eq)


-- ============================================================
-- FUNCIÓN PRINCIPAL: main
-- Punto de entrada del programa.
-- ============================================================
main :: IO ()
main = do
    putStrLn "Sistema de Eventos Comerciales"
    putStrLn "Evento de prueba:"
    let evento1 = Evento 1 "compra" 15000.0 1746123456
    print evento1
