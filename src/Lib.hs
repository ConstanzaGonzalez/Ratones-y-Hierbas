module Lib where
import Text.Show.Functions

laVerdad = True

type Enfermedades = [String]

data Raton = UnRaton {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: Enfermedades
} deriving (Eq, Show)

cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampion", "tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["alta obesidad", "sinusitis"]

hierbaBuena :: Raton -> Raton
hierbaBuena = modificarEdad rejuvenecerRaton

rejuvenecerRaton :: Raton -> Float
rejuvenecerRaton raton = sqrt (edad raton)

modificarEdad :: (Raton -> Float) -> Raton -> Raton
modificarEdad f raton = raton { edad = f raton}

hierbaVerde :: String -> Raton -> Raton
hierbaVerde tipoHierba = modificarEnfermedades ( eliminarEnfermedadesTipoHierba tipoHierba)

modificarEnfermedades :: (Raton -> Enfermedades) -> Raton -> Raton
modificarEnfermedades f raton = raton { enfermedades = f ( raton) }

eliminarEnfermedadesTipoHierba :: String -> Raton -> Enfermedades
eliminarEnfermedadesTipoHierba tipoHierba  = filter (esElTipoHierba tipoHierba) . enfermedades

esElTipoHierba :: String -> String -> Bool
esElTipoHierba tipoHierba enfermedad = tipoHierba /= (obtenerUltimasLetras tipoHierba enfermedad)

obtenerUltimasLetras :: String -> String -> String
obtenerUltimasLetras tipoHierba enfermedad = drop (length enfermedad - length tipoHierba) enfermedad


