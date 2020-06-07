module Lib where
import Text.Show.Functions

laVerdad = True

type Enfermedades = [String]
type Hierba = Raton -> Raton

data Raton = UnRaton {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: Enfermedades
} deriving (Eq, Show)

cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampion", "tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["alta obesidad", "sinusitis"]

hierbaBuena :: Hierba
hierbaBuena = modificarEdad rejuvenecerRaton

rejuvenecerRaton :: Raton -> Float
rejuvenecerRaton raton = sqrt (edad raton)

modificarEdad :: (Raton -> Float) -> Raton -> Raton
modificarEdad f raton = raton { edad = f raton}

hierbaVerde :: String -> Hierba
hierbaVerde tipoHierba = modificarEnfermedades ( eliminarEnfermedadesTipoHierba tipoHierba)

modificarEnfermedades :: (Raton -> Enfermedades) -> Raton -> Raton
modificarEnfermedades f raton = raton { enfermedades = f ( raton) }

eliminarEnfermedadesTipoHierba :: String -> Raton -> Enfermedades
eliminarEnfermedadesTipoHierba tipoHierba  = filter (esElTipoHierba tipoHierba) . enfermedades

esElTipoHierba :: String -> String -> Bool
esElTipoHierba tipoHierba enfermedad = tipoHierba /= (obtenerUltimasLetras tipoHierba enfermedad)

obtenerUltimasLetras :: String -> String -> String
obtenerUltimasLetras tipoHierba enfermedad = drop (length enfermedad - length tipoHierba) enfermedad


alcachofa :: Hierba
alcachofa = modificarPeso reducirPesoPorcentaje

modificarPeso :: (Raton -> Float) -> Raton -> Raton
modificarPeso f raton = raton { peso = f raton}

reducirPesoPorcentaje :: Raton -> Float
reducirPesoPorcentaje raton 
            | peso raton > 2 = peso raton - peso raton * 0.1
            | otherwise = peso raton - peso raton * 0.05

hierbaZort :: Hierba
hierbaZort = modificarEdad perderEdad . modificarEnfermedades perderEnfermedades

perderEdad :: Raton -> Float
perderEdad raton = 0

perderEnfermedades :: Raton -> Enfermedades
perderEnfermedades raton = []

hierbaDelDiablo :: Hierba
hierbaDelDiablo = modificarPeso reducirPeso

reducirPeso :: Raton -> Float
reducirPeso raton 
        | (peso raton - 0.1 ) < 0 = 0
        | otherwise = peso raton - 0.1

type Medicamento = [Hierba]

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

aplicarMedicamento :: Medicamento -> Raton-> Raton
aplicarMedicamento medicamento raton  = foldl aplicar raton medicamento

aplicar :: Raton -> Hierba -> Raton
aplicar raton hierba = hierba raton

reduceFatFast :: Int -> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ replicate potencia alcachofa

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

listaInfinita = iterate (+1) 1 

lista = [1,2,3,4,5, 6]

cantidadIdeal ::  (Int -> Bool) -> Int
cantidadIdeal condicion =  encontrarIdeal condicion lista

encontrarIdeal :: (Int -> Bool) -> [Int] -> Int
encontrarIdeal condicion [] = 0
encontrarIdeal condicion (cabeza:cola) 
                        | condicion cabeza = cabeza
                        |otherwise = encontrarIdeal condicion cola

lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento  = all esEstable . map (aplicarMedicamento medicamento) 

esEstable :: Raton -> Bool
esEstable raton = peso raton > 1 && length(enfermedades raton) < 3

--5)
--a) No se puede, por que si todos estan estabilizados el all de una lista infinita no terminaria nunca
--b) Si se puede, por que va a cortar cuando verifique que el raton no esta estable  

--6)
-- a) Hay que crear una funcion con el nombre de la nuerva hierba y utilizar las funciones existentes 
-- como modificar edad , peso, etc. No habria que modificar ninguna funcion
-- b) Generalizamos las funciones para no repetir codigo y utilizamos las funciones como valores
-- c) Habria que modigicar reducirPesoPorcentaje, reducirPeso y esEstable


