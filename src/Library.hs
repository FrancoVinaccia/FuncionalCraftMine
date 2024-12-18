module Library where
import PdePreludat
import GHC.IOArray (unsafeReadIOArray)

type Material = String

data Personaje = UnPersonaje {
    nombre :: String,
    puntaje :: Number,
    inventario :: [Material]
} deriving (Show, Eq)

data Receta = UnaReceta {
    materiales :: [Material],
    tiempo :: Number,
    resultado :: Material
} deriving (Show, Eq)

recetaFogata :: Receta
recetaFogata = UnaReceta ["madera", "fosforo"] 10 "fogata"

recetaPolloAsado :: Receta
recetaPolloAsado = UnaReceta ["pollo", "fogata"] 300 "pollo asado"

recetaSueter :: Receta
recetaSueter = UnaReceta ["lana", "agujas", "tintura"] 600 "sueter"

fogata,fosforo, madera,polloAsado,pollo,sueter :: Material
fogata = "fogata"
fosforo = "fosforo"
madera = "madera"
pollo = "pollo"
polloAsado = "pollo asado"
sueter = "sueter"

juan, maria :: Personaje
juan = UnPersonaje "juan" 20 [madera, fosforo, pollo, sueter]
maria = UnPersonaje "maria" 1000 [fogata, pollo, pollo, sueter]

unasRecetas :: [Receta]
unasRecetas = [recetaFogata, recetaPolloAsado]

intentarCraftear :: Personaje -> Receta -> Personaje
intentarCraftear personaje receta
    | tieneMateriales personaje receta = craftear receta personaje
    | otherwise = personaje

tieneMateriales :: Personaje -> Receta -> Bool
tieneMateriales personaje receta = all (tieneMaterial personaje) (materiales receta)

tieneMaterial :: Personaje -> Material -> Bool
tieneMaterial personaje material = elem material (inventario personaje)

craftear ::  Receta -> Personaje -> Personaje
craftear receta = agregarResultado(resultado receta) . quitarMateriales(materiales receta) . sumarPuntaje(10*tiempo receta)

agregarResultado :: Material -> Personaje -> Personaje
agregarResultado material personaje = personaje { inventario = material : inventario personaje }

quitarMateriales :: [Material] -> Personaje -> Personaje
quitarMateriales materiales personaje = personaje { inventario = foldr quitarMaterial (inventario personaje) materiales }

quitarMaterial :: Eq a => a -> [a] -> [a]
quitarMaterial  _ [] = []
quitarMaterial material (x:xs)
    | material == x = xs
    | otherwise = x : quitarMaterial material xs

sumarPuntaje :: Number -> Personaje -> Personaje
sumarPuntaje cant personaje = personaje { puntaje = cant + puntaje personaje }

crateablesDuplicadores :: [Receta] -> Personaje -> [Material]
crateablesDuplicadores recetas personaje = map resultado (filter (duplicaPuntaje personaje) recetas)

duplicaPuntaje :: Personaje -> Receta -> Bool
duplicaPuntaje personaje receta = puntaje personaje * 2 < puntaje (craftear receta personaje)

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente = foldl intentarCraftear 

masPuntosAlReves :: [Receta] -> Personaje -> Bool
masPuntosAlReves recetas personaje = puntaje (craftearSucesivamente personaje recetas) < puntaje (craftearSucesivamente personaje (reverse recetas))