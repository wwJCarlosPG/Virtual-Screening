module DSLFunc
where
import System.Directory
import Data.List
import System.IO.Unsafe
import Data.Maybe (fromMaybe)
import Data.Char
import Prelude
import Data.Function.Memoize



--TRABAJO CON LAS LISTAS Y LOS TXTs
{-devuelve la lista de lineas(cadenas) dado una direccion en forma de cadena(String)
es inseguro porque si modifico el archivo se modifica esto(tiene efectos colaterales.) -}
listOfLines :: String -> [String]           
listOfLines path = lines content where content = unsafePerformIO $ readFile path



{-limpia la lista para que no compare con la diana dada el id de la diana y el texto de lineas-}
cleanList::String->[String]->[String]   
cleanList _ [] = []
cleanList item (x:xs) | item == x = cleanList item xs
                           |otherwise = [x]++cleanList item xs

{-devuelve la lista de las proteinas sin la diana pasandole la direccion y la diana-}
proteinsWoutBS::String->String->[String]
proteinsWoutBS path bs  =
    result
    where
      l = getOnlyProteins (listOfLines path)
      sequenceBs = getOnlyProteins (listOfLines path) !! getInt (getIndex bs (getOnlyIds (listOfLines path)))
      result = cleanList sequenceBs l

{-devuelve la lista de los ids sin la diana pasandole la direccion y la diana-}
idsWoutBS::String->String->[String]
idsWoutBS path bs  =
    result
    where
      l = getOnlyIds (listOfLines path)
      result = cleanList bs l

{-devuelve solo las cadenas de proteinas dado el texto de lineas-}
getOnlyProteins :: [String] -> [String]    
getOnlyProteins [] = []
getOnlyProteins [x] = [x]
getOnlyProteins [x,y] = [y]
getOnlyProteins (x1:x2:x3:xs)  = [x2] ++ getOnlyProteins xs
                           
{-devuelve una lista con los ids de las proteinas dado el texto de lineas-}
getOnlyIds :: [String] -> [String]    
getOnlyIds [] = []
getOnlyIds [x] = [x]
getOnlyIds [x,y] = [x]    
getOnlyIds (x1:x2:x3:xs) = [x1] ++ getOnlyIds xs   
--FIN DE TRABAJO CON LAS LISTAS Y TXTs








--FUNCIONES DE SIMILITUD
{-devuevle el tamanno de la subcadena maxima comun-}
lcs :: String->String->Int    
lcs [] _ = 0
lcs _ [] = 0
lcs (x:xs) (y:ys) | x==y = 1 + lcs xs ys
                |otherwise = max (lcs (x:xs) ys) (lcs xs (y:ys))


{-funcion auxiliar para occurrences, cuenta la cantidad de veces que se repite un caracter en una cadena-}
countLetters :: String -> Char -> Int    
countLetters str c = length $ filter (== c) str

{-calcula el numero de transformaciones elementales(insertar, borrar, sustituir) que
hay que hacerle a una cadena para convertirla en otra-}
levenshtein::String->String->Int
levenshtein [] [] = 0
levenshtein [] ys = length ys
levenshtein xs [] = length xs
levenshtein (x:xs) (y:ys) |x==y = levenshtein xs ys
                          | otherwise = 1 + minimum [levenshtein xs (y:ys), levenshtein (x:xs) ys, levenshtein xs ys]

{-calcula el numero de transformaciones elementales que hay que hacerle a una
cadena para convertirla en otra, pero en esta caso se usa memoization ya que el metodo
simple es muy lento para grandes cadenas.-}
levenshteinM :: String->String->Int
levenshteinM = memoize2 levenshteinM'
  where
    levenshteinM' [] ys = length ys
    levenshteinM' xs [] = length xs
    levenshteinM' (x:xs) (y:ys)
      | x == y = levenshteinM xs ys
      | otherwise = minimum [1 + levenshteinM xs (y:ys),
                              1 + levenshteinM (x:xs) ys,
                              1 + levenshteinM xs ys]

{-devuelve la diferencia de valores numericos de cada caracter y si hay espacio en blanco le suma 1 a ese valor
Ejemplo: "ABCD" "ABC" devuelve 1
"ABCX" "ABCD" devuelve 21-}
charsDistance::String->String->Int
charsDistance [] _ = 1
charsDistance _ [] = 1
charsDistance (x:xs) (y:ys) = (abs ((getASCII x) - (getASCII y))) + charsDistance xs ys  
      
{-funcion de similitud que devuelve la cantidad de ocurrencias de cada caracter de la cadena en otra cadena
usando la funcion auxiliar countLetters
Nota: Se cuentan los caracteres repetidos
Ejemplo: "ABCA" "AAXCA" devolveria 7, ya que hay 3 As en la segunda cadena y 2As en la primera
mas una C, por lo que daria 3 * 2 + 1 = 7 -}
occurrences :: String -> String -> Int   
occurrences [] _ = 0
occurrences _ [] = 0
occurrences (x:xs) ys =  countLetters ys x + occurrences xs ys 
--FIN DE FUNCIONES DE SIMILITUD





--FUNCIONES PRINCIPALES
{-a partir de una lista de cadenas y otra de enteros escribe tuplas en un txt con los respectivos valores
en este caso recibiria la lista de resultados de la funcion de similitud y la lista de los ids
se pasan en este orden:
Archivo para cargar la info(A)
Id de la proteina diana(D)
Array de enteros que seria simFunc(funcion de similitud, (A), (D))
Nombre del archivo donde se guardaran los resultados(ejemplo: "archivo", sin la extension)
-}
getTuples:: String -> String->[Int]->String->IO()    
getTuples path bs resultsOfSF nameOfFile = do                  
   let strings = idsWoutBS path bs 
   let tuples = zip strings resultsOfSF
   let sortedTuples = sortBy compareTuples tuples  
   let strRes = map show sortedTuples
   writeFile (nameOfFile++".txt") (unlines strRes)

{-Recibe una funcion de similitud, la direccion donde esta el txt
y la diana y devuelve una lista de enteros, donde cada entero es el resultado de aplicar la funcion de similitd
con la diana y el elemento correspondiente(la diana no se compara consigo misma)-}
simFunc:: (String->String->Int)->String -> String -> [Int]                                                
simFunc sf path bs = 
   result 
   where 
      protein = getOnlyProteins (listOfLines path) !! getInt (getIndex bs (getOnlyIds (listOfLines path)))
      list = proteinsWoutBS path bs
      result = getSimFunc sf protein list 
--FIN DE FUNCIONES PRINCIPALES





--FUNCIONES AUXILIARES
{-Devuelve el indice de un elemento en una lista dada-}
getIndex :: Eq a => a -> [a] -> Maybe Int
getIndex item list = elemIndex item list

{--}
compareTuples :: (String,Int) -> (String, Int) -> Ordering
compareTuples (_,x) (_,y) = compare x y

{-Retorna la suma de todos los valores numericos de los caracteres-}
getAllASCII::String->Int
getAllASCII "" = 0
getAllASCII (x:xs) = getASCII x + getAllASCII xs

{-Convierte de Maybe Int a Int-}
getInt :: Maybe Int -> Int
getInt maybeInt = fromMaybe 0 maybeInt

{-Funcion auxiliar para simFunc que devuelve la lista de enteros despues de decodificar los argumentos que 
recibe simFunc-}
getSimFunc:: (String->String->Int)->String->[String]->[Int]
getSimFunc sf _ [] = []
getSimFunc sf bs (x:xs) = [sf bs x]++getSimFunc sf bs xs

{-Retorna el valor numerico del caracter dado en la tabla ASCII-}
getASCII:: Char->Int
getASCII c = (ord c)

{-Filtra la lista de tuplas (IdProteina, Proteina) segun el length de la proteina
aplicado a la funcion de comparacion-}
filterProteins:: String->(Int->Bool)->[String]
filterProteins file comp = 
  let proteins = getOnlyProteins (listOfLines file)
      ids = getOnlyIds (listOfLines file)
      tup = zip ids proteins
      filteredTup = filterTuple comp  tup
      filteredIds = map fst filteredTup
  in filteredIds

{-Filtra la lista de tuplas (String, String) segun el tamanno del segundo elemento de la tupla.-}
filterTuple :: (Int -> Bool) -> [(String, String)] -> [(String, String)]
filterTuple r xs = [t | t@(_, b) <- xs, r (length b)]

 -- FIN DE FUNCIONES AUXILIARES







--TESTS
substrings :: String -> [String]
substrings str = [substring i j str | i <- [0..length str], j <- [i..length str]]
  where substring i j s = take (j - i) (drop i s)  --con take tomo los primeros j-i caracters

maxSubstringLength :: String -> String -> Int
maxSubstringLength str1 str2 = maximum [length s | i <- [0..length str1], j <- [i..length str1], let s = substring i j str1, isInfixOf s str2]
  where substring i j s = take (j - i) (drop i s)


