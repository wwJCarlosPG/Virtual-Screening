module DSLFunc
where
import System.Directory
import Data.List
import Data.List (intercalate)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe
import Data.Maybe (fromMaybe)
import Data.Char
import Prelude
import Data.Function.Memoize
import DSLData (hidrophobAA)

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
{-devuevle la cantidad de caracteres repetidos que hay en ambas cadenas-}
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
simple es muy lento para grandes cadenas, incluso este no es muy recomendable usarlo.-}
levenshteinM :: String -> String -> Int
levenshteinM xs ys = table m n
  where
    m = length xs
    n = length ys
    table = memoize2 dist
    dist 0 j = j
    dist i 0 = i
    dist i j
      | xs !! (i-1) == ys !! (j-1) = table (i-1) (j-1)
      | otherwise = minimum [table (i-1) j, table i (j-1), table (i-1) (j-1)] + 1

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
getTuples:: String -> String->[Int]->[(String, Int)]    
getTuples path bs resultsOfSF = 
  result   
  where             
    strings = idsWoutBS path bs 
    tuples = zip strings resultsOfSF
    sortedTuples = sortBy compareTuples tuples  
    result =  sortedTuples

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
compareTuples :: (a,Int) -> (a, Int) -> Ordering
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

{-filtra las proteinas, recibe el file y la funcion de filtrado-}
filterProteins:: String->(Int->Bool)->[String]
filterProteins file comp = 
  let proteins = getOnlyProteins (listOfLines file)
      ids = getOnlyIds (listOfLines file)
      tup = zip ids proteins
      filteredTup = filterTuple comp  tup
      filteredIds = map fst filteredTup
  in filteredIds

{-filtra igual pero devuelve la tupla-}
filterTuplesToResponse::String->(Int->Bool)->[(String, String)]
filterTuplesToResponse file cond = 
  let proteins = getOnlyProteins (listOfLines file)
      ids = getOnlyIds (listOfLines file)
      tup = zip ids proteins
      filteredTup = filterTuple cond tup
  in filteredTup

{-Filtra la lista de tuplas (String, String) segun el tamanno del segundo elemento de la tupla.-}
filterTuple :: (Int -> Bool) -> [(String, String)] -> [(String, String)]
filterTuple r xs = [t | t@(_, b) <- xs, r (length b)]

sortedHidrophob::String->[String]
sortedHidrophob file =
  let prot = getOnlyProteins (listOfLines file)
      ids = getOnlyIds (listOfLines file)
      hidrophobIn = map hidrophobIndex prot
      tup = zip ids hidrophobIn
      sortedTup = sortBy compareTuples tup
      res = map fst sortedTup
  in res

{-Recibe el file y la funcion de ordenacion y devuelve la coleccion de ids ordenada-}
sortingFunc::String-> (String->Int)->[String]
sortingFunc file sFunc = 
  let prot = getOnlyProteins (listOfLines file)
      ids = getOnlyIds (listOfLines file)
      sItems = map sFunc prot
      tup = zip ids sItems
      sortedTup = sortBy compareTuples tup
      res = map fst sortedTup
  in res

{-Recibe el file y la funcion de ordenacion y devuelve la coleccion de tupalas id-prot-}
sortingFuncTup::String->(String->Int)->[(String, String)]
sortingFuncTup file sFunc = 
  let prot = getOnlyProteins (listOfLines file)
      ids = getOnlyIds (listOfLines file)
      sItems = map sFunc prot
      tup = zip (zip ids prot) sItems
      sortedTup = sortBy compareTuples tup
      res = map fst sortedTup
  in res

{-Funcion de ordenacion por hidrofobicidad-}
hidrophobIndex::String->Int
hidrophobIndex [] = 0
hidrophobIndex (x:xs) | elem x hidrophobAA == True = 1 + hidrophobIndex xs
                      | otherwise = hidrophobIndex xs

writeWithSpace :: String -> [(String,String)]->IO()
writeWithSpace path tup = writeFile path formattedTuples
  where
    formattedTuples = intercalate "\n" (map formatTuple tup)
    formatTuple (x,y) = x ++ "\n" ++y++"\n"


 -- FIN DE FUNCIONES AUXILIARES

