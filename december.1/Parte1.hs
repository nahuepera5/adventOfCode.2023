import System.IO
import Data.Char
import Prelude

info :: IO ()
info = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          putStr c
          hClose f
          print ':'
          print ( mapSum ( reduceInt ( map getNums (words c) ) ) )

getNums :: [Char] -> [Char]
getNums = \ s -> case s of {
    [] -> [];
    x:xs -> case x of {
        '0' -> '0' : (getNums xs);
        '1' -> '1' : (getNums xs);
        '2' -> '2' : (getNums xs);
        '3' -> '3' : (getNums xs);
        '4' -> '4' : (getNums xs);
        '5' -> '5' : (getNums xs);
        '6' -> '6' : (getNums xs);
        '7' -> '7' : (getNums xs);
        '8' -> '8' : (getNums xs);
        '9' -> '9' : (getNums xs);
        _ -> (getNums xs)
    };
}

reduceInt :: [[Char]] -> [Int]
reduceInt = \ strings -> case strings of {
    [] -> [];
    string:resto -> case string of {
        [] -> 0 : (reduceInt resto);
        caracter:caracteres -> case caracteres of {
            [] -> (read (caracter : [caracter]) :: Int) : (reduceInt resto);
            x:xs -> (read ( caracter : [(last caracteres)] ) ::Int) : (reduceInt resto) 
        }
    } 
}

mapSum :: [Int] -> Int
mapSum = \ l -> case l of {
    [] -> 0;
    x:xs -> x + mapSum xs;
}