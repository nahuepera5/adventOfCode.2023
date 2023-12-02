import System.IO
import Data.Char
import Prelude

info :: IO ()
info = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          putStr c
          hClose f
          print ':'
          print ( mapSum ( reduceInt ( map getNums ( map replaceWords (words c) ) ) ) )

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

slice :: [Char] -> Int -> [Char]
slice = \ l n -> case l of {
    [] -> [];
    x:xs -> case (n > 0) of {
        False -> [];
        True -> x : (slice xs (n - 1));
    }
}

replaceWords :: [Char] -> [Char]
replaceWords = \ s -> case s of {
    [] -> [];
    x:xs -> (containsInitial s x) ++ [x] ++ (replaceWords xs);
}

containsInitial :: [Char] -> Char  -> [Char]
containsInitial = \ s c -> case c of {
    'o' -> case ( slice s 3  ) == "one" of {
        True -> "1";
        False -> "";
    };
    't' -> case ( slice s 3  ) == "two" of {
        True -> "2";
        False -> case ( slice s 5 ) == "three" of {
            True -> "3";
            False -> "";
        };
    };
    'f' -> case ( slice s 4 ) == "four" of {
        True -> "4";
        False -> case ( slice s 4 ) == "five" of {
            True -> "5";
            False -> "";
        };
    };
    's' -> case ( slice s 3 ) == "six" of {
        True -> "6";
        False -> case ( slice s 5 ) == "seven" of {
            True -> "7";
            False -> "";
        };
    };
    'e' -> case (slice s 5) == "eight" of {
        True -> "8";
        False -> "";
    };
    'n' -> case (slice s 4) == "nine" of {
        True -> "9";
        False -> "";
    };
    _ -> "";
}