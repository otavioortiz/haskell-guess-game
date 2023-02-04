module Graphics ( 
   drawBoard
) where 

import Text.Read (Lexeme(String))
import Data.List
import Data.Char ( chr, ord )
import Data.Binary.Builder (append)

drawBoard :: [Int] -> Int -> Int -> IO()
drawBoard dataList attempts score = do
    drawPlayerData attempts score
    drawBoardLetters
    drawBoardLine dataList
    print "                                                                                    "
    print "-> Check in the panel and type a letter that is not already selected and click enter"

drawBoardLine :: [Int] -> IO()
drawBoardLine list = do
    let convertedList = map show list
    let listWithPipes = intercalate "|" convertedList
    print (map replaceNumberToSymbols listWithPipes)

drawBoardLetters :: IO()
drawBoardLetters = print(intersperse '|' [chr x | x<-[65..90]])

drawPlayerData :: Int -> Int -> IO()
drawPlayerData attempts score = do
    print "___________________________________________________"
    print "                                                   "
    print ((" Attempts: " ++ show attempts) ++ (" | Score: " ++ show score))
    print "___________________________________________________"

replaceNumberToSymbols :: Char -> Char
replaceNumberToSymbols x
    | x == '0' = ' '
    | x == '1' = 'O'
    | x == '2' = 'X'
    | otherwise = '|'
