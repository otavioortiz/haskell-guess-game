module Game ( 
   initializeGame
) where 

import Graphics 
import Data.Char
import Distribution.Simple.Setup (trueArg)

initializeGame :: [Int] -> [Int] -> [Int] -> Int -> Int -> IO()
initializeGame  = runRound

runRound :: [Int] -> [Int] -> [Int] -> Int -> Int -> IO()
runRound dataList selectedList digitedChars attempts score = do

   drawBoard selectedList attempts score
   inputLine <- getLine
   let charIndex = ord (head inputLine) -97
   let (a, b, c, d) = verifySelection dataList selectedList digitedChars charIndex attempts score

   if attempts > 0 
      then runRound dataList a d b c 
      else putStrLn ("GAME OVER - Your score: " ++ show score)

verifySelection :: [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> ([Int], Int, Int, [Int])
verifySelection dataList selectedList digitedChars charIndex attempts score = do

   let (a, b) = splitAt charIndex dataList
   let (c, d) = splitAt 1 b
   let (e, f) = splitAt charIndex selectedList
   let (g, h) = splitAt 1 f

   let newScore = score + (if c == [1] then (if letterExistsInList charIndex digitedChars then 0 else 1) else 0)
   let newAttempts = attempts - (if c == [2] then 1 else 0)
   let newDigitedChars = digitedChars ++ [charIndex]

   let newSelecteds = e ++ c ++ h

   (newSelecteds, newAttempts, newScore, newDigitedChars)

letterExistsInList :: Int -> [Int] -> Bool
letterExistsInList n (x:xs)
   |null xs = False
   |n == x = True
   |otherwise = letterExistsInList n xs
   

