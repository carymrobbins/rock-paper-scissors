import Control.Monad (liftM)
import Data.Char (toUpper)
import Data.List (intersperse)
import System.Random (randomIO)
import Text.Read (readMaybe)

data Outcome = Win | Lose | Draw deriving (Show, Eq)
data Choice = Rock | Paper | Scissors | Lizard | Spock
    deriving (Show, Read, Eq, Enum)

main :: IO ()
main = do
    putStrLn $ showChoices ++ "?"
    human <- humanChoice
    computer <- computerChoice
    putListLn ["Computer picks ", show computer]
    putListLn ["You ", show $ human `against` computer]
    putStrLn "Would you like to play again? (y/n)"
    response <- getLine
    if map toUpper response == "Y" then main else return ()

showChoices :: String
showChoices = concat . intersperse ", " . map show $ (autoEnum :: [Choice])

against :: Choice -> Choice -> Outcome
Rock `against` Paper      = Lose
Rock `against` Spock      = Lose
Paper `against` Scissors  = Lose
Paper `against` Lizard    = Lose
Scissors `against` Rock   = Lose
Scissors `against` Spock  = Lose
Lizard `against` Rock     = Lose
Lizard `against` Scissors = Lose
Spock `against` Paper     = Lose
Spock `against` Lizard    = Lose
x `against` y | x == y    = Draw
              | otherwise = Win

computerChoice :: IO Choice
computerChoice = randomPick autoEnum

humanChoice :: IO Choice
humanChoice = (parseChoice . readMaybe . capitalize) =<< getLine
  where
    parseChoice :: Maybe Choice -> IO Choice
    parseChoice Nothing = putStrLn "Invalid, try again." >> humanChoice
    parseChoice (Just choice) = return choice

-- Helpers.

putListLn :: [String] => IO ()
putListLn = putStrLn . concat

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x):xs

autoEnum :: Enum a => [a]
autoEnum = [(toEnum 0) ..]

randomPick :: [a] -> IO a
randomPick xs = liftM ((xs !!) . (`mod` length xs)) randomIO
