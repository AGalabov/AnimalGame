module Main where

import BinaryTree (BinaryTree(Empty,Node),isEmpty)
import Data.Char (toLower) --might write it myselfz       

type AnimalsTree = BinaryTree String

newNode :: String -> String -> String -> String-> AnimalsTree
newNode currAnimal newAnimal newQuestion yesAnswer
  | yesAnswer == currAnimal = (Node newQuestion prev new)
  | otherwise               = (Node newQuestion new prev)
  where new  = (Node newAnimal Empty Empty)
        prev = (Node currAnimal Empty Empty)

addNewAnimal :: AnimalsTree -> String -> String -> String -> String-> AnimalsTree
addNewAnimal Empty _ _ _ _ = Empty
addNewAnimal (Node x _ _) "" newAnimal newQuestion yesAnswer = newNode x newAnimal newQuestion yesAnswer
addNewAnimal (Node x l r) (p:path) newAnimal newQuestion yesAnswer
  | p == 'l'  = (Node x (addNewAnimal l path newAnimal newQuestion yesAnswer) r)
  | otherwise = (Node x l (addNewAnimal r path newAnimal newQuestion yesAnswer))
                                                               

printTree :: AnimalsTree -> FilePath -> IO ()
printTree tree path = do
  let treeString = show tree
  writeFile path treeString
  
lowerCase :: String -> String
lowerCase line = map toLower line

play :: AnimalsTree -> AnimalsTree -> String -> IO ()
play (Node animal Empty Empty) startTree path = do putStrLn $ "My guess is: " ++ animal ++ "\nAm I right? (yes/no)"
                                                   answer <- getLine
                                                   if lowerCase answer == "yes" 
                                                     then do putStrLn "Success!\nThanks for playing!" 
                                                     else do putStrLn "What was your animal?"
                                                             newAnimal <- getLine
                                                             putStrLn $ "How can I differentiate " ++ animal ++ " from " ++ newAnimal ++ " ?"
                                                             newQuestion <- getLine
                                                             putStrLn $ "The answer to which one is 'Yes'? (" ++ animal ++ "/" ++ newAnimal ++ ")"
                                                             yesAnswer <- getLine
                                                             let newTree = addNewAnimal startTree path newAnimal newQuestion yesAnswer
                                                             putStrLn "My knowledge increased!\nThanks for playing!"
                                                             printTree newTree "tree.txt"
play (Node question left right) startTree path = do putStrLn $ question ++ "(yes/no)"
                                                    answer <- getLine
                                                    if lowerCase answer == "yes" then play left startTree (path ++ "l")
                                                                                 else play right startTree (path ++ "r")
           
main = do treeStr <- readFile "tree.txt"
          let tree = read treeStr
          play tree tree ""
          putStrLn "Do you want to play again!"
          playAgain <- getLine
          if lowerCase playAgain == "yes" then main
                                          else putStrLn "Goodbye!"
