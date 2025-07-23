module Main where

import Data.Char (toLower)
import System.Exit -- Success or Failure exit.
import Text.Read (readMaybe) -- Safe parsing

{-
 MERMAID_DIAGRAM_START
 graph TD
     A[Start: main] --> B[Call getInput]
     B --> C[Get birth year]
     C --> D[Display prompt]
     D --> E[Get user input]
     E --> F{Valid integer?}
     F -->|No| G{Input is quit?}
     G -->|Yes| H[Exit program]
     G -->|No| I[Show error]
     I --> C
     F -->|Yes| J[Store birth year]
     J --> K[Get future year]
     K --> L[Display prompt]
     L --> M[Get user input]
     M --> N{Valid integer?}
     N -->|No| O{Input is quit?}
     O -->|Yes| P[Exit program]
     O -->|No| Q[Show error]
     Q --> K
     N -->|Yes| R[Store future year]
     R --> S[Calculate age]
     S --> T[Print result]
     T --> U[End]
    classDef yellowNode fill:#FFD700,color:#000,font-weight:bold;
    class I,Q yellowNode;
    classDef greenNode fill:#006400,color:#ffffff,font-weight:bold;
    class A greenNode;
    classDef redNode fill:#FF0000,color:#FFFFFF,font-weight:bold;
    class H,P,U redNode;
 MERMAID_DIAGRAM_END
-}

stringToLower :: String -> String
stringToLower = map toLower

-- Helper function to get a single integer input with a custom prompt
getIntInput :: String -> IO Int
getIntInput prompt = do
  putStrLn $ prompt ++ " or\ntype QUIT to exit."
  inputStr <- getLine

  case readMaybe inputStr :: Maybe Int of
    Nothing
      | stringToLower inputStr == "quit" -> putStrLn "Exiting..." >> exitSuccess
      | otherwise -> putStrLn "Invalid input.  Please try again." >> getIntInput prompt
    Just val -> return val

-- Function to get both birth year and future year, returning a tuple
getInput :: IO (Int, Int) -- Returns tuple of (birthYear, futureYear)
getInput = do
  birthYear <- getIntInput "Please enter your birth year"
  futureYear <- getIntInput "Please enter the future year"
  return (birthYear, futureYear)

main :: IO () -- Declare the type of main as an IO action returning unit
main = do
  (birthYear, futureYear) <- getInput -- Call the function to get both birth and future year.
  putStrLn $ "In the year " ++ show futureYear ++ ", you will be: " ++ show (futureYear - birthYear) -- Calculate and print the age difference
