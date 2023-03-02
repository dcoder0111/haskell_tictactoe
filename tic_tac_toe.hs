import Data.List (sort)
import Data.List (groupBy)

-- Append an element to the end of a list
append :: a -> [a] -> [a]
append a [] = [a]
append a (x:xs) = x : append a xs

-- Check if a choice is not already in a list of values
choiceNotInList :: Eq a => a -> [a] -> Bool
choiceNotInList choice storedValues = not (elem choice storedValues)

-- Check if a game is over
gameIsOver :: [String] -> Bool
gameIsOver values = length values > 8
    -- Check if any player has won
    -- checkWin values
    -- -- Check if all cells are filled
    -- length values >= 9


-- Check if a player has won
checkWinA :: [String] -> Bool
checkWinA storedValues = (choiceNotInList "A1" storedValues == False) && (choiceNotInList "A2" storedValues == False) && (choiceNotInList "A3" storedValues == False)

checkWinB :: [String] -> Bool
checkWinB storedValues = (choiceNotInList "B1" storedValues == False) && (choiceNotInList "B2" storedValues == False) && (choiceNotInList "B3" storedValues == False)

checkWinC :: [String] -> Bool
checkWinC storedValues = (choiceNotInList "C1" storedValues == False) && (choiceNotInList "C2" storedValues == False) && (choiceNotInList "C3" storedValues == False)

checkWin1 :: [String] -> Bool
checkWin1 storedValues = (choiceNotInList "A1" storedValues == False) && (choiceNotInList "B1" storedValues == False) && (choiceNotInList "C1" storedValues == False)

checkWin2 :: [String] -> Bool
checkWin2 storedValues = (choiceNotInList "A2" storedValues == False) && (choiceNotInList "B2" storedValues == False) && (choiceNotInList "C2" storedValues == False)

checkWin3 :: [String] -> Bool
checkWin3 storedValues = (choiceNotInList "A3" storedValues == False) && (choiceNotInList "B3" storedValues == False) && (choiceNotInList "C3" storedValues == False)

checkWinDiag1 :: [String] -> Bool
checkWinDiag1 storedValues = (choiceNotInList "A1" storedValues == False) && (choiceNotInList "B2" storedValues == False) && (choiceNotInList "C3" storedValues == False)

checkWinDiag2 :: [String] -> Bool
checkWinDiag2 storedValues = (choiceNotInList "A3" storedValues == False) && (choiceNotInList "B2" storedValues == False) && (choiceNotInList "C1" storedValues == False)

checkWin :: [String] -> Bool
checkWin newValues = checkWinA newValues || checkWinB newValues || checkWinC newValues || checkWin1 newValues || checkWin2 newValues || checkWin3 newValues || checkWinDiag1 newValues || checkWinDiag2 newValues

-- Start the game
startGame :: [String] -> [String] -> [String] -> Bool -> IO ()
startGame storedValues player1Choices player2Choices playerTurn = do
    let possibleChoices = ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"] -- possible choices

    print("Here is the board layout: ")
    print("A1 | A2 | A3")
    print("B1 | B2 | B3")
    print("C1 | C2 | C3")
    
    if playerTurn
    then do 
        putStrLn "Player 1's Turn! What is your choice? (A1, A2, A3, B1, B2, B3, C1, C2, C3)"
        userGuess <- getLine
    
        if choiceNotInList userGuess storedValues && elem userGuess possibleChoices
        then do -- if user choice is not occupied, add the thing in there, and game over is not completed yet
            let newValues = userGuess : player1Choices
            let newStoredValues = userGuess : storedValues
            print("Player 1 spots: " ++ show newValues)
            print("Taken spots: " ++ show newStoredValues)
            if gameIsOver newValues
            then putStrLn "Game Over!"
            else do 
                if checkWin newValues
                then print("Player 1 won!")
                else do
                    startGame newStoredValues newValues player2Choices False
        else do
            putStrLn "This spot is already taken! Choose again: "
            print("These spots are occupied or not a possible move: ")
            print(storedValues)
            startGame storedValues player1Choices player2Choices True
    else do
        putStrLn "Player 2's Turn! What is your choice? (A1, A2, A3, B1, B2, B3, C1, C2, C3)"
        userGuess <- getLine

        if choiceNotInList userGuess storedValues && elem userGuess possibleChoices
        then do -- if user choice is not occupied, add the thing in there, and game over is not completed yet
            let newValues = userGuess : player2Choices
            let newStoredValues = userGuess : storedValues
            print("Player 2 spots: " ++ show newValues)
            print("Taken spots: " ++ show newStoredValues)
            if gameIsOver newValues
            then putStrLn "Game Over!"
            else do 
                if checkWin newValues
                then print("Player 2 won!")
                else do
                    startGame newStoredValues player1Choices newValues True
        else do
            putStrLn "This spot is already taken! Choose again: "
            print("These spots are occupied or not a possible move: ")
            print(storedValues)
            startGame storedValues player1Choices player2Choices False


startOfflineGame :: [String] -> [String] -> [String] -> Bool -> IO ()
startOfflineGame storedValues playerChoices machineChoices playerTurn = do
    let possibleChoices = ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"] -- possible choices
    
    if playerTurn
    then do 
        putStrLn "Player's Turn! What is your choice? (A1, A2, A3, B1, B2, B3, C1, C2, C3)"
        userGuess <- getLine
    
        if choiceNotInList userGuess storedValues && elem userGuess possibleChoices
        then do -- if user choice is not occupied, add the thing in there, and game over is not completed yet
            let newValues = userGuess : playerChoices
            let newStoredValues = userGuess : storedValues
            print("Player spots: " ++ show newValues)
            print("Taken spots: " ++ show newStoredValues)
            if gameIsOver newValues
            then putStrLn "Game Over!"
            else do 
                if checkWinA newValues || checkWinB newValues || checkWinC newValues || checkWin1 newValues || checkWin2 newValues || checkWin3 newValues || checkWinDiag1 newValues || checkWinDiag2 newValues
                then print("Player 1 won!")
                else do
                    startOfflineGame newStoredValues newValues machineChoices False
        else do
            putStrLn "This spot is already taken! Choose again: "
            print("These spots are occupied or not a possible move: ")
            print(storedValues)
            startOfflineGame storedValues playerChoices machineChoices False
    else do
        if choiceNotInList "A1" storedValues
        then do 
            let newValues = "A1" : machineChoices
            let newStoredValues = "A1" : storedValues
            if checkWin newValues
                then print("Machine won!")
                else do 
                    print("Machine chose A1")
                    startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "A2" storedValues
            then do
                let newValues = "A2" : machineChoices
                let newStoredValues = "A2" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose A2")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "A3" storedValues
            then do
                let newValues = "A3" : machineChoices
                let newStoredValues = "A3" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose A3")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "B1" storedValues
            then do
                let newValues = "B1" : machineChoices
                let newStoredValues = "B1" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose B1")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "B2" storedValues
            then do
                let newValues = "B2" : machineChoices
                let newStoredValues = "B2" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose B2")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "B3" storedValues
            then do
                let newValues = "B3" : machineChoices
                let newStoredValues = "B3" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose B3")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "C1" storedValues
            then do
                let newValues = "C1" : machineChoices
                let newStoredValues = "C1" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose C1")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "C2" storedValues
            then do
                let newValues = "C2" : machineChoices
                let newStoredValues = "C2" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose C2")
                        startOfflineGame newStoredValues playerChoices newValues True
        else if choiceNotInList "C3" storedValues
            then do
                let newValues = "C3" : machineChoices
                let newStoredValues = "C3" : storedValues
                if checkWin newValues
                    then print("Machine won!")
                    else do
                        print("Machine chose C3")
                        startOfflineGame newStoredValues playerChoices newValues True
        else return ()

-- Main function
main :: IO ()
-- main = startGame [] [] [] True
-- print("Here is the board layout: ")
-- print("A1 | A2 | A3")
-- print("B1 | B2 | B3")
-- print("C1 | C2 | C3")
main = startOfflineGame [] [] [] True
