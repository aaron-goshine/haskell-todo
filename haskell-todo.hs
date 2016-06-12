import System.IO
import System.Directory
import System.Environment
import Data.List
import Control.Exception


dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove


main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      nuberedTasks = zipWith(\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines nuberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      nuberedTasks = zipWith(\n line -> show n ++ " - " ++ line) [0..] todoTasks

  putStrLn "these are your TODO items:"
  mapM_ putStrLn nuberedTasks
  putStrLn "Which one do you want to delete?"

  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle 
      removeFile tempName)

    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile "todo.txt"
      renameFile tempName "todo.txt")
