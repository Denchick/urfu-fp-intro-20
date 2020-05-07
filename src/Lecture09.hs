{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.Random
import System.Directory
import System.IO
import System.FilePath
import Data.List
import Data.UUID as UUID
import Data.UUID.V4 as UUIDv4

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show, Read)

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)


instance Ord Todo where
  compare (Todo _ _ _ (Deadline d1) _) (Todo _ _ _ (Deadline d2) _) = compare d1 d2

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show, Read)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder

getTodoPath :: TodoList -> Id -> FilePath
getTodoPath (TodoList todoList) (Id id) = joinPath [todoList, id]

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title content deadline = do
  id <- UUIDv4.nextRandom
  let todoId = Id $ UUID.toString id
  let filepath = getTodoPath todoList todoId
  file <- openFile filepath WriteMode
  hPutStr file $ show $ Todo todoId title content deadline False
  hClose file
  return todoId

readTodo :: TodoList -> Id -> IO Todo
readTodo todoList id = do
  todo <- readFile $ getTodoPath todoList id
  return $ read todo

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  todo <- readTodo todoList id
  putStrLn $ show todo

removeTodo :: TodoList -> Id -> IO ()
removeTodo todoList id = removeFile $ getTodoPath todoList id

updateTodo :: TodoList -> Id -> Todo -> IO ()
updateTodo todoList todoId todo = do
  let filepath = getTodoPath todoList todoId
  removeFile filepath
  file <- openFile filepath WriteMode
  hPutStr file $ show todo
  hClose file

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList todoId (TodoEdit title content deadline) = do

  todo <- readTodo todoList todoId
  updateTodo todoList todoId todo {
      title = title,
      content = content,
      deadline = deadline
    }

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList todoId = do
  todo <- readTodo todoList todoId
  updateTodo todoList todoId todo { isDone = True }

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList rootFolder) = do
  files <- listDirectory rootFolder
  todo <- mapM (readTodo todoList . Id) files
  return $ sort todo

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  allTodo <- readAllTodo todoList
  return $ filter (\(Todo _ _ _ _ isDone) -> not isDone) allTodo

showTodos :: [Todo] -> IO ()
showTodos = mapM_ $ putStrLn . show

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = readAllTodo todoList >>= showTodos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = readUnfinishedTodo todoList >>= showTodos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  number <- randomRIO (1, 100)
  playGuessGameHandleTurn number

playGuessGameHandleTurn :: Int -> IO ()
playGuessGameHandleTurn number = do
  putStr "Your number: "
  playerNumber <- readLn :: IO Int 
  case compare playerNumber number of
    GT -> do
      putStrLn "Too small"
      playGuessGameHandleTurn number
    LT -> do
      putStrLn "Too big"
      playGuessGameHandleTurn number
    EQ -> putStrLn "Yep, that's the number!"

-- </Задачи для самостоятельного решения>