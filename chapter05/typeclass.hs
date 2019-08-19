import Data.List (intercalate)

data Dog = Dog deriving Show
data Cat = Cat deriving Show
data Human = Human String deriving Show

class Greeting a where
  name :: a -> String
  hello :: a -> String
  hello _ = "..."
  bye :: a -> String
  bye _ = "..."

instance Greeting Human where
  name (Human n) = n
  hello h = "Hi, I'm " ++ name h ++ "."
  bye _ = "See you."

instance Greeting Dog where
  name _ = "a dog"
  hello _ = "Bark!"

instance Greeting Cat where
  name _ = "a cat"
  bye _ = "Meow.."

main1 = do
  print $ hello (Human "hoge")
  print $ hello Dog
  print $ hello Cat

  print $ bye (Human "hoge")
  print $ bye Dog
  print $ bye Cat

-- ----------------------------------------------
sayHello :: Greeting a => a -> IO ()
sayHello x = putStrLn (hello x)

class Greeting a => Laughing a where
  laugh :: a -> String

instance Laughing Human where
  laugh _ = "Zehahahahah...!!"

leaveWithLaugh :: Laughing a => a -> IO ()
leaveWithLaugh x = do
  putStrLn (bye x)
  putStrLn (laugh x)

main2 = do
  leaveWithLaugh (Human "hoge")

-- ----------------------------------------------
liftGreet :: (a -> String) -> ([a] -> String)
liftGreet f = intercalate "\n" . map f

instance Greeting a => Greeting [a] where
  name = liftGreet name
  hello = liftGreet hello
  bye = liftGreet bye

main3 = do
  sayHello [Human "abc", Human "xyz"]

-- ----------------------------------------------
class Breeding a where
  breed :: String -> a

instance Breeding Human where
  breed = Human

main4 = do
  let baby = breed "hoge"
  print $ hello (baby :: Human)
  -- print $ hello baby  -- compile error

main5 = do
  print $ hello $ clone (Human "hoge")
  where
    clone x = breed (name x) `asTypeOf` x