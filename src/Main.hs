{-# Language ScopedTypeVariables #-}
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Lens a b s t =
  Lens { view :: s -> a
       , update :: b -> s -> t
       }
-- where `s` is the Whole, and a is a part.
-- where b is a part, s is the whole and t is the new whole

-- for example a lens onto the left of a tuple
p1 :: Lens a b (a, c) (b, c)
p1 = Lens view update where
  view (x,y) = x
  update x' (x,y) = (x',y)

-- we can also have a lens for the sign of an integer
--            part part' whole    whole'   I should just  s t a b
sign :: Lens Bool Bool  Integer Integer
sign = Lens view update where
  view x = x >= 0
  update b x = if b then abs x else -(abs x)

-- A Co-Lens appears, think coproduct
data Prism a b s t =
  Prism { match :: s -> Either t a -- co-update, if update was (b,s) -> t
        , build :: b -> t -- coview, co-(s -> a)
        }

-- A prism over maybe
the :: Prism a b (Maybe a) (Maybe b)
the = Prism match build where
  match :: Maybe a -> Either (Maybe b) a
  match (Just x) = Right x
  match (Nothing) = Left Nothing
  build :: b -> Maybe b
  build = Just 
