{-# Language ScopedTypeVariables #-}
module Section1 where

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
  match Nothing = Left Nothing
  build :: b -> Maybe b
  build = Just 

-- we could also have a prism over whole numbers
whole :: Prism Integer Integer Double Double
whole = Prism match build where
  match :: Double -> Either Double Integer
  match d
    | f == 0 = Right n
    | otherwise = Left d
    where (n,f) = properFraction d
  build :: Integer -> Double
  build = fromIntegral

{- we could go for using lenses over nested data structures such as
   a lens onto `a` in the tuple ((a, b), c). BUT IT IS MEGA CLUMSY
   Notice how update is basically from first principles and not lensy at all
-}

p11 :: Lens a b ((a,c), d) ((b, c), d)
p11 = Lens view update where
  Lens v u = p1 -- take apart p1
  view :: ((a,c), d) -> a
  view = v . v -- the left of the left!
  update :: b -> ((a,c),d) -> ((b,c),d)
  update x' xyz = u xy' xyz where
    xy = v xyz -- get the left!
    xy' = u x' xy --update the left with x'

{- This representation (Lens view update/ Prism match build) breaks
   even more for arbitrary cannoncially represented data types
-}