{-# Language InstanceSigs #-}
module Section3 where
-- Let's have profunctors!

class Profunctor p where
  -- | notice the co and contra variance with the '
  dimap :: (a' -> a ) -> (b -> b') -> p a b -> p a' b'

{- with laws: dimap id id = id
              dimap (f' . f) (g . g') = dimap f g . dimap f' g'
  it's just the functor laws with co- ancd contra-variance
-}

instance Profunctor (->) where
  dimap :: (a' -> a) -> (b -> b') -> (a -> b) -> (a' -> b')
  dimap f g h = g . h . f

-- I'll need newtypes to write the Maybe a -> Maybe be instance
-- It is probably just choice.