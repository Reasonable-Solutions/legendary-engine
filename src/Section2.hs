{-# Language InstanceSigs #-}
{-# Language ScopedTypeVariables #-}
{-# Language NoImplicitPrelude #-} -- because 2.2 introduces functor and applicative explictly

module Section2 where
-- Optics, Concretely
import Prelude (Bool(..), Integer (..), ($), (+), succ, even)

-- the Adapter type, it is basically Iso
data Adapter a b s t = Adapter { from :: s -> a
                               , to :: b -> t
                               }
{-
            +---------------------+
            |                     |
   S------->| ----from----------->|---------> A
            |                     |
            |                     |
            |                     |
  T<--------|<---to------------   |<-------- B
            |                     |
            +---------------------+
-}


-- lets revisit our P1 over ((a,b),c) lens with adapters



flatten :: Adapter (a, b, c) (a', b', c') ((a,b),c) ((a',b'),c')
flatten = Adapter from to where
  to :: (a',b',c') -> ((a',b'),c')
  to (x', y', z') = ((x', y'), z')
  from :: ((a,b),c) -> (a,b,c)
  from ((x, y),z) = (x, y, z)

-- 2.2 traversal

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
  pure :: a -> f a

newtype State s a = State {runState :: (s -> (a, s))
}
inc :: Bool -> State Integer Bool
inc b = State $ \n -> (b, n + 1)

instance Functor (State s) where
  fmap f m = State $ \s -> let (x, s') = runState m s in (f x, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  m <*> n = State $ \s ->
    let (f, s') = runState m s
        (x, s'') = runState n s'
    in (f x, s'')

-- Now consider a tree
data Tree a = Empty | Node (Tree a) a (Tree a)

-- and it's inorder traversal
inOrder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inOrder _ Empty = pure Empty
inOrder f (Node t1 x t2) = (pure Node <*> inOrder f t1) <*> f x <*> inOrder f t2

-- we can count the odd bits
--                              s         a
countOdd :: Integer -> State Integer Bool
countOdd n = if even n then pure False else inc True

-- >>> :t inc
-- inc :: Bool -> State Integer Bool

-- and we could have this, for example. jfc so much prelimaries
county :: Tree Integer -> State Integer (Tree Bool)
county = inOrder countOdd


-- 2.3 Traversal as concrete optics
-- lets define the funlist!

data FunList a b t = Done t | More a (FunList a b (b -> t))