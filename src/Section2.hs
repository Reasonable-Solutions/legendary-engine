{-# Language ScopedTypeVariables #-}
module Section2 where
-- Optics, Concretely

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