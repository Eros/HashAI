module util.like where

class like l a | a -> l where
    abst:: (Eq 1) => a -> 1

like :: (Eq 1, like l a) => a -> a -> Bool
like left right = (abst left) == (abst right)