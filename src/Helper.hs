module Helper (
    ifM
) where

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM mc mt mf = mc >>= \b -> if b then mt else mf