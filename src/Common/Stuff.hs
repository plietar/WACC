module Common.Stuff where
import Control.Monad

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM p t e = do
  q <- p
  if q
  then t
  else e

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (p:ps) = ifM p (andM ps) (return False)

orM :: Monad m => [m Bool] -> m Bool
orM []     = return False
orM (p:ps) = ifM p (return False) (orM ps)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = andM . map p

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = orM . map p

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = ifM p m (return ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = ifM p (return ()) m

infixl 3 <^
infixl 3 ^>
(<^) :: Functor f => f a -> (a -> b) -> f b
(<^) = flip (<$>)
(^>) :: Applicative f => f (a -> b) -> f a -> f b
(^>) = (<*>)

