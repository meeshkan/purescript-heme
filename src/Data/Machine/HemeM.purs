module Data.Machine.HemeM where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)

data HemeM_ = HemeM_

instance hetHemeM ::
  HemeM i o =>
  Mapping HemeM_ i o where
  mapping HemeM_ = hemeM

data HemeMz_ a = HemeMz_ a

instance hetHemeMz ::
  HemeM (i /\ a) o =>
  Mapping (HemeMz_ a) i o where
  mapping (HemeMz_ a) i = hemeM (i /\ a)

class HemeM i o | i -> o where
  hemeM :: i -> o

instance hemeMUnit :: HemeM Unit Unit where
  hemeM = identity

instance hemeMF4 :: (Functor m, Applicative m, Apply m, HemeM f g) => HemeM ((a -> b -> c -> d -> m e) /\ f) (a -> b -> c -> d -> m (e /\ g)) where
  hemeM (abcde /\ f) a b c d = (/\) <$> (abcde a b c d) <*> pure (hemeM f)
else instance hemeMF3 :: (Functor m, Applicative m, Apply m, HemeM e f) => HemeM ((a -> b -> c -> m d) /\ e) (a -> b -> c -> m (d /\ f)) where
  hemeM (abcd /\ e) a b c = (/\) <$>  (abcd a b c) <*> pure (hemeM e)
else instance hemeMF2 :: (Functor m, Applicative m, Apply m, HemeM d e) => HemeM ((a -> b -> m c) /\ d) (a -> b -> m (c /\ e)) where
  hemeM (abc /\ d) a b = (/\) <$> (abc a b) <*> pure (hemeM d)
else instance hemeMF1 :: (Functor m, Applicative m, Apply m, HemeM c d) => HemeM ((a -> m b) /\ c) (a -> m (b /\ d)) where
  hemeM (ab /\ c) a = (/\) <$> (ab a) <*> pure (hemeM c)
else instance hemeMFRec :: HMap (HemeMz_ b) { | i } { | o } => HemeM ({ | i } /\ b) { | o } where
  hemeM (a /\ b) = hmap (HemeMz_ b) a
else instance hemeMF0 :: (Functor m, Applicative m, Apply m, HemeM b c) => HemeM (m a /\ b) (m (a /\ c)) where
  hemeM (a /\ b) = (/\) <$> a <*> pure (hemeM b)
else instance hemeMRec :: HMap HemeM_ { | i } { | o } => HemeM { | i } { | o } where
  hemeM = hmap HemeM_
