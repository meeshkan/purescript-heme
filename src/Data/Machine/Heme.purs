module Data.Machine.Heme where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)

data Heme_ = Heme_

instance hetHeme ::
  Heme i o =>
  Mapping Heme_ i o where
  mapping Heme_ = heme

data Hemez_ a = Hemez_ a

instance hetHemez ::
  Heme (i /\ a) o =>
  Mapping (Hemez_ a) i o where
  mapping (Hemez_ a) i = heme (i /\ a)

class Heme i o | i -> o where
  heme :: i -> o

instance hemeUnit :: Heme Unit Unit where
  heme = identity

instance hemeF4 :: Heme f g => Heme ((a -> b -> c -> d -> e) /\ f) (a -> b -> c -> d -> (e /\ g)) where
  heme (abcde /\ f) a b c d = (abcde a b c d) /\ heme f
else instance hemeF3 :: Heme e f => Heme ((a -> b -> c -> d) /\ e) (a -> b -> c -> (d /\ f)) where
  heme (abcd /\ e) a b c = (abcd a b c) /\ heme e
else instance hemeF2 :: Heme d e => Heme ((a -> b -> c) /\ d) (a -> b -> (c /\ e)) where
  heme (abc /\ d) a b = (abc a b) /\ heme d
else instance hemeF1 :: Heme c d => Heme ((a -> b) /\ c) (a -> (b /\ d)) where
  heme (ab /\ c) a = (ab a) /\ heme c
else instance hemeFRec :: HMap (Hemez_ b) { | i } { | o } => Heme ({ | i } /\ b) { | o } where
  heme (a /\ b) = hmap (Hemez_ b) a
else instance hemeF0 :: Heme b c => Heme (a /\ b) (a /\ c) where
  heme (a /\ b) = a /\ heme b
else instance hemeRec :: HMap Heme_ { | i } { | o } => Heme { | i } { | o } where
  heme = hmap Heme_
