{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
module ConstraintUnions where
import ProjectM36.Base
import Data.Convertible

class A a where
  fA :: a -> RestrictionPredicateExpr 
class B b where
  fB :: b -> RestrictionPredicateExpr
class C c where
  fC :: c -> RestrictionPredicateExpr
class ABC abc where
  fABC :: abc -> RestrictionPredicateExpr

class (ABC abc, ABC abc') => Logicalable abc abc' where
  (&&&) :: abc -> abc' -> RestrictionPredicateExpr
  a &&& b = AndPredicate (fABC a) (fABC b)
  (|||) :: abc -> abc' -> RestrictionPredicateExpr
  a ||| b = OrPredicate (fABC a) (fABC b)

data Yes
data No
class IsA a flag | a -> flag
class IsB b flag | b -> flag
class IsC c flag | c -> flag
instance Delay No flag => IsA a flag
instance Delay No flag => IsB b flag
instance Delay No flag => IsC c flag

instance (IsA abc isA, IsB abc isB, IsC abc isC, ABC' isA isB isC abc) => ABC abc where
  fABC = fABC' (undefined :: isA) (undefined :: isB) (undefined :: isC)

class ABC' isA isB isC abc where
  fABC' :: isA -> isB -> isC -> abc -> RestrictionPredicateExpr 
instance (A a) => ABC' Yes No No a where fABC' _ _ _ a = fA a
instance (B b) => ABC' No Yes No b where fABC' _ _ _ b = fB b
instance (C c) => ABC' No No Yes c where fABC' _ _ _ c = fC c
instance (A a) => ABC' Yes Yes No a where fABC' _ _ _ a = fA a
instance (A abc) => ABC' isA isB isC abc where fABC' _ _ _ impredicted = error "impredicted type"

class Delay a b | a -> b
instance Delay a a

instance IsA RestrictionPredicateExpr Yes
instance A RestrictionPredicateExpr where fA = id

instance {-# Overlapping #-} IsA RelationalExpr Yes
instance A RelationalExpr where fA = convert

instance {-# Overlapping #-} IsA AtomExpr Yes
instance A AtomExpr where fA = convert
{-
instance {-# Overlapping #-} Convertible a RelationalExpr => IsB a Yes
instance Convertible a RelationalExpr => B a where 
  fB a = case convertVia (undefined :: RelationalExpr) a of
              Left e -> error (prettyConvertError e)
              Right a -> a
-}

instance {-# Overlapping #-} Convertible RelationalExpr RestrictionPredicateExpr where
  safeConvert a = Right $ RelationalExprPredicate a

 
instance {-# Overlapping #-} Convertible AtomExpr RestrictionPredicateExpr where
  safeConvert a = Right $ AtomExprPredicate a


instance (Convertible a AtomExpr, Convertible AtomExpr RestrictionPredicateExpr) => Convertible a RestrictionPredicateExpr where
  safeConvert = convertVia (undefined :: AtomExpr)
{-
 -- if it's a Relation, don't bother with RelationalAtom
instance {-# Overlapping #-} (Convertible a RelationalExpr, Convertible RelationalExpr RestrictionPredicateExpr) => Convertible a RestrictionPredicateExpr where
  safeConvert = convertVia (undefined :: RelationalExpr)
 -}

