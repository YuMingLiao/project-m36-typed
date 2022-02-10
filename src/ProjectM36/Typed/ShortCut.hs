{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
module ProjectM36.Typed.ShortCut where
-- users need OverloadedLabels, OverloadedLists, and default(Int,Text) to use these shortcuts.
import Data.Text hiding (foldl, map)
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Atomable
import Prelude hiding ((!!))
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits hiding (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Exts (IsList(..), IsString)
import Data.Char
import Data.Convertible
import Data.Proxy

default (Text)

instance IsList (AttributeNamesBase ()) where
  type Item (AttributeNamesBase ()) = AttributeName
  fromList = AttributeNames . S.fromList 
  toList (AttributeNames ns) = S.toList ns

instance IsList (TupleExprsBase ()) where
  type Item TupleExprs = TupleExpr
  fromList ts = TupleExprs () ts
  toList (TupleExprs _ ts) = ts

instance IsList TupleExpr where
  type Item TupleExpr = (AttributeName, AtomExpr) 
  fromList attributeValues = TupleExpr (M.fromList attributeValues)
  toList (TupleExpr attributeValues) = M.toList attributeValues


-- #xxx :: Text
instance KnownSymbol x => IsLabel x Text where
  fromLabel = T.pack $ symbolVal @x Proxy

-- #relvarName :: RelationalExpr
instance KnownSymbol x => IsLabel x RelationalExpr where
  fromLabel = RelationVariable (T.pack $ symbolVal @x Proxy) ()

-- #a Int :: AttributeExpr
-- I don't understand the usage of "AttributeAndTypeNameExpr AttributeName TypeConstructor a"
instance KnownSymbol x => IsLabel x (AtomTypeHaskName -> AttributeExpr) where
  fromLabel = (\atyn -> NakedAttributeExpr (Attribute name atyn)) . toAtomType'
    where name = T.pack $ symbolVal @x Proxy

-- (#a 1) :: ExtendTupleExpr
-- no need for :=
instance (Convertible a AtomExpr, KnownSymbol x) => IsLabel x (a -> ExtendTupleExpr) where
  fromLabel = \x -> AttributeExtendTupleExpr name (convert x) 
    where name = T.pack $ symbolVal @x Proxy

-- #name AtomExpr 
-- ex. tuple [ #name 3 ]
-- default(Text) is needed in client code to avoid no Atomable Char
instance (Convertible a AtomExpr, KnownSymbol x) => IsLabel x (a -> (AttributeName, AtomExpr)) where
  fromLabel = \x -> (name, (convert x))
    where name = T.pack $ symbolVal @x Proxy

-- #funcName [AtomExpr]
instance (KnownSymbol x, Convertible a AtomExpr) => IsLabel x ([a] -> AtomExpr) where
  fromLabel = \as -> FunctionAtomExpr name (map convert as) ()
    where name = T.pack $ symbolVal @x Proxy

instance (KnownSymbol x) => IsLabel x AtomExpr where
  fromLabel = AttributeAtomExpr name
    where name = T.pack $ symbolVal @x Proxy

-- right now, there is no way to do:  relation{xxx AtomableTypes} 
data AtomTypeHaskName = 
  Int |
  Integer |
  Double |
  Text |
  Day |
  DateTime |
  ByteString |
  Bool 
  -- | Relation  -- can't offer relation type right now.

toAtomType' Int = IntAtomType
toAtomType' Integer= IntegerAtomType
toAtomType' Double = DoubleAtomType
toAtomType' Text = TextAtomType
toAtomType' Day  = DayAtomType
toAtomType' DateTime = DateTimeAtomType
toAtomType' ByteString = ByteStringAtomType
toAtomType' Bool = BoolAtomType
-- unsupported atomtypes:
--                RelationAtomType Attributes |
--                ConstructedAtomType TypeConstructorName TypeVarMap |
--                TypeVariableType TypeVarName

-- relation [tuple [#a 1, #b "b"], tuple [#a 2, #b "b"]]
relation :: [TupleExpr] -> RelationalExpr
relation ts = MakeRelationFromExprs Nothing (TupleExprs () ts)

relation' :: [AttributeExprBase ()] -> [TupleExpr] -> RelationalExpr
relation' as ts = MakeRelationFromExprs (Just as) (TupleExprs () ts)

-- tuple [#name "Mike",#age 6]
tuple :: [(AttributeName, AtomExpr)] -> TupleExprBase ()
tuple as = TupleExpr (M.fromList as)

rename :: RelationalExpr -> [(AttributeName,AttributeName)] -> RelationalExpr 
rename relExpr renameList = case renameList of 
  [] -> Restrict TruePredicate relExpr
  renames -> 
    foldl (\acc (old,new) -> Rename old new  acc) relExpr renames 

--project !!
infix 9 !!
(!!) :: RelationalExpr -> AttributeNames -> RelationalExpr  
relExpr !! xs = Project xs relExpr

--join ><
(><) :: RelationalExpr -> RelationalExpr -> RelationalExpr
a >< b = Join a b

all_but :: AttributeNames -> AttributeNames
all_but (AttributeNames ns) = InvertedAttributeNames ns
all_but _ = error "give all_but something other than attribute names."

all_from :: RelationalExpr -> AttributeNames
all_from = RelationalExprAttributeNames 


(@>) = to
to :: AttributeName -> AttributeName -> (AttributeName, AttributeName)
to = (,)

as :: AttributeNames -> AttributeName -> (AttributeNames, AttributeName)
as = (,)

group :: RelationalExpr -> (AttributeNames, AttributeName) -> RelationalExpr
group relExpr (aNames, aName) = Group aNames aName relExpr

ungroup :: RelationalExpr -> AttributeName -> RelationalExpr
ungroup relExpr aName = Ungroup aName relExpr

--extend
-- #a #:= true #: ( #b @:= #count [1,2])
(#:) :: RelationalExpr -> ExtendTupleExpr -> RelationalExpr
a #: b = Extend b a
infix 8 #:

instance Convertible AtomExpr AtomExpr where
  safeConvert n = Right $ n

instance Convertible RelVarName AtomExpr where
  safeConvert n = Right $ RelationAtomExpr (RelationVariable n ()) 

instance Convertible RelationalExpr AtomExpr where
  safeConvert relExpr = Right $ RelationAtomExpr relExpr

instance Convertible RelVarName RelationalExpr where
  safeConvert n = Right $ RelationVariable n ()

-- it's @ in relational algebra
-- (@@) "aaa", not so much pleasant way to write
(@@) :: AttributeName -> AtomExpr
(@@) = AttributeAtomExpr 

type AtomFunctionName = Text
f :: Convertible a AtomExpr => AtomFunctionName -> [a] -> AtomExpr
f n as = FunctionAtomExpr n (map convert as) ()


-- DatabaseContextExpr

(#::) :: RelVarName -> [AttributeExpr] -> DatabaseContextExpr
s #:: xs =  Define s xs
infix 5 #::

(#:=) :: RelVarName -> RelationalExpr -> DatabaseContextExpr 
s #:= r = Assign s r
infix 5 #:=

class Boolean a b where
  (&&&) :: a -> b -> RestrictionPredicateExpr
  infixl 6 &&&
  (|||) :: a -> b -> RestrictionPredicateExpr
  infixl 5 |||

-- where  
-- true #: (#a 1) @~ #a ?= 1 &&& not' false ||| (f "gte" [1])
(@~) :: (Convertible a RelationalExpr, Convertible b RestrictionPredicateExpr) => a -> b -> RelationalExpr
(@~) relExpr resPreExpr = Restrict (convert resPreExpr) (convert relExpr)
infix 4 @~

true :: RelationalExpr
true = ExistingRelation relationTrue

false :: RelationalExpr
false = ExistingRelation relationFalse

trueP = TruePredicate
falseP = NotPredicate TruePredicate

(?=) :: Convertible a AtomExpr => AttributeName -> a -> RestrictionPredicateExpr
(?=) name a = AttributeEqualityPredicate name (convert a)
infix 9 ?=

not' :: Convertible a RestrictionPredicateExpr => a -> RestrictionPredicateExpr
not' = NotPredicate . convert

instance (Convertible a RestrictionPredicateExpr, Convertible b RestrictionPredicateExpr) => Boolean a b where
  a &&& b = AndPredicate (convert a) (convert b) 
  a ||| b = OrPredicate (convert a) (convert b)

instance {-# Incoherent #-} Atomable a => Convertible a RestrictionPredicateExpr where
  safeConvert n = Right $ AtomExprPredicate $ toAtomExpr . toAtom $ n 

instance {-# Incoherent #-} Convertible RelationalExpr RestrictionPredicateExpr where
  safeConvert a = Right $ RelationalExprPredicate a
 
instance {-# Incoherent #-} Convertible AtomExpr RestrictionPredicateExpr where
  safeConvert a = Right $ AtomExprPredicate a

instance {-# Incoherent #-} Convertible RestrictionPredicateExpr RestrictionPredicateExpr where
  safeConvert a = Right $ a

instance {-# Incoherent #-} Atomable a => Convertible a AtomExpr where
  safeConvert n = Right $ toAtomExpr . toAtom $ n 

toAtomExpr :: Atom -> AtomExpr
toAtomExpr (ConstructedAtom n _ xs) = ConstructedAtomExpr n (toAtomExpr <$> xs) () 
toAtomExpr a = NakedAtomExpr a



