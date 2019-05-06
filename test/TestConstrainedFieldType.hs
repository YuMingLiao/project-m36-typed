{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.TypeLits
import ProjectM36.Typed.TypeFunctions
import ProjectM36.Typed.Schema (FromMaybe)
data A = A
 {
   afield1 :: U Int
-- I intend to use this type information to do some type-level checking
-- But I can't derive Show for (F a f). I guess this is because (F a f) is undecidable
-- , afield2 :: F B "bField"
 } deriving Show

data B = B
 {
   bField :: Char 
 }

type U a = a
type F a (f :: Symbol) = FromMaybe (LookupFieldType (ExtractFields A) f)

type Main = FromMaybe (LookupFieldType (ExtractFields A) "afield1")
type AFields = F A "afield1"
type family LookupFieldType (xs :: [Field Symbol *]) (b::Symbol) :: Maybe * where
  LookupFieldType (('Field a t) ':xs) b = If (TypeEqual a b) ('Just t) (LookupFieldType xs b)   
  LookupFieldType '[] _ = Nothing


