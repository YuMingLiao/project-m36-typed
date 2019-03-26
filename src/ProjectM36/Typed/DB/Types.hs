{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
module ProjectM36.Typed.DB.Types where

import RIO
import qualified RIO.Time as Time

import System.Random

import Data.Binary

--import ProjectM36.Typed.Types
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Arbitrary as SOP

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import ProjectM36.Typed.Gen
import ProjectM36.Atomable
import ProjectM36.Tupleable
import ProjectM36.Base --(Attribute(..))
import ProjectM36.DataTypes.Primitive
import ProjectM36.Attribute (addAttributes)
import ProjectM36.Tuple ({-mkRelationTupleFromMap,-} atomForAttributeName, tupleExtend)
import ProjectM36.Error (RelationalError)
import qualified Data.Map.Ordered as OM
import qualified Data.Vector as V
import Data.Proxy
--import Data.Text.Encoding
import qualified Debug.Trace as D
import Test.QuickCheck.Instances.Time () 
import Data.Maybe (fromJust)
type Date = Time.UTCTime

data RecordSoftDeleted = RecordSoftDeleted Bool
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Binary, Atomable)

instance Arbitrary RecordSoftDeleted where
  arbitrary = RecordSoftDeleted <$> (arbitrary :: Gen Bool)

data RecordCreated = RecordCreated Date
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Binary, Atomable)

instance Arbitrary RecordCreated where
  arbitrary = RecordCreated <$> (arbitrary :: Gen Date)

data RecordLastModified = RecordLastModified (Maybe Date)
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Binary, Atomable)

instance Arbitrary RecordLastModified where
  arbitrary = RecordLastModified <$> (arbitrary :: Gen (Maybe Date))

data DbRecord a = DbRecord {
      dbRecordRecord :: a
    , dbRecordId :: RecordId a
    , dbRecordCreated :: RecordCreated
    , dbRecordLastModified :: RecordLastModified
    , dbRecordDeleted :: RecordSoftDeleted
--  , dbRecordLogs :: IxSet.IxSet DbRecordLogIxs DbRecordLog
--  , dbRecordETag :: ETag
  } deriving (Eq, Show, Generic, Ord)

instance SOP.Generic (DbRecord a)
instance SOP.HasDatatypeInfo (DbRecord a)
instance (Arbitrary a) => Arbitrary (DbRecord a) where arbitrary = SOP.garbitrary

-- dbRecordRecord is flattened into a one-layer relation of DbRecord a
--
instance (Show a, Tupleable a) => Tupleable (DbRecord a) where
  toTuple dbRecord = D.traceShowId $ tupleExtend (toTuple $ (dbRecordRecord dbRecord)) $ D.traceShowId $
    mkRelationTupleFromOMap' $ D.traceShowId $ (\x-> D.traceShow (keys x) x) $(OM.fromList ( [("dbRecordId", toAtom (dbRecordId dbRecord)),
                                         ("dbRecordCreated", toAtom (dbRecordCreated dbRecord)),
                                         ("dbRecordLastModified", toAtom (dbRecordLastModified dbRecord)),
                                         ("dbRecordDeleted", toAtom (dbRecordDeleted dbRecord))
                                        ] ))
    where  mkRelationTupleFromOMap' :: OM.OMap AttributeName Atom -> RelationTuple
           mkRelationTupleFromOMap' attrMap = RelationTuple attrs (V.map (\k-> fromJust $ OM.lookup k attrMap) attrNames)
              where
                attrNames = D.traceShowId $ V.fromList (keys attrMap)
                attrs = V.map (\attrName -> Attribute attrName (atomTypeForAtom (fromJust $ OM.lookup (D.traceShowId attrName) (D.traceShowId attrMap) ))) attrNames
  fromTuple tupIn = do
    idAtom <- atomForAttributeName "dbRecordId" tupIn
    created <- atomForAttributeName "dbRecordCreated" tupIn
    lastModified <- atomForAttributeName "dbRecordLastModified" tupIn
    softDeleted <- atomForAttributeName "dbRecordDeleted" tupIn
    case (fromTuple tupIn :: Either RelationalError a) of
      Left err  -> error $show err
      Right res -> pure $ D.traceShowId $ DbRecord {
          dbRecordRecord = res
        , dbRecordId = fromAtom idAtom
        , dbRecordCreated = fromAtom (created) 
        , dbRecordLastModified = fromAtom (lastModified)
        , dbRecordDeleted = fromAtom (softDeleted)
        } 
  toAttributes _ = D.traceShowId $ addAttributes (toAttributes (Proxy :: Proxy a)) $ V.fromList
                                        [Attribute "dbRecordId" (toAtomType (Proxy :: Proxy (RecordId a))),
                                         Attribute "dbRecordCreated" (toAtomType (Proxy :: Proxy RecordCreated)),
                                         Attribute "dbRecordLastModified" (toAtomType (Proxy :: Proxy RecordLastModified )),
                                         Attribute "dbRecordDeleted" (toAtomType (Proxy :: Proxy RecordSoftDeleted))]
                                          
keys  :: OM.OMap k a -> [k]
keys m
  = [k | (k,_) <- OM.assocs m]

data DbDeletedRecord a = DbDeletedRecord {
    dbDeletedRecordId :: RecordId a
 -- , dbDeletedRecordLogs :: IxSet.IxSet DbRecordLogIxs DbRecordLog
  }

-- record logs
data DbRecordLogEvent =
    DbInsert
  | BBReplace
  | DbPatch
  | DbSoftDelete
  | DbHardDelete

data DbRecordLog = DbRecordLog {
    dbRecordLogTime :: Date
  , dbRecordLogEvent :: DbRecordLogEvent
  }

data RecordId a where
  RecordId :: Text -> RecordId a
  deriving (Generic, Eq, Show, Ord)
  deriving anyclass (Binary, NFData, Atomable )

{-
instance Atomable (RecordId a) where
  toAtom (RecordId (SafeId bs)) = TextAtom $ decodeUtf8 bs
  fromAtom (TextAtom t) = RecordId . SafeId $ encodeUtf8 t
  fromAtom _ = error "improper fromAtom"  
  toAtomType _ = TextAtomType
  toAddTypeExpr _ = NoOperation
-}

instance Arbitrary (RecordId a) where
  arbitrary = do
    g <- gFromGen
--    pure . RecordId . fst. random $ g
    pure . RecordId $ (\_ -> "ABC") g
{-
safeIdFromRecordId :: RecordId a -> SafeId
safeIdFromRecordId (RecordId s) = s

data Record a = Record {
    recordRecord :: a
  , recordId :: RecordId a
  , recordCreated :: RecordCreated
  , recordLastModified :: RecordLastModified
  , recordDeleted :: RecordSoftDeleted
--  , recordETag :: ETag
  }

dbRecordToRecord :: DbRecord a -> Record a
dbRecordToRecord DbRecord{..} = Record {
    recordRecord = dbRecordRecord
  , recordId = dbRecordId
  , recordCreated = dbRecordCreated
  , recordLastModified = dbRecordLastModified
  , recordDeleted = dbRecordDeleted
--  , recordETag = dbRecordETag
  }
-}
data DbError =
    DbErrorNotFound Text
  deriving (Show)


type DbGen = StdGen


-- USEFUL TYPES FOR STORING DATA
-- @todo extend this to be a newtype with length constrained using a smart constructor?
type ShortText = Text


data Gender =
    GenderFemale
  | GenderMale
  | GenderUnspecified
  deriving (Generic, Eq, Show)

type DateOfBirth = Time.Day


