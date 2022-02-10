{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DataKinds, StandaloneDeriving, TypeOperators, TypeFamilies, DeriveGeneric #-}
import Test.TypeSpec
import ProjectM36.Typed
import ProjectM36.Typed.DB.Types
import qualified Generics.SOP as SOP
import qualified Generics.SOP.Arbitrary as SOP
import Test.QuickCheck as QC
import Prelude 
import Data.Text
import GHC.Generics
import Control.DeepSeq
import Data.Binary

main :: IO ()
main = print spec0

spec0 :: Expect (AppSchema `Isn't` Bool)
spec0 = Valid


data User = User
  { userFirstName :: Text
  , userLastName :: Text
  , userEmail :: Text
  , userDateOfBirth :: Maybe DateOfBirth
  , userAddress :: Address
  } deriving (Generic)

data Address = Address
  { addressLineOne :: Text
  , addressLineTwo :: Maybe Text
  , addressTown :: Maybe Text
  , addressCounty :: Maybe Text
  , addressCountry :: Maybe Text
  , addressPostcode :: Maybe Text
  , addressOwnerEmail :: Text 
--  , addressOwner :: RecordId User
  } deriving (Generic)


data PhoneNumber = PhoneNumber
  { phoneNumberNumber :: Text
  , phoneNumberComment :: Maybe Text
---  , phoneNumberOwner :: RecordId User
  } deriving (Generic)

type AppSchema = (
      Define "Users" (DbRecord User) -- :$ ('[UniqueConstraint '["userEmail"]]) 
   :& Define "Addresses" Address -- :$ ('[ForeignConstraint '["addressOwnerEmail"] (Define "Users" User) '["userEmail"]])
   :& Define "PhoneNumbers" PhoneNumber :$ '[UniqueConstraint '["phoneNumberNumber"]] 
  )

deriving instance Eq User
deriving instance Ord User
deriving instance Show User
instance SOP.Generic User
instance SOP.HasDatatypeInfo User
instance Arbitrary User where arbitrary = SOP.garbitrary
instance AppRecordMeta User where
  type AppRecordName User = "User"
instance Tupleable User

deriving instance Eq Address
deriving instance Ord Address
deriving instance Show Address
instance SOP.Generic Address
instance SOP.HasDatatypeInfo Address
instance Arbitrary Address where arbitrary = SOP.garbitrary
instance AppRecordMeta Address where
  type AppRecordName Address = "Address"
instance Tupleable Address
instance NFData Address
instance Binary Address
instance Atomable Address

deriving instance Eq PhoneNumber
deriving instance Ord PhoneNumber
deriving instance Show PhoneNumber
instance SOP.Generic PhoneNumber
instance SOP.HasDatatypeInfo PhoneNumber
instance Arbitrary PhoneNumber where arbitrary = SOP.garbitrary
instance AppRecordMeta PhoneNumber where
  type AppRecordName PhoneNumber = "PhoneNumber"
instance Tupleable PhoneNumber

