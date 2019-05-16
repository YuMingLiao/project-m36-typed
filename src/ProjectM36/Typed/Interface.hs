{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
module ProjectM36.Typed.Interface where
import RIO
import ProjectM36.Typed.Execute
import ProjectM36.Typed.Ops
import ProjectM36.Typed.Internal
import ProjectM36.Typed.DB.Types
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad.IO.Class
import Prelude (zipWith)
import Data.UUID
import Data.UUID.V4
import Debug.Trace as D

-- Identifiable: Custom Unique Constraint as Identifiers
-- Recordable: RecordId a using UUID as Identifiers, or using auth id.
type Databasable env db m a = (
  HasLogFunc (env db), 
  HasDbConnection env db,
  MonadReader (env db) m,
  MonadIO m,
  AppRecordMeta a,
  IsDbType a,
  HasNamedDbType db (AppRecordName a) (DbRecord a)
  ) 

throwDbError :: (MonadIO m, Applicative m) => Either DbErrorQ a -> m a
throwDbError = either (liftIO . throwIO) pure


insertUUID :: forall env db m a. Databasable env db m a => a -> m (DbRecord a)
insertUUID a = do
  uuid <- liftIO nextRandom 
  e <- executeUpdateM (insertRecordT @db (RecordId (toText uuid)) a)
  r <- throwDbError e
  return r

insertUUID_ID :: forall env db m a. Databasable env db m a => a -> m (RecordId a)
insertUUID_ID a = dbRecordId <$> insertUUID a



insertCustomIdR :: forall env db m a. Databasable env db m a => RecordId a -> a -> m (DbRecord a)
insertCustomIdR i a = do
  e <- executeUpdateM @env @db (insertRecordT @db i a)
  throwDbError e


getRec :: forall env db m a. Databasable env db m a => RecordId a -> m (Maybe (DbRecord a))
getRec i = do
  e <- executeQueryM (getR @db i) 
  r <- throwDbError e
  return r

get :: forall env db m a. Databasable env db m a => RecordId a -> m (Maybe a)
get i = fmap (fmap dbRecordRecord) $ getRec i

{-
getBy f v = do
  r <- throwDbError =<< executeQueryM (getByFieldR f v) 
  return (dbRecordRecord <$> r)
-}


fetchPair :: forall env db m a. Databasable env db m a => m [(RecordId a, a)]
fetchPair = do
  e <- executeQueryM (fetchR @db) 
  rs <- throwDbError e
  return $ zipWith (,) (dbRecordId <$> rs) (dbRecordRecord <$> rs)

fetch :: forall env db m a. Databasable env db m a => m [a]
fetch = do
  e <- executeQueryM (fetchNoR @db) 
  throwDbError e

replace :: forall env db m a. Databasable env db m a => RecordId a -> a -> m ()
replace i a = do
  e <- executeUpdateM @env @db @m (updateR @db @a i a) 
  throwDbError e
  return ()

deleteHard :: forall env db m a. Databasable env db m a => RecordId a -> m ()
deleteHard i = do
  emr <- executeQueryM @env @db @m @(Maybe (DbRecord a)) $ getR @db @a i
  mr <- throwDbError emr
  case mr of
       Nothing -> errNoRecord i
       Just r -> do
           eRes <- executeUpdateM @env @db $ deleteHardR @db @a r
           throwDbError eRes

