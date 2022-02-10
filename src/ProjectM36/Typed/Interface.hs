{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
module ProjectM36.Typed.Interface where
import RIO
import ProjectM36.Typed.Execute
import qualified ProjectM36.Typed.Ops as Op
import ProjectM36.Typed.Internal
import ProjectM36.Typed.DB.Types
import ProjectM36.Tupleable
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad.IO.Class
import Prelude (zipWith)
import Data.UUID
import Data.UUID.V4
import Debug.Trace as D
import GHC.TypeLits
import ProjectM36.Atomable
import ProjectM36.Base
import Control.Monad.Error
-- Identifiable: Custom Unique Constraint as Identifiers
-- Recordable: RecordId a using UUID as Identifiers, or using auth id.

type Queryable env db m = (
  HasLogFunc (env db), 
  HasDbConnection env db,
  MonadReader (env db) m,
  MonadIO m,
  MonadError DbErrorQ m
  )

type Relationable env db m a = (
  HasLogFunc (env db), 
  HasDbConnection env db,
  MonadReader (env db) m,
  MonadIO m,
  MonadError DbErrorQ m,
  AppRecordMeta a,
  HasNamedDbType db (AppRecordName a) a
  )


type Crudable env db m a = (
  Relationable env db m a,
  Op.Uniqueable a
  )

insert :: forall env db m a. Relationable env db m a => a -> m a
insert a = throwQ $  executeUpdateM (Op.insertT @db @a a)

-- TODO: curd need insert (tag id), but now User userIdent is decided by login.

get :: forall env db m a. Crudable env db m a => UniqueKeyType a -> m (Maybe a)
get i = throwQ $ executeQueryM (Op.get @db @a i)

upsert :: forall env db m a. Crudable env db m a => a -> m ()
upsert a = do
  ma <- throwQ $ executeQueryM (Op.get @db @a (uniqueKey a))
  case ma of
    Nothing -> do insert a
                  return ()
    Just v -> replace (uniqueKey v) a
 
getWhereU :: forall env db m a. Crudable env db m a => AtomExpr -> m (Maybe a)
getWhereU atomExpr = throwQ $ executeQueryM (Op.getWhereU @db @a atomExpr)

getWhere :: forall env db m a. Relationable env db m a => [(AttributeName, AtomExpr)] -> m [a]
getWhere xs = throwQ $ executeQueryM (Op.getWhere @db @a xs)

queryRelExpr :: forall env db m. Queryable env db m => RelationalExpr -> m Relation
queryRelExpr relExpr = throwQ $ executeQueryM (Op.queryRelExpr @db relExpr)

{-
boolRE :: forall. env db m. Queryable env db m => RelationalExpr -> m Bool
boolRE relExpr = do
  re <- queryRelExpr relExpr  
  
  return $ bool True False 
-}
fetch :: forall env db m a. Relationable env db m a => m [a]
fetch = throwQ $ executeQueryM (Op.fetchT @db) 


 
fetchPair :: forall env db m a. Crudable env db m a => m [(UniqueKeyType a, a)]
fetchPair = do
  rs <- throwQ $ executeQueryM (Op.fetchT @db) 
  return $ zipWith (,) (uniqueKey <$> rs) (rs)

replace :: forall env db m a. Crudable env db m a => UniqueKeyType a -> a -> m ()
replace i a = do
  throwQ $ executeUpdateM @env @db @m (Op.update @db i a) 
  return ()

delete :: forall env db m a. Crudable env db m a => UniqueKeyType a -> m ()
delete i = do
  m <-throwQ $ executeQueryM @env @db @m @(Maybe a) $ Op.get @db @a i
  case m of
       Nothing -> error ("Not found: " ++ show i)
       Just a -> throwQ $ executeUpdateM @env @db $ Op.delete @db @a a




