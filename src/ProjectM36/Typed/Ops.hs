{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module ProjectM36.Typed.Ops where

import RIO hiding (toList, and, (&&&))
import qualified RIO.List as L hiding (and, (&&&))
import Data.List hiding (and)
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Tupleable (Tupleable(..), toInsertExpr, toDeleteExpr, toUpdateExpr)
import ProjectM36.Typed.TypeFunctions (IsJust, FromJust)
import Data.Proxy(Proxy(..))

import Control.Monad.Except

import ProjectM36.Typed.Internal
import ProjectM36.Typed.Execute
import ProjectM36.Typed.Generics
import ProjectM36.Typed.DB.Types 
import qualified Debug.Trace as D
import Data.Text as T (pack)
import Data.UUID.V4
import Data.UUID
import Debug.Trace as D
import ProjectM36.Typed.ShortCut
import GHC.TypeLits

type SchemaOpM env m = (MonadError DbErrorQ m, MonadIO m, MonadReader env m)

createSchema :: forall env db. (HasLogFunc env) => SessionId -> Connection -> QDbSchema db -> RIO env (Either DbErrorQ (DbConnection db))
createSchema sid conn sc@(QDbSchema schemaP) = do
  mainRes <- withTransactionUsingQ (sid, conn) UnionMergeStrategy $ runExceptT $ do
    logInfo "Creating schema"
    logInfo $ displayShow schemaP
    let operations = [
            ("Automated generation of database context from schema", sortByWeight $ nub $ toDatabaseSchemaContext schemaP)
          ]
    warnDuplicates operations
    mapM_ runOperation operations
    return $ DbConnection sid conn sc
  when (isRight mainRes) $ logInfo "Schema created successfully"
  return mainRes

  where
    warnDuplicates :: (SchemaOpM env m) => [(Text, [DatabaseContextExpr])] -> m ()
    warnDuplicates ops = 
      case findDuplicates $ concatMap snd ops  of
        [] -> pure ()
        ds -> do
          logWarn "Duplicates detected in schema creation operations: "
          logWarn $ displayShow ds
    findDuplicates :: (Eq a) => [a] -> [a]
    findDuplicates [] = []
    findDuplicates (x:xs) =
      if x `elem` xs
        then x : findDuplicates xs
        else findDuplicates xs
    sortByWeight :: [DatabaseContextExpr] -> [DatabaseContextExpr]
    sortByWeight xs = map fst $ L.sortBy (comparing snd) $ map (\e -> (e, dceWeight e)) xs
    dceWeight :: DatabaseContextExpr -> Int
    dceWeight (AddTypeConstructor a b) = 5
    dceWeight (Define _ _) = 10
    dceWeight (AddInclusionDependency _ _ ) = 15
    dceWeight _ = 20
    runOperation :: (SchemaOpM env m) => (Text, [DatabaseContextExpr]) -> m ()
    runOperation (n, !exprs) = do
      logInfo $ "Running schema operation: " <> displayShow n
      mapM_ runContextExpr exprs

    runContextExpr :: (SchemaOpM env m) => DatabaseContextExpr -> m ()
    runContextExpr e = do
      logInfo "Executing database context expr"
      logInfo $ displayShow e
      handleUnexpectedError $ liftIO $ executeDatabaseContextExpr sid conn e

    handleUnexpectedError :: (SchemaOpM env m) => m (Either RelationalError ()) -> m ()
    handleUnexpectedError act = do
      err <- act
      case err of
        Left e ->
          if isExpectedError e
            then do
              logInfo $ "Expected error: " <> displayShow e
              pure ()
            else throwError . toDbErrorQ $ e
        Right _ -> pure ()
  {-    handleUnexpectedErrors :: (SchemaOpM env m) => m [Either RelationalError ()] -> m ()
      handleUnexpectedErrors act = do
        errs <- act
        let (expected, unexpected) = L.partition isExpectedError (lefts errs)
        when (not . null $ expected) (void $ mapM (logInfo . displayShow) expected)

        case NE.nonEmpty $ unexpected of
         Nothing -> return ()
         Just xs -> throwError . toDbErrorQ $ NE.map toDbErrorQ xs-}

    isExpectedError :: RelationalError -> Bool
    isExpectedError (RelVarAlreadyDefinedError _) = True
    isExpectedError (InclusionDependencyNameInUseError _) = True
    isExpectedError _ = False

toList :: (Tupleable a, Show a) => Relation -> Either RelationalError [a]
toList rel = mapM fromTuple (relationTuples rel)

rvname :: forall a. AppRecordMeta a => Text
rvname = showSymbol $ Proxy @(AppRecordName a)

ukname :: forall a. (HasUniqueKey a, KnownSymbol (UniqueKey a))  => Text
ukname = showSymbol $ Proxy @(UniqueKey a)

insertT :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) a) => a -> UpdateM db a
insertT a = do
  insertBulkT [a]
  return a

insertBulkT :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) a) => [a] -> UpdateM db [a]
insertBulkT as = do
  e <- liftEitherQ $ toInsertExpr as (rvname @a)
  throwQ $ executeUpdate e
  return as

fetchT :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) a) => QueryM db [a]
fetchT = do
  rel <- throwQ $ executeQuery (RelationVariable (rvname @a) ())
  liftEitherQ $ toList rel 

type Uniqueable a = (HasUniqueKey a, KnownSymbol (UniqueKey a), Atomable (UniqueKeyType a))

get :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) a, Uniqueable a) => UniqueKeyType a -> QueryM db (Maybe a)
get rId = do 
  rel <- throwQ $ executeQuery $ getByUniqueConstraintExpr 
  as  <- liftEitherQ $ toList rel 
  return (listToMaybe as)
    where getByUniqueConstraintExpr = (rvname @a) @~ (ukname @a) ?= rId


getWhereU :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) a, Uniqueable a) => AtomExpr -> QueryM db (Maybe a)
getWhereU atomExpr = do 
  rel <- throwQ $ executeQuery $ (rvname @a) @~ (ukname @a) ?= atomExpr
  as  <- liftEitherQ $ toList rel 
  return (listToMaybe as)

getWhere :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) a) => [(AttributeName, AtomExpr)] -> QueryM db [a]
getWhere xs = do 
  rel <- throwQ $ executeQuery $ (rvname @a) @~ (foldl1 (&&&) (fmap (uncurry (?=)) xs)) -- attrName ?= atomExpr
  as  <- liftEitherQ $ toList rel 
  return as



queryRelExpr :: forall db. RelationalExpr -> QueryM db Relation
queryRelExpr relExpr = do
  rel <- throwQ $ executeQuery $ relExpr 
  return rel

{-
-- crud for DbRecord
mkNewRec a ident now = DbRecord {
    dbRecordRecord = a,
    dbRecordId = ident,
    dbRecordCreated = RecordCreated now,
    dbRecordLastModified = RecordLastModified Nothing,
    dbRecordDeleted = RecordSoftDeleted False
  }

recordT :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => RecordId a -> a -> UpdateM db (DbRecord a) 
recordT i a = do
  now  <- getCurrentTimeM
  return (mkNewRec a i now)

--TODO: what if UUID repeat? should deal with insert error and get another uuid?
recordWithUUIDT :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => a -> UpdateM db (DbRecord a) 
recordWithUUIDT a = do
  uuid <- getUUIDM
  now  <- getCurrentTimeM
  return (mkNewRec a (RecordId $ toText uuid) now)

fetchFromDbRecordT :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => QueryM db [a]
fetchFromDbRecordT = do
  rs <- fetchT 
  return (map dbRecordRecord rs)

insertRecordT :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => RecordId a -> a -> UpdateM db (DbRecord a)
insertRecordT i a = do
  r <- recordT i a
  e <- liftEitherQ $ toInsertExpr [r] (rvname @a)
  throwQ $ executeUpdate e
  return r

insertRecordBulkT as = mapM insertRecordT as

getRecordExpr rv rid = Restrict restrictionPredicate (RelationVariable rv ())
  where restrictionPredicate = AttributeEqualityPredicate "dbRecordId" (NakedAtomExpr (toAtom rid))

getByExpr rv f v = Restrict restrictionPredicate (RelationVariable rv ())
  where restrictionPredicate = AttributeEqualityPredicate f (NakedAtomExpr (toAtom v))

fetchR :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => QueryM db [DbRecord a]
fetchR = do
  rel <- throwQ $ executeQuery (RelationVariable (rvname @a) ())
  liftEitherQ $ toList rel 

fetchNoR :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => QueryM db [a]
fetchNoR = do
  recs <- fetchR @db 
  return (dbRecordRecord <$> recs)

fetchSub :: forall db a b. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) a, Tupleable b, Show b) => QueryM db [b]
fetchSub = do
  rel <- throwQ $ executeQuery (RelationVariable (rvname @a) ())
  liftEitherQ $ (toList rel  :: Either RelationalError [b])



getR :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => RecordId a -> QueryM db (Maybe (DbRecord a))
getR rid = do
    rel <- throwQ . executeQuery $ getRecordExpr (rvname @a) rid
    as  <- liftEitherQ $ toList rel 
    return (listToMaybe as)

errNoRecord rid = error ("No record is found by " ++ show rid)


-}
{-
deleteSoftR :: forall db name a. (AppRecordMeta a, HasNamedDbType db name a)=> RecordId a -> UpdateM db ()
deleteSoftR rid = do
    mr <- getR rid
    case mr of
         Nothing -> errNoRecord rid 
         Just r -> do 
           case dbRecordDeleted r of
             RecordSoftDeleted True  -> error (show rid ++" had been soft-deleted.") 
             RecordSoftDeleted False -> do 
                 e <- liftEitherQ $ toUpdateExpr (rvname @a) ["dbRecordDeleted"] (softDelete r)
                 throwQ $ executeUpdate e
                 return ()
     where softDelete x = x { dbRecordDeleted = RecordSoftDeleted True }
-}


errIDNotExist i = error (show i ++ " is not found.")

update :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) a, Uniqueable a) => UniqueKeyType a -> a -> UpdateM db ()
update i new = do
    let getByUniqueConstraintExpr = (rvname @a) @~ (ukname @a) ?= i
    rel <- throwQ . executeQuery $ getByUniqueConstraintExpr 
    as  <- liftEitherQ $ toList @a rel 
    case (listToMaybe as) of
         Nothing -> errIDNotExist i 
         Just _ -> do
             e <- liftEitherQ $ toUpdateExpr (rvname @a) [ukname @a] new
             throwQ $ executeUpdate e
             return ()

delete :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) a, Uniqueable a) => a -> UpdateM db ()
delete ent = do
  e <- liftEitherQ $ toDeleteExpr (rvname @a) [ukname @a] ent 
  throwQ $ executeUpdate e
  return ()





