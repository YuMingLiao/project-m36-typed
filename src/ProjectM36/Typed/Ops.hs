{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module ProjectM36.Typed.Ops where

import RIO hiding (toList)
import qualified RIO.List as L
import Data.List
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Tupleable (Tupleable(..), toInsertExpr, toDeleteExpr, toUpdateExpr)
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
    dceWeight (AddTypeConstructor a b) = D.traceShow (AddTypeConstructor a b) 5
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




rvname :: forall a. AppRecordMeta a => Text
rvname = showSymbol $ Proxy @(AppRecordName a)

--crud for a plain a (not DbRecord a)
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

{- TODO: getBy should type-check if the field is UniqueConstraint
getByUniqueConstraintExpr rv rid = Restrict restrictionPredicate (RelationVariable rv ())
  where restrictionPredicate = AttributeEqualityPredicate "dbRecordId" (NakedAtomExpr (toAtom rid))
-}

{-
--it may not get what you want when the field is not unique constraint
getByFieldR :: forall db a.(AppRecordMeta a, HasNamedDbType db (AppRecordName a) a) => Text -> RecordId a -> QueryM db (Maybe (DbRecord a))
getByFieldR field value = do
  rel <- throwQ $ executeQuery $ getByExpr (rvname @a) field value
  as  <- liftEitherQ $ toList rel 
  return (listToMaybe as)
-}

{-
getT :: forall db name a . (AppRecordMeta a, HasNamedDbType db name a) => RecordId a -> QueryM db (Maybe a)
getT rid = do
  traceM . T.pack $ "querying " ++ show rid
  rec <- getR rid
  return (dbRecordRecord rec)
-}


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

toList :: (Tupleable a, Show a) => Relation -> Either RelationalError [a]
toList rel = mapM fromTuple (relationTuples rel)

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

getR :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => RecordId a -> QueryM db (Maybe (DbRecord a))
getR rid = do
    rel <- throwQ . executeQuery $ getRecordExpr (rvname @a) rid
    as  <- liftEitherQ $ toList rel 
    return (listToMaybe as)

errNoRecord rid = error ("No record is found by " ++ show rid)


deleteHardR :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => DbRecord a -> UpdateM db ()
deleteHardR r = do
  e <- liftEitherQ $ toDeleteExpr (rvname @a) ["dbRecordId"] r 
  throwQ $ executeUpdate e
  return ()

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

updateR :: forall db a. (AppRecordMeta a, HasNamedDbType db (AppRecordName a) (DbRecord a)) => RecordId a -> a -> UpdateM db ()
updateR i a = do
    rel <- throwQ . executeQuery $ getRecordExpr (rvname @a) i  
    as  <- liftEitherQ $ toList rel 
    case (listToMaybe as) of
         Nothing -> errNoRecord i 
         Just r -> do
             now <- getCurrentTimeM
             let modified = r {
                   dbRecordRecord = a,
                   dbRecordLastModified = RecordLastModified (Just now)
                 }
             e <- liftEitherQ $ toUpdateExpr (rvname @a) ["dbRecordId"] modified
             throwQ $ executeUpdate e
             return ()





