{-# LANGUAGE TemplateHaskell #-}

module Template where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char
import Database.Persist
import Utils

{-
 - create :: User -> m (Key User) -- updatedとcreatedを更新する
 - all :: m [User]
 - find :: Key User -> m (Maybe User)
 - update :: Key User -> User -> m User -- updatedを更新する
-}

monadIO m = forallT [PlainTV m] $ sequenceQ [appT (conT ''MonadIO) (varT m)]
runDBTemplate = infixE (Just (varE 'runDB)) (varE (mkName "$")) 

mkBoilerplate name = concat <$> mapM (\mk -> mk name) [mkFetchAll, mkFind, mkCreate, mkUpdate]

mkFetchAll name = do
  fetchall <- newName "all"
  m <- newName "m"
  let targetType = mkName name
  fd <- funD fetchall [clause [] (normalB $ [|runDB $ selectList [] [] |]) []]
  td <- sigD fetchall ((monadIO m) (appT (varT m) (appT listT (appT (conT ''Entity) (conT targetType)))))
  return [fd, td]

mkFind name = do
  find <- newName "find"
  key <- newName "key"
  m <- newName "m"
  let
    targetType = conT $ mkName name
    targetIdType = appT (conT ''Key) targetType
  fd <- funD find [clause [varP key] (normalB $ runDBTemplate (Just (appE (varE 'get) (varE key)))) []]
  td <- sigD find ((monadIO m) (appT (appT arrowT targetIdType) (appT (varT m) (appT (conT ''Maybe) targetType))))
  return [fd, td]

mkCreate name = do
  create <- newName "create"
  target <- newName "target"
  now <- newName "now"
  m <- newName "m"
  let
    targetType = conT $ mkName name
    targetIdType = appT (conT ''Key) targetType
    upd = mkName $ (map toLower name) ++ "Updated"
    crd = mkName $ (map toLower name) ++ "Created"
  fd <- funD create [clause [varP target] (normalB (infixE (Just (varE 'getCurrentTime')) (varE $ mkName ">>=") (Just (lamE [varP now] (appE (varE 'runDB) (infixE (Just (varE 'insert)) (varE (mkName "$")) (Just (recUpdE (varE target) [return (upd,VarE now), return (crd, VarE now)])))))))) []]
  td <- sigD create ((monadIO m) (appT (appT arrowT targetType) (appT (varT m) targetIdType)))
  return [fd, td]

mkUpdate name = do
  update <- newName "update"
  targetid <- newName "targetid"
  target <- newName "target"
  now <- newName "now"
  m <- newName "m"
  let
    upd = mkName $ (map toLower name) ++ "Updated"
    targetType = conT $ mkName name
    targetTypeId = conT . mkName $ name ++ "Id"
  fd <- funD update [clause [varP targetid, varP target] (normalB (infixE (Just (varE 'getCurrentTime')) (varE $ mkName ">>=") (Just (lamE [varP now] (appE (varE 'runDB) (infixE (Just (appE (varE 'repsert) (varE targetid))) (varE $ mkName "$") (Just (recUpdE (varE target) [return (upd,VarE now)])))))))) []]
  td <- sigD update ((monadIO m) (appT (appT arrowT targetTypeId) (appT (appT arrowT targetType) (appT (varT m) (tupleT 0)))))
  return [fd, td]
