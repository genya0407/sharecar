{-# LANGUAGE TemplateHaskell #-}

module Template where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Model.Type
import Data.Char
import Database.Persist
import Control.Monad.IO.Class
import Control.Monad

monadIO m = forallT [PlainTV m] $ sequenceQ [appT (conT ''Control.Monad.IO.Class.MonadIO) (varT m)]

runDBTemplate = infixE (Just (varE 'runDB)) (varE (mkName "$")) 

mkBoilerplate name = do
  fetchAll <- mkFetchAll name
  find <- mkFind name
  create <- mkCreate name
  return . concat $ [fetchAll, find, create]

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
  m <- newName "m"
  target <- newName "target"
  let
    targetType = conT $ mkName name
    targetIdType = appT (conT ''Key) targetType
  fd <- funD create [clause [varP target] (normalB $ runDBTemplate (Just (appE (varE 'insert) (varE target)))) []]
  td <- sigD create ((monadIO m) (appT (appT arrowT targetType) (appT (varT m) targetIdType)))
  return [fd, td]


