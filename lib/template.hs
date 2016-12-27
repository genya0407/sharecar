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

mkActiveRecord name = do
  fetchAll <- mkFetchAll name
  find <- mkFind name
  return . concat $ [fetchAll, find]

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
  let
    targetType = mkName name
    targetIdType = mkName (name ++ "Id")
  m <- newName "m"
  fd <- funD find [clause [varP key] (normalB (infixE (Just (varE 'runDB)) (varE (mkName "$")) (Just (appE (varE 'get) (varE key))))) []]
  td <- sigD find ((monadIO m) (appT (appT arrowT (conT targetIdType)) (appT (varT m) (appT (conT ''Maybe) (conT targetType)))))
  return [fd, td]
