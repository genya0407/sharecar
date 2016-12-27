{-# LANGUAGE TemplateHaskell #-}

module Template where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Model.Type
import Data.Char
import Database.Persist
import Control.Monad.IO.Class

mkActiveRecord name = do
  fetchall <- newName ("all" ++ name)
  m <- newName "m"
  let targetType = mkName name
      monadio = mkName "MonadIO"
  fd <- funD fetchall [clause [] (normalB $ [|runDB $ selectList [] [] |]) []]
  td <- sigD fetchall (forallT [PlainTV m] (sequenceQ [appT (conT ''Control.Monad.IO.Class.MonadIO) (varT m)]) (appT (varT m) (appT listT (appT (conT ''Entity) (conT targetType)))))
  return [fd, td]
