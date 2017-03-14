{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Route.Gas where

import           Web.Spock
import           Data.HVect

import qualified View as V
import qualified Model.Car as Car
import qualified Model.Gas as Gas
import qualified Form as F
import           Route.Url

import           Utils

gasRoute :: SpockCtxM (HVect (Entity User ': '[])) () SessionVal MyAppState ()
gasRoute = do
  get ("/car" <//> var <//> "gas/new") $ \_carid -> do
    let carid = toSqlKey _carid
    (me :: Entity User) <- liftM findFirst getContext
    Just cwo <- Car.withOccupied carid
    html $ V.gasNew_ me cwo
  post ("/car" <//> var <//> "gas/new") $ \_carid -> do
    let carid = toSqlKey _carid
    (Entity meid me :: Entity User) <- liftM findFirst getContext
    mGas <- F.formGas meid carid
    case mGas of
      Just gas -> do
        Gas.create gas
        redirect "/car"
      Nothing -> do
        redirect $ gasNewUrl carid
