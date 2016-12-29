module Route.Url where

import Data.Monoid ((<>))
import Database.Persist.Sql (fromSqlKey)

import Data.Text
import Model.Type

carDetailUrl :: CarId -> Text
carDetailUrl carid = "/car/" <> (pack . show . fromSqlKey $ carid)

carOccupyNewUrl :: CarId -> Text
carOccupyNewUrl carid = (carDetailUrl carid) <> "/occupy/new"

occupationEditUrl :: OccupationId -> Text
occupationEditUrl occupid = "/occupation/" <> ( pack . show . fromSqlKey $ occupid) <> "/edit"

