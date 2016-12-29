module Route.Url where

import Data.Monoid ((<>))
import Data.Text

import Utils

carDetailUrl :: CarId -> Text
carDetailUrl carid = "/car/" <> (pack . show . fromSqlKey $ carid)

carOccupyNewUrl :: CarId -> Text
carOccupyNewUrl carid = (carDetailUrl carid) <> "/occupy/new"

occupationEditUrl :: OccupationId -> Text
occupationEditUrl occupid = "/occupation/" <> ( pack . show . fromSqlKey $ occupid) <> "/edit"

gasNewUrl :: CarId -> Text
gasNewUrl carid = "/car/" <> (pack . show . fromSqlKey $ carid) <> "/gas/new"
