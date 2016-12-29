{-# LANGUAGE TemplateHaskell #-}
module Model.Gas where

import           Template

import           Utils

mkBoilerplate "Gas"

new = Gas (toSqlKey 0) (toSqlKey 0) 0 defaultUTCTime defaultUTCTime
