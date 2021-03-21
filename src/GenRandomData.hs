{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module GenRandomData where

import Control.Lens (Lens', lens, view)
import Control.Lens.TH (makeLenses)
import Test.QuickCheck.Utf8 ()
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.Text
import Data.Text (Text)
import Data.Text as T
import Test.QuickCheck
import Data.Text.Encoding as E


data TableId = TableId
  { _database :: Text,
    _schemaName :: Text,
    _tableName :: Text
  }
  deriving (Show, Eq, Ord)

makeLenses ''TableId

instance Arbitrary TableId where
  arbitrary = do
    _database <- genText
    _schemaName <- genText
    _tableName <- genText
    return TableId {..}
    where genText = T.pack <$> listOf (choose ('a', 'z'))

name :: Lens' TableId Text
name = lens getter setter
  where
    getter t = T.concat [view schemaName t , T.pack "." , view tableName t]
    setter = undefined
