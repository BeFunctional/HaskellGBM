{-# LANGUAGE OverloadedStrings #-}

module LightGBM.Wrapper.Dataset
  ( withDataset
  )
where

import           LightGBM.Wrapper.Bindings
import           LightGBM.Wrapper.LGBM
import           Foreign.Ptr                    ( nullPtr )
import           System.FilePath                ( FilePath )
import           Control.Exception              ( bracket )

withDataset :: FilePath -> (DatasetHandle -> IO a) -> IO a
withDataset filePath go = do
  Just dataset <- lgbmDatasetCreateFromFile filePath "" nullPtr
  bracket (return dataset) lgbmDatasetFree go
