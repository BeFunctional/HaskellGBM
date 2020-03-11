{-# LANGUAGE OverloadedStrings #-}

module LightGBM.Wrapper.Dataset
  ( withDataset
  )
where

import           LightGBM.Wrapper.Bindings
import           LightGBM.Wrapper.LGBM
import           Foreign.Ptr                    ( nullPtr )
import           System.FilePath                ( FilePath )
import           Foreign.Marshal.Alloc          ( alloca )
import           Foreign.Storable               ( peek )
import           Control.Exception              ( bracket )

withDataset :: FilePath -> (DatasetHandle -> IO a) -> IO a
withDataset filePath go = alloca $ \datasetHandle ->
  bracket (createDataset filePath "" datasetHandle) freeDataset go
 where
  createDataset cFilePath cParameters datasetHandle = do
    _ <- lgbmDatasetCreateFromFile cFilePath cParameters nullPtr datasetHandle
    peek datasetHandle
  freeDataset = lgbmDatasetFree
