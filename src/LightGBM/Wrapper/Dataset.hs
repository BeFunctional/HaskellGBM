module LightGBM.Wrapper.Dataset where

import           Foreign.C.String               ( CString
                                                , withCString
                                                )
import           Foreign.C.Types                ( CInt(..) )
import           Foreign.Ptr                    ( Ptr
                                                , nullPtr
                                                )
import           System.FilePath                ( FilePath )

data DatasetStruct = DatasetStruct
type DatasetHandle = Ptr DatasetStruct

foreign import ccall "LGBM_DatasetCreateFromFile" lgbmDatasetCreateFromFile :: CString -> CString -> DatasetHandle -> Ptr DatasetHandle -> CInt

newtype Dataset = Dataset
  { datasetHandle :: DatasetHandle
  }

withDataset :: FilePath -> (Dataset -> IO a) -> IO a
withDataset filePath go = withCString filePath $ \cFilePath ->
  withCString "" $ \cParameters -> do
    let _ = lgbmDatasetCreateFromFile cFilePath cParameters nullPtr nullPtr
    go $ Dataset nullPtr
