{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module LightGBM.Wrapper.Bindings
  ( lgbmDatasetCreateFromFile
  , lgbmDatasetFree
  , lgbmDatasetDumpText
  )
where

import           Data.ByteString                ( ByteString
                                                , useAsCString
                                                )
import           LightGBM.Wrapper.LGBM
import qualified Language.C.Inline.Cpp         as C
import           Foreign.C.Types                ( CInt(..) )
import           Foreign.Ptr                    ( Ptr )
import           Foreign.C.String               ( withCString )

C.context (C.cppCtx <> lgbmCtx)
C.include "LightGBM/c_api.h"

lgbmDatasetCreateFromFile
  :: FilePath -> ByteString -> DatasetHandle -> Ptr DatasetHandle -> IO CInt
lgbmDatasetCreateFromFile filename parameters reference out =
  withCString filename $ \filename' ->
    useAsCString parameters $ \parameters' -> [C.exp| int {
  LGBM_DatasetCreateFromFile($(const char *filename'),
                             $(const char *parameters'),
                             $(const DatasetHandle reference),
                             $(DatasetHandle *out))
} |]

lgbmDatasetFree :: DatasetHandle -> IO CInt
lgbmDatasetFree handle = [C.exp| int {
  LGBM_DatasetFree($(DatasetHandle handle))
} |]

lgbmDatasetDumpText :: DatasetHandle -> FilePath -> IO CInt
lgbmDatasetDumpText handle filename = withCString filename $ \filename' ->
  [C.exp| int {
  LGBM_DatasetDumpText($(DatasetHandle handle),
                       $(const char * filename'))
} |]
