{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module LightGBM.Wrapper.Bindings
  ( lgbmDatasetCreateFromFile
  , lgbmDatasetFree
  , lgbmDatasetDumpText
  , lgbmDatasetGetNumFeature
  , lgbmDatasetGetNumData
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
import           Foreign.Marshal.Alloc          ( alloca )
import           Control.Monad.Extra            ( whenMaybeM )
import           Data.Coerce                    ( coerce
                                                , Coercible
                                                )
import           Data.Int                       ( Int32 )
import           Foreign.Storable               ( Storable
                                                , peek
                                                )

C.context (C.cppCtx <> lgbmCtx)
C.include "LightGBM/c_api.h"

lgbmDatasetCreateFromFile
  :: FilePath -> ByteString -> DatasetHandle -> Ptr DatasetHandle -> IO Bool
lgbmDatasetCreateFromFile filename parameters reference out =
  fmap isNoError $ withCString filename $ \filename' ->
    useAsCString parameters $ \parameters' -> [C.exp| int {
  LGBM_DatasetCreateFromFile($(const char *filename'),
                             $(const char *parameters'),
                             $(const DatasetHandle reference),
                             $(DatasetHandle *out))
} |]

lgbmDatasetFree :: DatasetHandle -> IO Bool
lgbmDatasetFree handle = fmap
  isNoError
  [C.exp| int {
  LGBM_DatasetFree($(DatasetHandle handle))
} |]

lgbmDatasetDumpText :: DatasetHandle -> FilePath -> IO Bool
lgbmDatasetDumpText handle filename =
  fmap isNoError $ withCString filename $ \filename' -> [C.exp| int {
  LGBM_DatasetDumpText($(DatasetHandle handle),
                       $(const char * filename'))
} |]

lgbmDatasetGetNumFeature :: DatasetHandle -> IO (Maybe Int32)
lgbmDatasetGetNumFeature handle = alloca $ \numFeature -> whenLGBM
  [C.exp| int {
  LGBM_DatasetGetNumFeature($(DatasetHandle handle),
                            $(int * numFeature))
} |]
  numFeature

lgbmDatasetGetNumData :: DatasetHandle -> IO (Maybe Int32)
lgbmDatasetGetNumData handle = alloca $ \numData -> whenLGBM
  [C.exp| int {
  LGBM_DatasetGetNumData($(DatasetHandle handle),
                         $(int * numData))
} |]
  numData

whenLGBM
  :: (Eq a, Num a, Storable b, Coercible b c) => IO a -> Ptr b -> IO (Maybe c)
whenLGBM ioErr ptr = whenMaybeM (isNoError <$> ioErr) (coerce <$> peek ptr)

isNoError :: (Eq a, Num a) => a -> Bool
isNoError = (== 0)
