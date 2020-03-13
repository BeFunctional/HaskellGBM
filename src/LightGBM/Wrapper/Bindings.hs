{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

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
                                                , packCString
                                                )
import           LightGBM.Wrapper.LGBM
import qualified Language.C.Inline.Cpp         as C
import           Foreign.C.Types                ( CInt(..) )
import           Foreign.C.String               ( withCString )
import           Foreign.Marshal.Alloc          ( alloca )
import           Foreign.Storable               ( peek )

C.context (C.cppCtx <> lgbmCtx)
C.include "LightGBM/c_api.h"

newtype LGBMError = LGBMError ByteString deriving (Show)

checkError :: (Eq a, Num a) => IO a -> IO b -> IO (Either LGBMError b)
checkError errorCode value = errorCode >>= \case
  0 -> Right <$> value
  _ -> Left . LGBMError <$> lgbmGetLastError

lgbmGetLastError :: IO ByteString
lgbmGetLastError =
  [C.exp| const char * { LGBM_GetLastError() } |] >>= packCString

lgbmDatasetCreateFromFile
  :: FilePath
  -> ByteString
  -> DatasetHandle
  -> IO (Either LGBMError DatasetHandle)
lgbmDatasetCreateFromFile filename parameters reference = alloca $ \dataset ->
  checkError
      (withCString filename $ \filename' ->
        useAsCString parameters $ \parameters' -> [C.exp| int {
  LGBM_DatasetCreateFromFile($(const char *filename'),
                             $(const char *parameters'),
                             $(const DatasetHandle reference),
                             $(DatasetHandle *dataset))
} |]
      )
    $ peek dataset

lgbmDatasetFree :: DatasetHandle -> IO (Either LGBMError ())
lgbmDatasetFree handle = checkError
  [C.exp| int {
  LGBM_DatasetFree($(DatasetHandle handle))
} |]
  mempty

lgbmDatasetDumpText :: DatasetHandle -> FilePath -> IO (Either LGBMError ())
lgbmDatasetDumpText handle filename = checkError
  (withCString filename $ \filename' -> [C.exp| int {
  LGBM_DatasetDumpText($(DatasetHandle handle),
                       $(const char * filename'))
} |]
  )
  mempty

lgbmDatasetGetNumFeature :: DatasetHandle -> IO (Either LGBMError Int)
lgbmDatasetGetNumFeature handle = alloca $ \numFeature ->
  checkError [C.exp| int {
  LGBM_DatasetGetNumFeature($(DatasetHandle handle),
                            $(int * numFeature))
} |]
    $   fromIntegral
    <$> peek numFeature

lgbmDatasetGetNumData :: DatasetHandle -> IO (Either LGBMError Int)
lgbmDatasetGetNumData handle = alloca $ \numData ->
  checkError [C.exp| int {
  LGBM_DatasetGetNumData($(DatasetHandle handle),
                         $(int * numData))
} |]
    $   fromIntegral
    <$> peek numData
