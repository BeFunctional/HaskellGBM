{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module LightGBM.Wrapper.LGBM
  ( lgbmCtx
  , BoosterHandle
  , DatasetHandle
  )
where

import           Language.C.Inline.Context      ( Context
                                                , TypesTable
                                                , ctxTypesTable
                                                )
import qualified Data.Map.Strict               as M
import           Language.C.Types               ( TypeSpecifier(TypeName) )
import           Foreign.Ptr                    ( Ptr )

lgbmCtx :: Context
lgbmCtx = mempty { ctxTypesTable = lgbmTypesTable }

data BoosterStruct
type BoosterHandle = Ptr BoosterStruct

data DatasetStruct
type DatasetHandle = Ptr DatasetStruct

lgbmTypesTable :: TypesTable
lgbmTypesTable = M.fromList
  [ (TypeName "DatasetHandle", [t| DatasetHandle |])
  , (TypeName "BoosterHandle", [t| BoosterHandle |])
  ]
