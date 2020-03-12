{-# LANGUAGE OverloadedStrings #-}

import           LightGBM.Wrapper.Dataset
import           LightGBM.Wrapper.Bindings

main :: IO ()
main = withDataset "examples/binary_classification/binary.train" $ \handle -> do
  _ <- lgbmDatasetDumpText handle "test/test.dump.csv"
  numFeature <- lgbmDatasetGetNumFeature handle
  numData <- lgbmDatasetGetNumData handle
  print numFeature
  print numData
