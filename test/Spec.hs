{-# LANGUAGE OverloadedStrings #-}

import           LightGBM.Wrapper.Dataset
import           LightGBM.Wrapper.Bindings

main :: IO ()
main = withDataset "test/test.csv" $ \handle -> do
  _ <- lgbmDatasetDumpText handle "test/test.dump.csv"
  putStrLn "Test suite not yet implementedd"
