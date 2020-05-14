{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An example module.
module Example (main) where

import TH
import qualified MyEnum as MyEnum

-- | An example function.
main :: IO ()
main = return ()




$(derivePostgresEnumFast ''MyEnum.MyEnum "my_enum")
