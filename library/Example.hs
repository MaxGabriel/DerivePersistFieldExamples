{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An example module.
module Example (main) where

import TH

-- | An example function.
main :: IO ()
main = return ()


data MyEnum = Foo | Bar deriving (Show)

$(derivePostgresEnumFast ''MyEnum "my_enum")
