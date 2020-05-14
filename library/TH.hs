{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TH where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib (varE)
import Database.Persist
import Database.Persist.Sql (PersistFieldSql, sqlType)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Language.Haskell.TH.Datatype



derivePostgresEnumFast :: Name -> String -> Q [Dec]
derivePostgresEnumFast typeName postgresType = do

  datatypeInfo <- reifyDatatype typeName
  constructorNames <- mapM getNormalConstructorName (datatypeCons datatypeInfo)

  toPersistValueMatches <- mapM constructorNameToMatch constructorNames
  let toPersistValueExp = LamCaseE toPersistValueMatches

  fromPersistValueGoodMatches <- mapM constructorNameFromMatch constructorNames
  fromPersistValueBadMatches <- fromPersistValueBadMatch typeName
  let fromPersistValueExp = LamCaseE (fromPersistValueGoodMatches <> [fromPersistValueBadMatches])

  sqlTypeImplementation <- [|SqlOther (T.pack postgresType)|]

  return
    [ InstanceD Nothing [] (ConT ''PersistField `AppT` (ConT typeName)) [ FunD 'toPersistValue
        [ Clause [] (NormalB toPersistValueExp) []
        ]
      , FunD 'fromPersistValue
        [ Clause [] (NormalB fromPersistValueExp) []
        ]
      ]
    , InstanceD Nothing [] (ConT ''PersistFieldSql `AppT` (ConT typeName)) [ FunD 'sqlType [ Clause [WildP] (NormalB sqlTypeImplementation) [] ] ]
    ]

-- | Given a constructor "Foo", generates @Foo -> PersistDbSpecific "Foo"@
constructorNameToMatch :: Name -> Q Match
constructorNameToMatch name = do
  let s = nameToOccNameString name
  (anExp :: Exp) <- [|PersistDbSpecific $ S8.pack s|]
  pure $ Match (ConP name []) (NormalB anExp) []

-- | Given a constructor "Foo", generates @"Foo" -> Right Foo@
constructorNameFromMatch :: Name -> Q Match
constructorNameFromMatch name = do
  let s = nameToOccNameString name
  let pat = ConP 'PersistDbSpecific [LitP (StringL s)]

  (rightConstructor :: Exp) <- [|Right|]
  let (result :: Exp) = rightConstructor `AppE` (ConE name)
  pure $ Match pat (NormalB result) []

-- | Generates a fallthrough pattern match for the given type name:
--
-- @unexpectedPersistValue -> Left "fromPersistField: When trying to deserialize a value of type MyEnum, got Baz"@
fromPersistValueBadMatch :: Name -> Q Match
fromPersistValueBadMatch typeName = do
  let s = nameToOccNameString typeName
  unexpectedPersistValueName <- newName "unexpectedPersistValue"
  -- TODO give clear error message
  errorCase <- [|Left $ T.pack $ "fromPersistField: When trying to deserialize a value of type " <> s <> ", got: " <> show $(varE unexpectedPersistValueName)|]
  -- let fullError = errorCase `AppE` VarE unexpectedPersistValueName
  pure $ Match (VarP unexpectedPersistValueName) (NormalB errorCase) []

getNormalConstructorName :: ConstructorInfo -> Q Name
getNormalConstructorName constructorInfo = case constructorVariant constructorInfo of
  NormalConstructor -> pure $ constructorName constructorInfo
  bad -> fail $ "derivePostgresEnumFast expects normal constructors, got: " <> show bad

nameToOccNameString :: Name -> String
nameToOccNameString (Name (OccName s) _) = s

