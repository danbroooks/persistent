{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.DeepSeq
import           Control.DeepSeq.Generics
import           Criterion.Main
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.PprLib

import           Database.Persist.Types
import           Database.Persist.Quasi
import           Database.Persist.TH
import           Models

main :: IO ()
main = do
  -- let ents = $(persistFileWith lowerCaseSettings "bench/model-relations-6")
  -- printCount ents
  -- printTH $(persistFileWith lowerCaseSettings "bench/model-relations-1")
  defaultMain
    [ bgroup "mkPersist"
        [ bgroup "From File"
          -- [ bench "models-slowly" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/models-slowly")
          [ bench "model-relations-1" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-1")
          , bench "model-relations-2" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-2")
          , bench "model-relations-3" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-3")
          , bench "model-relations-4" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-4")
          , bench "model-relations-5" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-5")
          , bench "model-relations-6" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-6")
          , bench "model-relations-7" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-7")
          , bench "model-relations-8" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-8")
          , bench "model-relations-9" $ nfIO $ mkPersist' $(persistFileWith lowerCaseSettings "bench/model-relations-9")
          ]
        --, bgroup "Non-Null Fields"
        --    , bgroup "Increasing model count"
        --        [ bench "1x10" $ nfIO $ mkPersist' $( parseReferencesQ (mkModels 10 10))
        --        , bench "10x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 10))
        --        , bench "100x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 100 10))
        --        -- , bench "1000x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 1000 10))
        --        ]
        --    , bgroup "Increasing field count"
        --        [ bench "10x1" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 1))
        --        , bench "10x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 10))
        --        , bench "10x100" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 100))
        --        -- , bench "10x1000" $ nfIO $ mkPersist' $(parseReferencesQ (mkModels 10 1000))
        --        ]
        --    ]
        --, bgroup "Nullable"
        --    [ bgroup "Increasing model count"
        --        [ bench "20x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 20 10))
        --        , bench "40x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 40 10))
        --        , bench "60x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 60 10))
        --        , bench "80x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 80 10))
        --        , bench "100x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 100 10))
        --        -- , bench "1000x10" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 1000 10))
        --        ]
        --    , bgroup "Increasing field count"
        --        [ bench "10x20" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 20))
        --        , bench "10x40" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 40))
        --        , bench "10x60" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 60))
        --        , bench "10x80" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 80))
        --        , bench "10x100" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 100))
        --        -- , bench "10x1000" $ nfIO $ mkPersist' $(parseReferencesQ (mkNullableModels 10 1000))
        --        ]
        --    ]
        ]
    ]

printCount :: [a] -> IO ()
printCount = print . length

trim = unwords . words

printTH expr =
  mapM_ (mapM_ putStrLn . prepDec) =<< mkPersist' expr

prepDec :: Dec -> [String]
prepDec d = lines . doReplace =<< lines (stripStrArray $ pprint d)

replace' a b =
  T.replace (T.pack a) (T.pack b)

removeText str =
  replace' str ""

textOp f =  T.unpack . f . T.pack

stripStrArray =
  decodeLines . doStrip . trim . encodeLines
  where
  doStrip =
    textOp (removeText "',@@@ '")

  encodeLines =
    textOp (replace' "\n" "@@@")

  decodeLines =
    textOp (replace' "@@@" "\n")

doReplace = textOp $
        {-
      . removeText "Data.Text."
      . removeText "Database.Persist.Class."
      . removeText "Database.Persist.Sql.Types.Internal."
      . removeText "Database.Persist.Sql.Types."
      . removeText "Database.Persist.Types.Base."
      . removeText "Database.Persist.TH."
-}
      replace' "Database.Persist.Class.PersistEntity.Key" "Key"
      . replace' "Database.Persist.Class.PersistEntity.entityKey" "entityKey"
      . replace' "Database.Persist.Class.PersistEntity.entityVal" "entityVal"
      . replace' "Database.Persist.TH.lensPTH" "lensPTH"
      . replace' "Database.Persist.TH.packPTH" "packPTH"
      . replace' "Database.Persist.Class.PersistField.PersistField" "PersistField"
      . replace' "Database.Persist.Class.PersistEntity.PersistEntity" "PersistEntity"
      . replace' "Database.Persist.Class.PersistEntity.Unique" "Unique"
      . replace' "Database.Persist.Class.PersistEntity.Entity" "Entity"
      . replace' "Database.Persist.Class.PersistEntity.persistFieldDef" "persistFieldDef"
      . replace' "Database.Persist.Class.PersistEntity.persistUniqueKeys" "persistUniqueKeys"
      . replace' "Database.Persist.Class.PersistEntity.persistUniqueToValues" "persistUniqueToValues"
      . replace' "Database.Persist.Class.PersistEntity.persistUniqueToFieldnames" "persistUniqueToFieldnames"
      . replace' "Database.Persist.Class.PersistEntity.toPersistFields" "toPersistFields"
      . replace' "Database.Persist.Class.PersistEntity.fromPersistFields" "fromPersistFields"
      . replace' "Database.Persist.Class.PersistEntity.toPersistValues" "toPersistValues"
      . replace' "Database.Persist.Class.PersistEntity.fromPersistValues" "fromPersistValues"
      . replace' "Database.Persist.Class.PersistEntity.persistUniqueToFieldNames" "persistUniqueToFieldNames"
      . replace' "Database.Persist.Class.PersistEntity.persistIdField" "persistIdField"
      . replace' "Database.Persist.Class.PersistEntity.fieldLens" "fieldLens"
      . replace' "Database.Persist.Class.PersistEntity.keyToValues" "keyToValues"
      . replace' "Database.Persist.Class.PersistEntity.keyFromValues" "keyFromValues"
      . replace' "Database.Persist.Class.PersistField.toPersistValue" "toPersistValue"
      . replace' "Database.Persist.Class.PersistField.fromPersistValue" "fromPersistValue"
      . replace' "Database.Persist.Class.PersistStore.fromBackendKey" "fromBackendKey"
      . replace' "Database.Persist.Class.PersistStore.toBackendKey" "toBackendKey"
      . replace' "Database.Persist.Class.PersistStore.ToBackendKey" "ToBackendKey"
      . replace' "Database.Persist.Class.PersistStore.BackendKey" "BackendKey"
      . replace' "Database.Persist.Sql.Types.Internal.SqlBackend" "SqlBackend"
      . replace' "Database.Persist.Types.Base.DBName" "DBName"
      . replace' "Database.Persist.Types.Base.FieldDef" "FieldDef"
      . replace' "Database.Persist.Types.Base.EntityDef" "EntityDef"
      . replace' "Database.Persist.Types.Base.EmbedEntityDef" "EmbedEntityDef"
      . replace' "Database.Persist.Types.Base.EmbedFieldDef" "EmbedFieldDef"
      . replace' "Database.Persist.Types.Base.EmbedRef" "EmbedRef"
      . replace' "Database.Persist.Types.Base.ForeignRef" "\n\nForeignRef"
      . replace' "Database.Persist.Types.Base.FTTypeCon" "FTTypeCon"
      . replace' "Database.Persist.Types.Base.HaskellName" "HaskellName"
      . replace' "Database.Persist.Types.Base.SqlInt64" "SqlInt64"
      . replace' "Database.Persist.Types.Base.SqlString" "SqlString"
      . replace' "Data.Aeson.Types.FromJSON.FromJSON" "FromJSON"
      . replace' "Data.Aeson.Types.ToJSON.ToJSON" "ToJSON"
      . replace' "Web.PathPieces.PathPiece" "PathPiece"
      . removeText "Data.Either."
      . removeText "Data.Functor."
      . removeText "Data.Map."
      . removeText "GHC.Base."
      . removeText "GHC.Classes."
      . removeText "GHC.Err."
      . removeText "GHC.Maybe."
      . removeText "GHC.Read."
      . removeText "GHC.Show."
      . removeText "GHC.Types."

-- Orphan instances for NFData Template Haskell types
instance NFData Overlap where
    rnf = genericRnf

instance NFData AnnTarget where
    rnf = genericRnf

instance NFData PatSynDir where
    rnf = genericRnf

instance NFData PatSynArgs where
    rnf = genericRnf

instance NFData RuleBndr where
    rnf = genericRnf

instance NFData Role where
    rnf = genericRnf

instance NFData Phases where
    rnf = genericRnf

instance NFData InjectivityAnn where
    rnf = genericRnf

instance NFData FamilyResultSig where
    rnf = genericRnf

instance NFData RuleMatch where
    rnf = genericRnf

instance NFData TypeFamilyHead where
    rnf = genericRnf

instance NFData TySynEqn where
    rnf = genericRnf

instance NFData Inline where
    rnf = genericRnf

instance NFData Pragma where
    rnf = genericRnf

instance NFData FixityDirection where
    rnf = genericRnf

instance NFData Safety where
    rnf = genericRnf

instance NFData Fixity where
    rnf = genericRnf

instance NFData Callconv where
    rnf = genericRnf

instance NFData Foreign where
    rnf = genericRnf

instance NFData SourceStrictness where
    rnf = genericRnf

instance NFData SourceUnpackedness where
    rnf = genericRnf

instance NFData FunDep where
    rnf = genericRnf

instance NFData Bang where
    rnf = genericRnf

instance NFData DerivStrategy where
    rnf = genericRnf

instance NFData DerivClause where
    rnf = genericRnf

instance NFData Con where
    rnf = genericRnf

instance NFData Range where
    rnf = genericRnf

instance NFData Clause where
    rnf = genericRnf

instance NFData PkgName where
    rnf = genericRnf

instance NFData Dec where
    rnf = genericRnf

instance NFData Stmt where
    rnf = genericRnf

instance NFData TyLit where
    rnf = genericRnf

instance NFData NameSpace where
    rnf = genericRnf

instance NFData Body where
    rnf = genericRnf

instance NFData Guard where
    rnf = genericRnf

instance NFData Match where
    rnf = genericRnf

instance NFData ModName where
    rnf = genericRnf

instance NFData Pat where
    rnf = genericRnf

instance NFData TyVarBndr where
    rnf = genericRnf

instance NFData NameFlavour where
    rnf = genericRnf

instance NFData Type where
    rnf = genericRnf

instance NFData Exp where
    rnf = genericRnf

instance NFData Lit where
    rnf = genericRnf

instance NFData OccName where
    rnf = genericRnf

instance NFData Name where
    rnf = genericRnf
