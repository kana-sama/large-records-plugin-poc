{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Plugin (plugin) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (runWriterT, tell)
import Data.Data (Data)
import Data.Generics.Labels ()
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import GHC.Generics (Generic)
import GHC.Hs
import GhcPlugins

deriving stock instance Generic HsParsedModule

deriving stock instance Generic (HsModule pass)

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

loc :: Traversal' (Located a) a
loc = traverse

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = \_ _ -> (#hpm_module . loc) transformModule}

transformModule :: HsModule GhcPs -> Hsc (HsModule GhcPs)
transformModule mod = do
  (mod, lrs) <- transformDecls mod
  mod <- transformConstructors lrs mod
  mod <- transformPats lrs mod
  pure mod

data RecordDecl = RecordDecl
  { tyName :: Located RdrName,
    args :: LHsQTyVars GhcPs,
    conName :: RdrName,
    conFields :: [(RdrName, HsType GhcPs)],
    derivs :: HsDeriving GhcPs,
    conDoc :: Maybe LHsDocString
  }

viewRecordDecl :: TyClDecl GhcPs -> Maybe RecordDecl
viewRecordDecl = \case
  DataDecl
    { tcdLName = tyName,
      tcdTyVars = args,
      tcdFixity = Prefix,
      tcdDataDefn =
        HsDataDefn
          { dd_ND = DataType,
            dd_ctxt = L _ [],
            dd_cType = Nothing,
            dd_kindSig = Nothing,
            dd_derivs = derivs,
            dd_cons =
              [ L
                  _
                  ConDeclH98
                    { con_name = L _ conName,
                      con_forall = L _ False,
                      con_ex_tvs = [],
                      con_mb_cxt = Nothing,
                      con_args = RecCon (L _ (mkFields -> conFields)),
                      con_doc = conDoc
                    }
                ]
          }
    } -> Just RecordDecl {tyName, args, conName, conFields, derivs, conDoc}
  _ -> Nothing
  where
    mkFields :: [LConDeclField GhcPs] -> [(RdrName, HsType GhcPs)]
    mkFields fields = do
      L _ ConDeclField {cd_fld_names = names, cd_fld_type = L _ ty} <- fields
      L _ (FieldOcc _ (L _ name)) <- names
      pure (name, ty)

mkLRDecl :: RecordDecl -> TyClDecl GhcPs
mkLRDecl RecordDecl {tyName, args, conName, conFields, derivs, conDoc} =
  DataDecl
    { tcdDExt = NoExtField,
      tcdLName = tyName,
      tcdTyVars = args,
      tcdFixity = Prefix,
      tcdDataDefn =
        HsDataDefn
          { dd_ext = NoExtField,
            dd_ND = NewType,
            dd_ctxt = noLoc [],
            dd_cType = Nothing,
            dd_kindSig = Nothing,
            dd_cons =
              [ noLoc
                  ConDeclH98
                    { con_ext = NoExtField,
                      con_name = noLoc conName,
                      con_forall = noLoc False,
                      con_ex_tvs = [],
                      con_mb_cxt = Nothing,
                      con_args = PrefixCon [noLoc (HsTupleTy NoExtField HsBoxedTuple [noLoc ty | (name, ty) <- conFields])],
                      con_doc = conDoc
                    }
              ],
            dd_derivs = derivs -- TODO transform some stock derivs
          }
    }

pattern LRAnno :: RdrName -> AnnDecl GhcPs
pattern LRAnno typeName <-
  HsAnnotation _ _ (TypeAnnProvenance (L _ typeName)) (L _ (HsLit _ (HsString _ ((\name -> fromString "large-record" == name) -> True))))

type LRs = Map RdrName RecordDecl

transformDecls :: HsModule GhcPs -> Hsc (HsModule GhcPs, LRs)
transformDecls mod = runWriterT do
  let annos = getLRAnnotations mod
  forOf (#hsmodDecls . each . loc) mod \case
    TyClD _ decl | unLoc (tcdLName decl) `Set.member` annos,
                   Just rec <- viewRecordDecl decl -> do
      liftIO do putStrLn ("Largify " ++ (rdrNameString . unLoc . tyName) rec)
      tell (Map.singleton (conName rec) rec)
      pure (TyClD NoExtField (mkLRDecl rec))
    decl -> pure decl
  where
    getLRAnnotations :: HsModule GhcPs -> Set RdrName
    getLRAnnotations mod = Set.fromList [t | L _ (AnnD _ (LRAnno t)) <- hsmodDecls mod]

transformConstructors :: LRs -> HsModule GhcPs -> Hsc (HsModule GhcPs)
transformConstructors lrs = Uniplate.transformBiM \case
  RecordCon {rcon_con_name = L _ conName', rcon_flds = HsRecFields fields' _}
    | Just rec <- conName' `Map.lookup` lrs -> do
      let con = noLoc (HsVar NoExtField (noLoc (conName rec)))
      let val = [noLoc (lookup name fields') | (name, _) <- conFields rec]
      let tup = noLoc (ExplicitTuple NoExtField [noLoc (Present NoExtField v) | v <- val] Boxed)
      pure (HsApp NoExtField con tup)
  expr -> pure expr
  where
    lookup field bs =
      case lookupRecField field bs of
        Just e -> e
        Nothing -> error ("No field: " ++ rdrNameString field)

transformPats :: LRs -> HsModule GhcPs -> Hsc (HsModule GhcPs)
transformPats lrs = Uniplate.transformBiM \case
  ConPatIn n@(L _ conName') (RecCon (HsRecFields fields' _)) | Just rec <- conName' `Map.lookup` lrs -> do
    let pat = [noLoc (lookup name fields') | (name, _) <- conFields rec]
    pure (ConPatIn n (PrefixCon [noLoc (TuplePat NoExtField pat Boxed)]))
  pat -> pure pat
  where
    lookup field bs =
      case lookupRecField field bs of
        Just p -> p
        Nothing -> WildPat NoExtField

lookupRecField :: RdrName -> [LHsRecField GhcPs (Located (x GhcPs))] -> Maybe (x GhcPs)
lookupRecField field [] = Nothing
lookupRecField field (L _ (HsRecField (L _ (FieldOcc _ (L _ field'))) arg _) : bs) | field' == field = Just (unLoc arg)
lookupRecField field (_ : bs) = lookupRecField field bs
