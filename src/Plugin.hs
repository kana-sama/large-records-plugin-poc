{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Plugin (plugin) where

import Control.Lens (to, use, (%=), (^.))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Writer (runWriterT, tell)
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import GHC.Generics (Generic)
import GHC.Hs
import GHC.IO (unsafePerformIO)
import GhcPlugins
import OrdList (mapOL)
import TcRnTypes (TcGblEnv (..), TcM)

plugin :: Plugin
plugin = defaultPlugin {renamedResultAction}
  where
    renamedResultAction _ env mod = do
      (mod, env) <- runStateT (transformModule mod) env
      pure (env, mod)

type ModuleLRs = Map ConName LRInfo

data LRInfo = LRInfo
  { constructor :: ConName,
    fields :: [RdrName]
  }
  deriving stock (Generic)

type LRM = StateT TcGblEnv TcM

-- | Internal plugin state, for sharing plugin results between modules
-- TODO: use files instead of IORef, because of partial compilation
{-# NOINLINE records #-}
records :: IORef (Map ModuleName ModuleLRs)
records = unsafePerformIO (newIORef Map.empty)

lookupInfo :: ConName -> LRM (Maybe LRInfo)
lookupInfo con = do
  mods <- liftIO do readIORef records
  case (moduleName . nameModule) con `Map.lookup` mods of
    Nothing -> pure Nothing
    Just recs -> pure (con `Map.lookup` recs)

-- | Removes `name` mention from `dus`
removeUse :: Name -> LRM ()
removeUse name = do
  #tcg_dus %= mapOL \case
    (Just ds, us) -> (Just (delFromNameSet ds name), delFromNameSet us name)
    (Nothing, us) -> (Nothing, delFromNameSet us name)

getModuleName :: LRM ModuleName
getModuleName = use (#tcg_mod . to moduleName)

-- | Transforms declarations, expressions and patterns
transformModule :: HsGroup GhcRn -> LRM (HsGroup GhcRn)
transformModule mod = do
  (mod, lrs) <- transformDecls mod
  moduleName <- getModuleName
  liftIO do modifyIORef' records (Map.insert moduleName lrs)
  (transformPats <=< transformExpr) mod

-- | Transforms
--
-- > {-# ANN type X "large-record" #-}
-- > data X = X {a :: Int, b :: String}
-- into
--
-- > newtype X = X (Int, String)
transformDecls :: HsGroup GhcRn -> LRM (HsGroup GhcRn, ModuleLRs)
transformDecls mod = runWriterT do
  let annos = Set.fromList [t | LRAnno t <- hs_annds mod]
  flip Uniplate.transformBiM mod \case
    decl@(viewRecordDecl -> Just rec) | tyName rec `Set.member` annos -> do
      -- Remove connection between constructor and it's fields
      -- It ruins DRF field resolver on renaming pass
      #tcg_field_env %= \e -> delFromNameEnv e (rec ^. #conName)
      tell (Map.singleton (conName rec) (recordDeclToInfo rec))
      pure (mkLRDecl rec)
    decl -> pure decl

-- | Transforms expression @X{b = 1, a = 2}@ into @X (2, 1)@
--
-- (for @data X = X{a b :: Int}@)
transformExpr :: HsGroup GhcRn -> LRM (HsGroup GhcRn)
transformExpr = Uniplate.transformBiM \case
  expr@RecordCon {rcon_con_name = L _ conName, rcon_flds = HsRecFields fields' _} ->
    lookupInfo conName >>= \case
      Just info -> do
        for_ (extractFields fields') removeUse
        let con = noLoc (HsVar NoExtField (noLoc (info ^. #constructor)))
        let val = [noLoc (lookup field fields') | field <- info ^. #fields]
        let tup = noLoc (ExplicitTuple NoExtField [noLoc (Present NoExtField v) | v <- val] Boxed)
        pure (HsApp NoExtField con tup)
      Nothing -> pure expr
  expr -> pure expr
  where
    lookup field = fromMaybe (error ("No field: " ++ rdrNameString field)) . lookupRecField field

-- | Transforms pattern @X{b = 1, a = 2}@ into @X (2, 1)@
--
-- (for @data X = X{a b :: Int}@)
transformPats :: HsGroup GhcRn -> LRM (HsGroup GhcRn)
transformPats = Uniplate.transformBiM \case
  pat@(ConPatIn n@(L _ conName) (RecCon (HsRecFields fields' _))) ->
    lookupInfo conName >>= \case
      Just info -> do
        for_ (extractFields fields') removeUse
        let pat = [noLoc (lookup field fields') | field <- info ^. #fields]
        pure (ConPatIn n (PrefixCon [noLoc (TuplePat NoExtField pat Boxed)]))
      Nothing -> pure pat
  pat -> pure pat
  where
    lookup field = fromMaybe (WildPat NoExtField) . lookupRecField field

-- | Extracts all field names from @{...}@
extractFields :: [LHsRecField GhcRn a] -> [Name]
extractFields fields = [f | L _ HsRecField {hsRecFieldLbl = L _ FieldOcc {extFieldOcc = f}} <- fields]

-- | Extact term, assigned to field @field@ in @{...}@
lookupRecField :: RdrName -> [LHsRecField GhcRn (Located (x GhcRn))] -> Maybe (x GhcRn)
lookupRecField field [] = Nothing
lookupRecField field (L _ (HsRecField (L _ (FieldOcc _ (L _ field'))) arg _) : bs) | field' == field = Just (unLoc arg)
lookupRecField field (_ : bs) = lookupRecField field bs

-- Misc

viewRecordDecl :: TyClDecl GhcRn -> Maybe RecordDecl
viewRecordDecl = \case
  DataDecl
    { tcdDExt = tyInfo,
      tcdLName = L _ tyName,
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
    } -> Just RecordDecl {tyInfo, tyName, args, conName, conFields, derivs, conDoc}
  _ -> Nothing
  where
    mkFields :: [LConDeclField GhcRn] -> [(RdrName, HsType GhcRn)]
    mkFields fields = do
      L _ ConDeclField {cd_fld_names = names, cd_fld_type = L _ ty} <- fields
      L _ (FieldOcc _ (L _ name)) <- names
      pure (name, ty)

mkLRDecl :: RecordDecl -> TyClDecl GhcRn
mkLRDecl RecordDecl {tyInfo, tyName, args, conName, conFields, derivs, conDoc} =
  DataDecl
    { tcdDExt = tyInfo,
      tcdLName = noLoc tyName,
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
                      con_args = PrefixCon [noLoc (HsTupleTy NoExtField HsBoxedTuple [noLoc ty | (_, ty) <- conFields])],
                      con_doc = conDoc
                    }
              ],
            dd_derivs = derivs -- TODO transform some stock derivs
          }
    }

pattern LRAnno :: Name -> LAnnDecl GhcRn
pattern LRAnno typeName <- L _ (HsAnnotation _ _ (TypeAnnProvenance (L _ typeName)) (L _ (HsLit _ (HsString _ ((\l -> fromString "large-record" == l) -> True)))))

deriving stock instance Generic TcGblEnv

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

type ConName = Name

-- | All required info about record declaration for large-record declaration construction
data RecordDecl = RecordDecl
  { tyInfo :: DataDeclRn,
    tyName :: Name,
    args :: LHsQTyVars GhcRn,
    conName :: ConName,
    conFields :: [(RdrName, HsType GhcRn)],
    derivs :: HsDeriving GhcRn,
    conDoc :: Maybe LHsDocString
  }
  deriving stock (Generic)

recordDeclToInfo :: RecordDecl -> LRInfo
recordDeclToInfo RecordDecl {conName, conFields} =
  LRInfo {constructor = conName, fields = [n | (n, _) <- conFields]}
