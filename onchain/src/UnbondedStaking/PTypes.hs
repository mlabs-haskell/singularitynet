{-# LANGUAGE UndecidableInstances #-}
{- This module contains orphan instances. This is because of two reasons:

    * Some Plutarch types don't have `PTryFrom` instances upstream. This might
      be related to the fact that we are not using Plutarch staging, but rather
      the master version with a backport of the TryFrom feature

    * The module `Types` needs to be compiled separately in the off-chain side
      as well, which does not have Plutarch. Because of this, `PConstant`
      instances need to be defined here instead
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnbondedStaking.PTypes (
  PBoolData (..),
  PEntry,
  PEntryFields,
  PEntryHRec,
  PUnbondedPoolParams (..),
  PUnbondedPoolParamsHRec,
  PUnbondedPoolParamsFields,
  PUnbondedStakingAction (..),
  PUnbondedStakingDatum (..),
) where

{-
 This module contains all the Plutarch-level synonyms of the types defined in
 `Types`. There are also other Plutarch types that are only used internally (like
 `PPeriod`)
-}

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import PNatural (PNatRatio, PNatural)

import Plutarch.Api.V1 (PMaybeData, PPOSIXTime)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  HRec,
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.TryFrom (PTryFrom)

import PTypes (
  HField,
  PAssetClass,
  PBurningAction,
  PMintingAction,
 )

import UnbondedStaking.Types (
  Entry,
  UnbondedPoolParams,
  UnbondedStakingAction,
  UnbondedStakingDatum,
 )

----- Plutarch synonyms -----

-- | `BondedPoolParams` synonym
newtype PUnbondedPoolParams (s :: S)
  = PUnbondedPoolParams
      ( Term
          s
          ( PDataRecord
              '[ "start" ':= PPOSIXTime
               , "userLength" ':= PPOSIXTime
               , "adminLength" ':= PPOSIXTime
               , "bondingLength" ':= PPOSIXTime
               , "interestLength" ':= PPOSIXTime
               , "increments" ':= PNatural
               , "interest" ':= PNatRatio
               , "minStake" ':= PNatural
               , "maxStake" ':= PNatural
               , "admin" ':= PPubKeyHash
               , "unbondedAssetClass" ':= PAssetClass
               , "nftCs" ':= PCurrencySymbol
               , "assocListCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PUnbondedPoolParams

deriving via
  PAsData (PIsDataReprInstances PUnbondedPoolParams)
  instance
    PTryFrom PData (PAsData PUnbondedPoolParams)

instance PUnsafeLiftDecl PUnbondedPoolParams where
  type PLifted PUnbondedPoolParams = UnbondedPoolParams

-- | `Entry` synonym
data PEntry (s :: S)
  = PEntry
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PByteString
               , "deposited" ':= PNatural
               , "newDeposit" ':= PNatural
               , "rewards" ':= PNatRatio
               , "totalRewards" ':= PNatural
               , "totalDeposited" ':= PNatural
               , "open" ':= PBoolData
               , "next" ':= PMaybeData PByteString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PEntry

deriving via
  PAsData (PIsDataReprInstances PEntry)
  instance
    PTryFrom PData (PAsData PEntry)

instance PUnsafeLiftDecl PEntry where
  type PLifted PEntry = Entry

-- | `BondedStakingDatum` synonym
data PUnbondedStakingDatum (s :: S)
  = PStateDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PMaybeData PByteString
               , "_1" ':= PBoolData
               ]
          )
      )
  | PEntryDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PEntry
               ]
          )
      )
  | PAssetDatum (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PUnbondedStakingDatum

deriving via
  PAsData (PIsDataReprInstances PUnbondedStakingDatum)
  instance
    (PTryFrom PData (PAsData PUnbondedStakingDatum))

instance PUnsafeLiftDecl PUnbondedStakingDatum where
  type PLifted PUnbondedStakingDatum = UnbondedStakingDatum

-- | `UnbondedStakingAction` synonym
data PUnbondedStakingAction (s :: S)
  = PAdminAct (Term s (PDataRecord '[]))
  | PStakeAct
      ( Term
          s
          ( PDataRecord
              '[ "stakeAmount" ':= PNatural
               , "pubKeyHash" ':= PPubKeyHash
               , "maybeMintingAction" ':= PMaybeData PMintingAction
               ]
          )
      )
  | PWithdrawAct
      ( Term
          s
          ( PDataRecord
              '[ "pubKeyHash" ':= PPubKeyHash
               , "burningAction" ':= PBurningAction
               ]
          )
      )
  | PCloseAct (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PUnbondedStakingAction

deriving via
  PAsData (PIsDataReprInstances (PMaybeData PMintingAction))
  instance
    PTryFrom PData (PAsData (PMaybeData PMintingAction))

deriving via
  PAsData (PIsDataReprInstances PUnbondedStakingAction)
  instance
    PTryFrom PData (PAsData PUnbondedStakingAction)

instance PUnsafeLiftDecl PUnbondedStakingAction where
  type PLifted PUnbondedStakingAction = UnbondedStakingAction

----- Type synonyms -----

-- | HRec with all of `PEntry`'s fields
type PEntryHRec (s :: S) =
  HRec
    '[ HField s "key" PByteString
     , HField s "deposited" PNatural
     , HField s "newDeposit" PNatural
     , HField s "rewards" PNatRatio
     , HField s "totalRewards" PNatural
     , HField s "totalDeposited" PNatural
     , HField s "open" PBoolData
     , HField s "next" (PMaybeData PByteString)
     ]

-- | Type level list with all of `PEntry`'s fields
type PEntryFields =
  '[ "key"
   , "deposited"
   , "newDeposit"
   , "rewards"
   , "totalRewards"
   , "totalDeposited"
   , "open"
   , "next"
   ]

-- | HRec with all of `PUnbondedPoolParams`'s fields
type PUnbondedPoolParamsHRec (s :: S) =
  HRec
    '[ HField s "start" PPOSIXTime
     , HField s "userLength" PPOSIXTime
     , HField s "adminLength" PPOSIXTime
     , HField s "bondingLength" PPOSIXTime
     , HField s "interestLength" PPOSIXTime
     , HField s "increments" PNatural
     , HField s "interest" PNatRatio
     , HField s "minStake" PNatural
     , HField s "maxStake" PNatural
     , HField s "admin" PPubKeyHash
     , HField s "unbondedAssetClass" PAssetClass
     , HField s "nftCs" PCurrencySymbol
     , HField s "assocListCs" PCurrencySymbol
     ]

-- | Type level list with all of `PUnbondedPoolParams's field names
type PUnbondedPoolParamsFields =
  '[ "start"
   , "userLength"
   , "adminLength"
   , "bondingLength"
   , "interestLength"
   , "increments"
   , "interest"
   , "minStake"
   , "maxStake"
   , "admin"
   , "unbondedAssetClass"
   , "nftCs"
   , "assocListCs"
   ]

------ PBool data instance ------

data PBoolData (s :: S)
  = PDFalse (Term s (PDataRecord '[]))
  | PDTrue (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBoolData

deriving via
  PAsData (PIsDataReprInstances PBoolData)
  instance
    PTryFrom PData (PAsData PBoolData)

---- PConstant instances ----

deriving via
  (DerivePConstantViaData UnbondedPoolParams PUnbondedPoolParams)
  instance
    (PConstant UnbondedPoolParams)

deriving via
  (DerivePConstantViaData Entry PEntry)
  instance
    (PConstant Entry)

deriving via
  (DerivePConstantViaData UnbondedStakingDatum PUnbondedStakingDatum)
  instance
    (PConstant UnbondedStakingDatum)

deriving via
  (DerivePConstantViaData UnbondedStakingAction PUnbondedStakingAction)
  instance
    (PConstant UnbondedStakingAction)
