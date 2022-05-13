{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-- This module contains some orphan instances for common Plutarch datatypes. For
-- some reason, these are not available upstream.
{-# OPTIONS_GHC -Wno-orphans #-}

module UnbondedStaking.Types (
  UnbondedPoolParams (..),
  PUnbondedPoolParams,
  UnbondedStakingAction (..),
  PUnbondedStakingAction (..),
  UnbondedStakingDatum (..),
  PUnbondedStakingDatum (..),
  Entry (
    Entry,
    key,
    deposited,
    newDeposit,
    rewards,
    totalRewards,
    totalDeposited,
    open,
    next
  ),
  PEntry,
  AssetClass (..),
  PAssetClass (PAssetClass),
  passetClass,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.Api.V1 (PMaybeData, PPOSIXTime, PTokenName, PTxId, PTxOutRef)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.TryFrom (PTryFrom)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  TokenName,
 )
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString)

import Data.Natural (
  NatRatio,
  Natural,
  PNatRatio,
  PNatural,
 )

-- Orphan instance for `PMaybeData PByteString`
deriving via
  PAsData (PIsDataReprInstances (PMaybeData PByteString))
  instance
    PTryFrom PData (PAsData (PMaybeData PByteString))

-- Orphan instance for `PTxOutRef`
deriving via
  PAsData (PIsDataReprInstances PTxOutRef)
  instance
    PTryFrom PData (PAsData PTxOutRef)

-- Orphan instance for `PTxId`
deriving via
  PAsData (PIsDataReprInstances PTxId)
  instance
    PTryFrom PData (PAsData PTxId)

-- Orphan instance for `PCurrencySymbol`
deriving via
  DerivePNewtype (PAsData PCurrencySymbol) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PCurrencySymbol)

-- Orphan instance for `PPubKeyHash`
deriving via
  DerivePNewtype (PAsData PPubKeyHash) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PPubKeyHash)

-- Orphan instance for `PPOSIXTime`
deriving via
  DerivePNewtype (PAsData PPOSIXTime) (PAsData PInteger)
  instance
    PTryFrom PData (PAsData PPOSIXTime)

-- Orphan instance for `PTokenName`
deriving via
  DerivePNewtype (PAsData PTokenName) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PTokenName)

-- Orphan instance for `PBool`
deriving via
  DerivePNewtype (PAsData PBool) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PBool)

-- | An `AssetClass` is simply a wrapper over a pair (CurrencySymbol, TokenName)
newtype PAssetClass (s :: S)
  = PAssetClass
      ( Term
          s
          ( PDataRecord
              '[ "currencySymbol" ':= PCurrencySymbol
               , "tokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PAssetClass

deriving via
  PAsData (PIsDataReprInstances PAssetClass)
  instance
    PTryFrom PData (PAsData PAssetClass)

data AssetClass = AssetClass
  { acCurrencySymbol :: CurrencySymbol
  , acTokenName :: TokenName
  }
  deriving stock (GHC.Generic, Show)
  deriving anyclass (Generic)

unstableMakeIsData ''AssetClass

deriving via
  ( DerivePConstantViaData
      AssetClass
      PAssetClass
  )
  instance
    PConstant AssetClass

instance PUnsafeLiftDecl PAssetClass where
  type PLifted PAssetClass = AssetClass

passetClass ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
passetClass = phoistAcyclic $
  plam $ \cs tn ->
    pcon $
      PAssetClass $
        pdcons # pdata cs
          #$ pdcons # pdata tn # pdnil

{- | Unbonded pool's parameters

     These parametrise the staking pool contract. However, the one parameter
     that makes each contract truly unique is `nftCs` (the NFT's
     CurrencySymbol).

     The currency symbol of the associated list (`assocListCs`) is also uniquely
     associated to the `nftCs`.
-}
newtype PUnbondedPoolParams (s :: S)
  = PUnbondedPoolParams
      ( Term
          s
          ( PDataRecord
              '[ "upp'start" ':= PPOSIXTime
               , "upp'userLength" ':= PPOSIXTime
               , "upp'adminLength" ':= PPOSIXTime
               , "upp'bondingLength" ':= PPOSIXTime
               , "upp'interestLength" ':= PPOSIXTime
               , "upp'increments" ':= PNatural
               , "upp'interest" ':= PNatRatio
               , "upp'minStake" ':= PNatural
               , "upp'maxStake" ':= PNatural
               , "upp'admin" ':= PPubKeyHash
               , "upp'unbondedAssetClass" ':= PAssetClass
               , "upp'nftCs" ':= PCurrencySymbol
               , "upp'assocListCs" ':= PCurrencySymbol
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

data UnbondedPoolParams = UnbondedPoolParams
  { upp'start :: POSIXTime -- absolute time
  , upp'userLength :: POSIXTime -- a time delta
  , upp'adminLength :: POSIXTime -- a time delta
  , upp'bondingLength :: POSIXTime -- a time delta
  , upp'interestLength :: POSIXTime -- a time delta
  , upp'increments :: Natural
  , upp'interest :: NatRatio -- interest per increment
  , upp'minStake :: Natural
  , upp'maxStake :: Natural
  , upp'admin :: PubKeyHash -- #NOTE# Spec divergence in bonded (PaymentPubKeyHash)
  , upp'unbondedAssetClass :: AssetClass
  , upp'nftCs :: CurrencySymbol -- this uniquely parametrizes the validator
  , upp'assocListCs :: CurrencySymbol -- CurrencySymbol for on-chain associated list UTXOs
  }
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''UnbondedPoolParams

deriving via
  (DerivePConstantViaData UnbondedPoolParams PUnbondedPoolParams)
  instance
    (PConstant UnbondedPoolParams)

instance PUnsafeLiftDecl PUnbondedPoolParams where
  type PLifted PUnbondedPoolParams = UnbondedPoolParams

{- | Associacion list's entry

     An entry in the association list. It keeps track of how much a user staked
     and the pending rewards. It also has a reference to the next entry in the
     list, which might be empty if it is the final element.
-}
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
               , "open" ':= PBool
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

data Entry = Entry
  { key :: BuiltinByteString
  , deposited :: Natural
  , newDeposit :: Natural
  , rewards :: NatRatio
  , totalRewards :: Natural
  , totalDeposited :: Natural
  , open :: Bool
  , next :: Maybe BuiltinByteString
  }
  deriving stock (Show)

unstableMakeIsData ''Entry

deriving via
  (DerivePConstantViaData Entry PEntry)
  instance
    (PConstant Entry)

instance PUnsafeLiftDecl PEntry where
  type PLifted PEntry = Entry

{- | Unbonded pool's state

     It can either contain:

     1. A reference to the on-chain association list of stakees-stakes (in the
     case of the pool UTXO)

     2. An entry in the association list (created by the stakers when using
     the StakeAct redeemer)

     3. A dummy datum (in the case of the stake UTXOs)
-}
data PUnbondedStakingDatum (s :: S)
  = PStateDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PMaybeData PByteString
               , "_1" ':= PBool
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

data UnbondedStakingDatum
  = StateDatum (Maybe BuiltinByteString) Bool
  | EntryDatum Entry
  | AssetDatum
  deriving stock (Show, GHC.Generic)

unstableMakeIsData ''UnbondedStakingDatum

deriving via
  (DerivePConstantViaData UnbondedStakingDatum PUnbondedStakingDatum)
  instance
    (PConstant UnbondedStakingDatum)

instance PUnsafeLiftDecl PUnbondedStakingDatum where
  type PLifted PUnbondedStakingDatum = UnbondedStakingDatum

{- | Minting redeemers

     These are used for staking and withdrawing funds but they are *not* used
     for consuming the bonded pool's contract, but rather for minting the NFTs
     that comprise each entry in the association list.
-}
data PMintingAction (s :: S)
  = PStake (Term s (PDataRecord '[]))
  | PWithdraw (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PMintingAction

deriving via
  PAsData (PIsDataReprInstances PMintingAction)
  instance
    PTryFrom PData (PAsData PMintingAction)

data MintingAction
  = Stake
  | Withdraw
  deriving stock (GHC.Generic)

unstableMakeIsData ''MintingAction

deriving via
  (DerivePConstantViaData MintingAction PMintingAction)
  instance
    (PConstant MintingAction)

instance PUnsafeLiftDecl PMintingAction where
  type PLifted PMintingAction = MintingAction

{- | Validator redeemers

     These are used by the admin to deposit the rewards and close the pool and
     withdraw the rewards unclaimed.

     These are used by the stakers to deposit their *initial* stake (after that
     they only update their respective entry) and withdrawing their rewards.
-}
data PUnbondedStakingAction (s :: S)
  = PAdminAct
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PNatural
               , "_1" ':= PNatural
               ]
          )
      )
  | PStakeAct
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PNatural
               , "_1" ':= PPubKeyHash
               ]
          )
      )
  | PWithdrawAct (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PCloseAct (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PUnbondedStakingAction
  
deriving via
  PAsData (PIsDataReprInstances PUnbondedStakingAction)
  instance
    PTryFrom PData (PAsData PUnbondedStakingAction)

data UnbondedStakingAction
  = AdminAct Natural Natural
  | StakeAct Natural PubKeyHash
  | WithdrawAct PubKeyHash
  | CloseAct
  deriving stock (Show)

unstableMakeIsData ''UnbondedStakingAction

deriving via
  (DerivePConstantViaData UnbondedStakingAction PUnbondedStakingAction)
  instance
    (PConstant UnbondedStakingAction)

instance PUnsafeLiftDecl PUnbondedStakingAction where
  type PLifted PUnbondedStakingAction = UnbondedStakingAction
