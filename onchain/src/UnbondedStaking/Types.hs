{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module UnbondedStaking.Types (
  Entry (..),
  UnbondedPoolParams (..),
  UnbondedStakingAction (..),
  UnbondedStakingDatum (..),
) where

{-
  This module contains all the Haskell-level types used throughout the project.

  Since this needs to be shared in the off-chain and on-chain side, *no*
  Plutarch or fancy GHC9 features are allowed. The Plutarch synonyms for these
  types are in `PTypes`
-}

import GHC.Generics qualified as GHC

import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime, PubKeyHash)
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString)

import SingularityNet.Natural (
  NatRatio,
  Natural,
 )
import SingularityNet.Types (AssetClass, BurningAction, MintingAction)

{- | Unbonded pool's parameters

     These parametrise the staking pool contract. However, the one parameter
     that makes each contract truly unique is `nftCs` (the NFT's
     CurrencySymbol).

     The currency symbol of the associated list (`assocListCs`) is also uniquely
     associated to the `nftCs`.
-}
data UnbondedPoolParams = UnbondedPoolParams
  { start :: POSIXTime -- absolute time
  , userLength :: POSIXTime -- a time delta
  , adminLength :: POSIXTime -- a time delta
  , bondingLength :: POSIXTime -- a time delta
  , interestLength :: POSIXTime -- a time delta
  , increments :: Natural
  , interest :: NatRatio -- interest per increment
  , minStake :: Natural
  , maxStake :: Natural
  , admin :: PubKeyHash
  , unbondedAssetClass :: AssetClass
  , nftCs :: CurrencySymbol -- this uniquely parametrizes the validator
  , assocListCs :: CurrencySymbol -- CurrencySymbol for on-chain associated list UTXOs
  }
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''UnbondedPoolParams

{- | Associacion list's entry

     An entry in the association list. It keeps track of how much a user staked
     and the pending rewards. It also has a reference to the next entry in the
     list, which might be empty if it is the final element.
-}
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

{- | Unbonded pool's state

     It can either contain:

     1. A reference to the on-chain association list of stakees-stakes (in the
     case of the pool UTXO)

     2. An entry in the association list (created by the stakers when using
     the StakeAct redeemer)

     3. A dummy datum (in the case of the stake UTXOs)
-}
data UnbondedStakingDatum
  = StateDatum (Maybe BuiltinByteString) Bool
  | EntryDatum Entry
  | AssetDatum
  deriving stock (Show, GHC.Generic)

unstableMakeIsData ''UnbondedStakingDatum

{- | Validator redeemers

     These are used by the admin to deposit the rewards, close the pool and
     withdraw the unclaimed rewards.

     These are used by the stakers to deposit their first stake or update their
     already existing stake.

     When the stake-holder makes their first deposit, the redeemer will be
     `StakeAct amt pkh (Just mintAct)`, where `mintAct` specifies the type of
     insertion that needs to checked by the validator.

     When the stake-holder wants to update their existing stake, the last field
     is set to `Nothing`, since no entry needs to be inserted or removed from
     the list.

     When the stake-holder wants to claim the their rewards, the redeemer will
     be `WithdrawAct pkh burnAct`, where `burnAct` specifies the type of removal
     that needs to be checked by the validator.
-}
data UnbondedStakingAction
  = AdminAct
  | StakeAct Natural PubKeyHash (Maybe MintingAction)
  | WithdrawAct PubKeyHash BurningAction
  | CloseAct
  deriving stock (Show)

unstableMakeIsData ''UnbondedStakingAction
