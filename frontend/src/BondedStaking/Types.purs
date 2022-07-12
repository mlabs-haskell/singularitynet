module Types
  ( AssetClass(..)
  , BondedPoolParams(..)
  , BondedStakingAction(..)
  , BondedStakingDatum(..)
  , BurningAction(..)
  , Entry(..)
  , InitialBondedParams(..)
  , ListAction(..)
  , MintingAction(..)
  , PoolInfo(..)
  , StakingType(..)
  ) where

import Contract.Prelude

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , I
  , PlutusData(Constr)
  , PNil
  , genericFromData
  , genericToData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt (BigInt)
import TypeLevel.Nat (S, Z)

newtype AssetClass = AssetClass
  { currencySymbol :: CurrencySymbol
  , tokenName :: TokenName
  }

derive instance Generic AssetClass _
derive instance Newtype AssetClass _
derive instance Eq AssetClass

instance
  HasPlutusSchema AssetClass
    ( "AssetClass"
        :=
          ( "currencySymbol" := I CurrencySymbol
              :+ "tokenName"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData AssetClass where
  toData = genericToData

instance Show AssetClass where
  show = genericShow

newtype PoolInfo = PoolInfo
  { stateNftCs :: CurrencySymbol
  , assocListCs :: CurrencySymbol
  , poolAddr :: Address
  }

derive instance Generic PoolInfo _
derive instance Newtype PoolInfo _
derive instance Eq PoolInfo

instance Show PoolInfo where
  show = genericShow

-- TODO: Add missing `ToData` instances for POSIXTime and NatRatio.
newtype BondedPoolParams =
  BondedPoolParams
    { iterations :: Natural
    , start :: BigInt
    , end :: BigInt
    , userLength :: BigInt
    , bondingLength :: BigInt
    , interest :: Rational
    , minStake :: Natural
    , maxStake :: Natural
    , admin :: PaymentPubKeyHash
    , bondedAssetClass :: AssetClass
    , nftCs :: CurrencySymbol
    , assocListCs :: CurrencySymbol
    }

derive instance Generic BondedPoolParams _
derive instance Eq BondedPoolParams
derive instance Newtype BondedPoolParams _

instance Show BondedPoolParams where
  show = genericShow

newtype InitialBondedParams = InitialBondedParams
  { iterations :: Natural
  , start :: BigInt
  , end :: BigInt
  , userLength :: BigInt
  , bondingLength :: BigInt
  , interest :: Rational
  , minStake :: Natural
  , maxStake :: Natural
  , bondedAssetClass :: AssetClass
  }

derive instance Generic InitialBondedParams _
derive instance Newtype InitialBondedParams _
derive instance Eq InitialBondedParams

instance Show InitialBondedParams where
  show = genericShow

-- The generic intsance is given below this, but we use this one for compilation
-- speed.
-- We copy the order of the fields from the Haskell implementation
instance ToData BondedPoolParams where
  toData (BondedPoolParams params) =
    Constr zero
      [ toData params.iterations
      , toData params.start
      , toData params.end
      , toData params.userLength
      , toData params.bondingLength
      , toData params.interest
      , toData params.minStake
      , toData params.maxStake
      , toData params.admin
      , toData params.bondedAssetClass
      , toData params.nftCs
      , toData params.assocListCs
      ]

-- -- Add these back in when generic instances have faster compilation.
-- instance https://github.com/Plutonomicon/cardano-transaction-lib/issues/433
--   HasPlutusSchema BondedPoolParams
--     ( "BondedPoolParams"
--         :=
--           ( "iterations" := I Natural
--               :+ "start"
--               := I BigInt
--               :+ "end"
--               := I BigInt
--               :+ "userLength"
--               := I BigInt
--               :+ "bondingLength"
--               := I BigInt
--               :+ "interest"
--               := I Rational
--               :+ "minStake"
--               := I Natural
--               :+ "maxStake"
--               := I Natural
--               :+ "admin"
--               := I PaymentPubKeyHash
--               :+ "bondedAssetClass"
--               := I AssetClass
--               :+ "nftCs"
--               := I CurrencySymbol
--               :+ "assocListCs"
--               := I CurrencySymbol
--               :+ PNil
--           )
--         @@ Z
--         :+ PNil
--     )

-- instance ToData BondedPoolParams where
--   toData = genericToData

data BondedStakingDatum
  = StateDatum { maybeEntryName :: Maybe ByteArray }
  | EntryDatum { entry :: Entry }
  | AssetDatum

derive instance Generic BondedStakingDatum _
derive instance Eq BondedStakingDatum

instance
  HasPlutusSchema BondedStakingDatum
    ( "StateDatum"
        :=
          ( "maybeEntryName" := I (Maybe ByteArray)
              :+ PNil
          )
        @@ Z
        :+ "EntryDatum"
        :=
          ( "entry" := I Entry :+ PNil
          )
        @@ (S Z)
        :+ "AssetDatum"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance FromData BondedStakingDatum where
  fromData = genericFromData

instance ToData BondedStakingDatum where
  toData = genericToData

instance Show BondedStakingDatum where
  show = genericShow

data BondedStakingAction
  = AdminAct
  | StakeAct
      { stakeAmount :: Natural
      , stakeHolder :: PaymentPubKeyHash
      , mintingAction :: Maybe MintingAction
      }
  | WithdrawAct
      { stakeHolder :: PaymentPubKeyHash
      , burningAction :: BurningAction
      }
  | CloseAct

instance
  HasPlutusSchema BondedStakingAction
    ( "AdminAct" := PNil
        @@ Z
        :+ "StakeAct"
        :=
          ( "stakeAmount" := I Natural
              :+ "stakeHolder"
              := I PaymentPubKeyHash
              :+ "mintingAction"
              := I (Maybe MintingAction)
              :+ PNil
          )
        @@ (S Z)
        :+ "WithdrawAct"
        :=
          ( "stakeHolder" := I PaymentPubKeyHash
              :+ "burningAction"
              := I BurningAction
              :+ PNil
          )
        @@ (S (S Z))
        :+ "CloseAct"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

derive instance Generic BondedStakingAction _
derive instance Eq BondedStakingAction

instance FromData BondedStakingAction where
  fromData = genericFromData

instance ToData BondedStakingAction where
  toData = genericToData

data MintingAction
  = MintHead TransactionInput
  | MintInBetween TransactionInput TransactionInput
  | MintEnd TransactionInput

derive instance Generic MintingAction _
derive instance Eq MintingAction

instance Show MintingAction where
  show = genericShow

instance
  HasPlutusSchema MintingAction
    ( "MintHead" := PNil @@ Z
        :+ "MintInBetween"
        := PNil
        @@ (S Z)
        :+ "MintEnd"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance FromData MintingAction where
  fromData = genericFromData

instance ToData MintingAction where
  toData = genericToData

data BurningAction
  = BurnHead TransactionInput TransactionInput
  | BurnOther TransactionInput TransactionInput
  | BurnSingle TransactionInput

derive instance Generic BurningAction _
derive instance Eq BurningAction

instance Show BurningAction where
  show = genericShow

instance
  HasPlutusSchema BurningAction
    ( "BurnHead" := PNil @@ Z
        :+ "BurnOther"
        := PNil
        @@ (S Z)
        :+ "BurnSingle"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance FromData BurningAction where
  fromData = genericFromData

instance ToData BurningAction where
  toData = genericToData

newtype Entry =
  Entry
    { key :: ByteArray
    , newDeposit :: BigInt
    , deposited :: BigInt
    , staked :: BigInt
    , rewards :: Rational
    , next :: Maybe ByteArray
    }

derive instance Generic Entry _
derive instance Newtype Entry _
derive instance Eq Entry

instance
  HasPlutusSchema Entry
    ( "Entry"
        :=
          ( "key" := I ByteArray
              :+ "newDeposit"
              := I BigInt
              :+ "deposited"
              := I BigInt
              :+ "staked"
              := I BigInt
              :+ "rewards"
              := I Rational
              :+ "next"
              := I (Maybe ByteArray)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance FromData Entry where
  fromData = genericFromData

instance ToData Entry where
  toData = genericToData

instance Show Entry where
  show = genericShow

data StakingType = Bonded | Unbonded

data ListAction
  = ListInsert MintingAction
  | ListRemove BurningAction

derive instance Generic ListAction _

instance
  HasPlutusSchema ListAction
    ( "ListInsert" := PNil @@ Z
        :+ "ListRemove"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData ListAction where
  toData = genericToData

instance Show ListAction where
  show = genericShow