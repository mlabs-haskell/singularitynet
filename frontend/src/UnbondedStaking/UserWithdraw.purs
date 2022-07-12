module UnbondedStaking.UserWithdraw
  ( userWithdrawUnbondedPoolContract
  ) where

import Contract.Prelude hiding (length)

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , scriptHashAddress
  )
import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , logInfo'
  , throwContractError
  )
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustMintValueWithRedeemer
  , mustPayToPubKeyAddress
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value (Value, mkTokenName, singleton)
import Data.Array (catMaybes)
import Data.BigInt (BigInt, fromInt)
import Data.Map as Map
import Plutus.Conversion (fromPlutusAddress)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings (unbondedStakingTokenName)
import Types
  ( BurningAction(BurnHead, BurnOther, BurnSingle)
  , ListAction(ListRemove)
  , StakingType(Unbonded)
  )
import Types.Rational (Rational, denominator, numerator)
import Types.Redeemer (Redeemer(Redeemer))
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(WithdrawAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import UnbondedStaking.Utils (getBondingTime)
import Utils
  ( findRemoveOtherElem
  , getAssetsToConsume
  , mkAssetUtxosConstraints
  , getUtxoWithNFT
  , hashPkh
  , logInfo_
  , mkOnchainAssocList
  )

-- Deposits a certain amount in the pool
userWithdrawUnbondedPoolContract :: UnbondedPoolParams -> Contract () Unit
userWithdrawUnbondedPoolContract
  params@
    ( UnbondedPoolParams
        { unbondedAssetClass
        , nftCs
        , assocListCs
        }
    ) = do
  ---- FETCH BASIC INFORMATION ----
  -- Get network ID
  networkId <- getNetworkId
  -- Get own public key hash and compute hashed version
  userPkh <- liftedM "userWithdrawUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  hashedUserPkh <- liftAff $ hashPkh userPkh
  logInfo_ "userWithdrawUnbondedPoolContract: User's PaymentPubKeyHash" userPkh
  -- Get own staking hash
  userStakingPubKeyHash <-
    liftedM
      "userWithdrawnUnbondedPoolContract: Cannot get\
      \ user's staking pub key hash" $
      ownStakePubKeyHash
  -- Get the (Nami) wallet address
  userAddr <-
    liftedM "userWithdrawUnbondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "userWithdrawUnbondedPoolContract: Cannot get user Utxos"
      $ utxosAt userAddr
  ---- FETCH POOL DATA ----
  -- Get the unbonded pool validator and hash
  validator <-
    liftedE' "userWithdrawUnbondedPoolContract: Cannot create validator"
      $ mkUnbondedPoolValidator params
  valHash <-
    liftContractAffM "userWithdrawUnbondedPoolContract: Cannot hash validator"
      $ validatorHash validator
  logInfo_ "userWithdrawUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "userWithdrawUnbondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <-
    liftedM
      "userWithdrawUnbondedPoolContract: Cannot get pool's\
      \ utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "userWithdrawUnbondedPoolContract: Pool UTxOs" unbondedPoolUtxos
  -- Get asset UTxOs in unbonded pool
  logInfo'
    "userWithdrawUnbondedPoolContract: Getting unbonded assets in \
    \the pool..."
  unbondedAssetUtxos <- getUnbondedAssetUtxos unbondedPoolUtxos
  logInfo_ "userWithdrawnUnbondedPoolContract: Bonded Asset UTxOs"
    unbondedAssetUtxos
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Unbonded nftCs
  -- Get the staking range to use
  logInfo' "userWithdrawUnbondedPoolContract: Getting user range..."
  { currTime, range } <- getBondingTime params
  logInfo_ "userWithdrawUnbondedPoolContract: Current time: " $ show currTime
  logInfo_ "userWithdrawUnbondedPoolContract: TX Range" range
  -- Get the token name for the user by hashing
  assocListTn <-
    liftContractM
      "userWithdrawUnbondedPoolContract: Could not create token name for user`"
      $ mkTokenName hashedUserPkh
  -- Get user entry utxo
  entryInput /\ entryOutput <-
    liftContractM "userWithdrawUnbondedPoolContract: Cannot get assocList utxo"
      $ getUtxoWithNFT unbondedPoolUtxos assocListCs assocListTn
  userEntry <- unwrap <$> getEntryDatumFromOutput entryOutput
  logInfo_ "userWithdrawUnbondedPoolContract: entry to consume" userEntry
  -- Build useful values for later
  let
    mintEntryValue = singleton assocListCs assocListTn one
    burnEntryValue = singleton assocListCs assocListTn (-one)
    assetParams = unwrap unbondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    assetDatum = Datum $ toData AssetDatum
    assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos

    -- Get amount to withdraw
    rewards :: Rational
    rewards = userEntry.rewards

    rewardsRounded :: BigInt
    rewardsRounded = numerator rewards / denominator rewards

    withdrawnAmt :: BigInt
    withdrawnAmt = userEntry.deposited + rewardsRounded

    withdrawnVal :: Value
    withdrawnVal = singleton assetCs assetTn withdrawnAmt

  logInfo_ "userWithdrawUnbondedPoolContract: rewards" rewards
  logInfo_ "userWithdrawUnbondedPoolContract: rewardsRounded" rewardsRounded
  logInfo_ "userWithdrawUnbondedPoolContract: withdrawnAmt" withdrawnAmt
  logInfo_ "userWithdrawUnbondedPoolContract: withdrawnVal" withdrawnVal
  logInfo_ "userWithdrawUnbondedPoolContract: rewards" rewards

  -- Calculate assets to consume and change that needs to be returned
  -- to the pool
  consumedAssetUtxos /\ withdrawChange <-
    liftContractM
      "userWithdrawUnbondedPoolContract: Cannot get asset \
      \UTxOs to consume" $
      getAssetsToConsume unbondedAssetClass withdrawnAmt
        unbondedAssetUtxos
  logInfo_ "userWithdrawUnbondedPoolContract: withdrawChange"
    withdrawChange
  logInfo_ "userWithdrawUnbondedPoolContract: consumedAssetUtxos"
    consumedAssetUtxos
  -- Build base constraints and lookups
  let
    changeValue :: Value
    changeValue =
      singleton
        (unwrap unbondedAssetClass).currencySymbol
        (unwrap unbondedAssetClass).tokenName
        withdrawChange

    baseConstraints :: TxConstraints Unit Unit
    baseConstraints =
      if withdrawChange > fromInt 0 then
        mconcat
          [ mustBeSignedBy userPkh
          , mustPayToScript valHash assetDatum changeValue
          , mustPayToPubKeyAddress userPkh userStakingPubKeyHash
              withdrawnVal
          , mustValidateIn range
          ]
      else
        mconcat
          [ mustBeSignedBy userPkh
          , mustPayToPubKeyAddress userPkh userStakingPubKeyHash
              withdrawnVal
          , mustValidateIn range
          ]

    baseLookups :: ScriptLookups.ScriptLookups PlutusData
    baseLookups = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.mintingPolicy listPolicy
      , ScriptLookups.unspentOutputs $ unwrap userUtxos
      , ScriptLookups.unspentOutputs $ unwrap unbondedPoolUtxos
      ]
  -- Deterimne if we are doing a closed or open withdrawal
  constraints /\ lookups <-
    if userEntry.open then do
      -- Get state utxo
      tokenName <- liftContractM
        "userWithdrawUnbondedPoolContract: Cannot create TokenName"
        unbondedStakingTokenName
      poolTxInput /\ poolTxOutput <-
        liftContractM "userWithdrawUnbondedPoolContract: Cannot get state utxo"
          $ getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
      logInfo'
        "userWithdrawUnbondedPoolContract: Getting head entry of the pool..."
      headEntry /\ poolOpenState <- getStateDatumFromOutput poolTxOutput
      logInfo_ "userWithdrawUnbondedPoolContract: Head entry of the pool"
        headEntry
      ---- BUILD CONSTRAINTS AND LOOKUPS ----
      constraints /\ lookups <- case headEntry of
        Nothing -> throwContractError
          "userWithdrawUnbondedPoolContract: no entries \
          \in the pool, expected at least one"
        Just headKey -> do
          logInfo'
            "userWithdrawUnbondedPoolContract: Found the head entry successfully"
          case compare hashedUserPkh headKey of
            -- If hashedUserPkh < headKey, we are trying to withdraw a non-existent
            --  entry
            LT -> throwContractError
              "userWithdrawUnbondedPoolContract: entry key < \
              \head key (non existent)"
            -- If hashedUserPkh == key, we are trying to withdraw the first entry of
            --  the list
            EQ -> do
              logInfo' "userWithdrawUnbondedPoolContract: Compare EQ"
              -- Get the datum of the head entry and the key of the new head
              logInfo'
                "userWithdrawUnbondedPoolContract: getting datum of entry to\
                \consume (head)..."
              oldHeadEntry <- unwrap <$> getEntryDatumFromOutput entryOutput
              logInfo_ "userWithdrawUnbondedPoolContract: entry to consume"
                oldHeadEntry
              let
                newHeadKey :: Maybe ByteArray
                newHeadKey = oldHeadEntry.next

                newState :: Datum
                newState = Datum <<< toData $
                  StateDatum
                    { maybeEntryName: newHeadKey
                    , open: poolOpenState
                    }
                -- Build validator redeemer
                valRedeemer = Redeemer <<< toData $
                  WithdrawAct
                    { stakeHolder: userPkh
                    , burningAction: BurnHead poolTxInput entryInput
                    }
                -- Build minting policy redeemer
                mintRedeemer = Redeemer $ toData $ ListRemove $ BurnHead
                  poolTxInput
                  entryInput
              -- New state lookup
              stateDatumLookup <-
                liftContractM
                  "userWithdrawUnbondedPoolContract: Could not create state datum \
                  \lookup"
                  $ ScriptLookups.datum newState
              let
                stateTokenValue = singleton nftCs tokenName one

                constraints :: TxConstraints Unit Unit
                constraints =
                  mconcat
                    [ mustSpendScriptOutput poolTxInput valRedeemer
                    , mustSpendScriptOutput entryInput valRedeemer
                    , mkAssetUtxosConstraints consumedAssetUtxos valRedeemer
                    , mustMintValueWithRedeemer mintRedeemer burnEntryValue
                    , mustPayToScript valHash newState stateTokenValue
                    ]
              pure $ constraints /\ stateDatumLookup
            -- If hashedUserPkh > key, we find the wanted entry in the list and
            --  withdraw its respective funds
            GT -> do
              -- The hashed key is greater than so we must look at the assoc. list
              -- in more detail
              logInfo' "userWithdrawUnbondedPoolContract: Compare GT"
              { firstInput, secondInput }
                /\ { firstOutput, secondOutput }
                /\ _ <-
                liftContractM
                  "userWithdrawUnbondedPoolContract: Cannot get position in Assoc. \
                  \List"
                  $ findRemoveOtherElem assocList hashedUserPkh

              -- Get the entry datum of the previous entry
              logInfo'
                "userWithdrawUnbondedPoolContract: getting datum of previous\
                \entry..."
              prevEntry <- unwrap <$> getEntryDatumFromOutput firstOutput
              logInfo_ "userWithdrawUnbondedPoolContract: entry to consume"
                prevEntry

              -- Get the entry datum of the entry to consume
              logInfo'
                "userWithdrawUnbondedPoolContract: getting datum of entry to\
                \ burn..."
              burnEntry <- unwrap <$> getEntryDatumFromOutput secondOutput
              logInfo_ "userWithdrawUnbondedPoolContract: entry to consume"
                burnEntry

              let
                -- Build updated previous entry and its lookup
                prevEntryUpdated = Datum $ toData $ EntryDatum
                  { entry: Entry $ prevEntry
                      { next = burnEntry.next
                      }
                  }
              prevEntryDatumLookup <-
                liftContractM
                  "userWithdrawUnbondedPoolContract: Could not create updated prev \
                  \ entry datum lookup"
                  $ ScriptLookups.datum prevEntryUpdated

              -- Build validator redeemer
              let
                valRedeemer = Redeemer <<< toData $
                  WithdrawAct
                    { stakeHolder: userPkh
                    , burningAction: BurnOther firstInput secondInput
                    }
                -- Build minting policy redeemer
                mintRedeemer = Redeemer $ toData $ ListRemove $ BurnOther
                  firstInput
                  secondInput

                constraints :: TxConstraints Unit Unit
                constraints =
                  mconcat
                    [ mustSpendScriptOutput firstInput valRedeemer
                    , mustSpendScriptOutput secondInput valRedeemer
                    , mkAssetUtxosConstraints consumedAssetUtxos valRedeemer
                    , mustMintValueWithRedeemer mintRedeemer burnEntryValue
                    , mustPayToScript valHash prevEntryUpdated mintEntryValue
                    ]
              pure $ constraints /\ prevEntryDatumLookup
      pure $ constraints /\ lookups
    else do
      logInfo' "userWithdrawUnbondedPoolContract: Pool closed withdrawal"
      let
        -- Build validator redeemer
        valRedeemer = Redeemer <<< toData $
          WithdrawAct
            { stakeHolder: userPkh
            , burningAction: BurnSingle entryInput
            }
        -- Build minting policy redeemer
        mintRedeemer = Redeemer $ toData $ ListRemove $ BurnSingle entryInput

        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustSpendScriptOutput entryInput valRedeemer
            , mkAssetUtxosConstraints consumedAssetUtxos valRedeemer
            , mustMintValueWithRedeemer mintRedeemer burnEntryValue
            ]
      pure $ constraints /\ mempty
  -- Balance transaction
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx
      (baseLookups <> lookups)
      (baseConstraints <> constraints)
  logInfo_
    "userWithdrawUnbondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  signedTx <-
    liftedM
      "userWithdrawUnbondedPoolContract: Cannot balance, reindex redeemers, \
      \ attach datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTx
  logInfo_
    "userWithdrawUnbondedPoolContract: Transaction successfully submitted with \
    \hash"
    $ byteArrayToHex
    $ unwrap transactionHash

-- | This function filters all the asset UTxOs from a `UtxoM`
getUnbondedAssetUtxos :: forall (r :: Row Type). UtxoM -> Contract r UtxoM
getUnbondedAssetUtxos utxos = do
  assetUtxos <- catMaybes <$> for utxoAssocList \utxo@(_ /\ txOutput) -> do
    datumHash <- liftContractM "getAssetUtxos: could not get datum hash"
      (unwrap txOutput).dataHash
    datum <-
      liftContractM "getAssetUtxos: could not get datum"
        =<< getDatumByHash datumHash
    unbondedDatum :: UnbondedStakingDatum <-
      liftContractM
        "getAssetUtxos: could not parse datum as a bonded staking \
        \datum" $ fromData (unwrap datum)
    case unbondedDatum of
      AssetDatum -> pure $ Just utxo
      _ -> pure Nothing
  pure <<< UtxoM $ Map.fromFoldable assetUtxos
  where
  utxoAssocList :: Array (TransactionInput /\ TransactionOutput)
  utxoAssocList = Map.toUnfoldable $ unwrap utxos

-- | Get entry datum from transaction output
getEntryDatumFromOutput
  :: forall (r :: Row Type). TransactionOutput -> Contract r Entry
getEntryDatumFromOutput txOut = do
  unbondedDatum <- getUnbondedDatum txOut
  case unbondedDatum of
    EntryDatum { entry: e } -> pure e
    _ -> throwContractError
      "getEntryDatumFromOutput: datum is not of Entry \
      \type"

-- | Get state datum from transaction output
getStateDatumFromOutput
  :: forall (r :: Row Type)
   . TransactionOutput
  -> Contract r (Tuple (Maybe ByteArray) Boolean)
getStateDatumFromOutput txOut = do
  unbondedDatum <- getUnbondedDatum txOut
  case unbondedDatum of
    StateDatum { maybeEntryName: key, open: isOpen } -> pure $ (key /\ isOpen)
    _ -> throwContractError
      "getStateDatumFromOutput: datum is not of State \
      \type"

-- | Gets an unbonded datum from a transaction output
getUnbondedDatum
  :: forall (r :: Row Type)
   . TransactionOutput
  -> Contract r UnbondedStakingDatum
getUnbondedDatum =
  liftContractM
    "getUnbondedDatum: could not parse datum as unbonded staking datum"
    <<< fromData
    <<< unwrap
    <=< liftContractM "getUnbondedDatum: could not get datum"
    <=< getDatumByHash
    <=< liftContractM "getUnbondedDatum: could not get datum hash"
    <<< _.dataHash
    <<< unwrap
