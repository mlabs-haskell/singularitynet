module DepositPool (depositBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( AddressWithNetworkTag(AddressWithNetworkTag)
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE'
  , liftedM
  , logInfo'
  , throwContractError
  )
import Contract.Numeric.Rational (Rational, (%))
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, singleton)
import Control.Applicative (unless)
import Data.BigInt (BigInt)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types
  ( BondedPoolParams(BondedPoolParams)
  , BondedStakingAction(AdminAct)
  , BondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  , Entry(Entry)
  )
import Types.Natural (Natural)
import Types.Redeemer (Redeemer(Redeemer))
import Types.Scripts (ValidatorHash)
import Utils
  ( getUtxoWithNFT
  , logInfo_
  , mkOnchainAssocList
  , mkRatUnsafe
  , roundUp
  , splitByLength
  , submitTransaction
  , toIntUnsafe
  )

-- Deposits a certain amount in the pool
depositBondedPoolContract
  :: BondedPoolParams
  -> Natural
  -> Array
       ( Tuple (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
  -> ( Array
         ( Tuple (TxConstraints Unit Unit)
             (ScriptLookups.ScriptLookups PlutusData)
         )
       -> Contract () Unit
     )
  -> Contract ()
       ( Array
           ( Tuple (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
depositBondedPoolContract
  params@
    ( BondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  batchSize
  depositList
  batchFinishCallback = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositBondedPoolContract: Admin is not current user"
  logInfo_ "depositBondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: adminAddr } <-
    liftedM "depositBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositBondedPoolContract: Cannot get user Utxos"
      $ utxosAt adminAddr
  -- Get the bonded pool validator and hash
  validator <- liftedE' "depositBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  valHash <- liftContractM "depositBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "depositBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "depositBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "depositBondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "depositBondedPoolContract: Pool UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "depositBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "depositBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "depositBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "depositBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "depositBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "depositBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <-
    liftContractM
      "depositBondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)

  -- Update the association list
  case bondedStakingDatum of
    -- Non-empty user list
    StateDatum { maybeEntryName: Just _ } -> do
      logInfo'
        "depositBondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ... }"
      let
        assocList = mkOnchainAssocList assocListCs bondedPoolUtxos

        -- Concatenate constraints/lookups
        constraints :: TxConstraints Unit Unit
        constraints = mustBeSignedBy admin

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs (unwrap adminUtxos)
            <> ScriptLookups.unspentOutputs (unwrap bondedPoolUtxos)

      -- Use depositList as updateList if not null, otherwise update stakes
      -- of all users
      updateList <-
        if not (null depositList) then pure depositList
        else traverse (mkEntryUpdateList params valHash) assocList
      -- Submit transaction with possible batching
      failedDeposits <-
        if batchSize == zero then do
          failedDeposits' <- submitTransaction constraints lookups updateList
          batchFinishCallback failedDeposits'
          pure failedDeposits'
        else
          let
            updateBatches = splitByLength (toIntUnsafe batchSize) updateList
          in
            mconcat <$> for updateBatches \txBatch -> do
              failedDeposits' <- submitTransaction constraints lookups txBatch
              batchFinishCallback failedDeposits'
              pure failedDeposits'
      logInfo_
        "depositBondedPoolContract: Finished updating pool entries. /\
        \Entries with failed updates"
        failedDeposits
      pure failedDeposits
    -- Other error cases:
    StateDatum { maybeEntryName: Nothing } ->
      throwContractError
        "depositBondedPoolContract: There are no users in the pool to deposit \
        \rewards for"
    _ ->
      throwContractError "depositBondedPoolContract: Datum incorrect type"

-- | Creates a constraint and lookups list for updating each user entry
mkEntryUpdateList
  :: BondedPoolParams
  -> ValidatorHash
  -> (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> Contract ()
       ( Tuple (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
mkEntryUpdateList
  (BondedPoolParams { interest, bondedAssetClass, assocListCs })
  valHash
  (_ /\ txIn /\ txOut) = do
  -- Get the Entry datum of the old assoc. list element
  dHash <- liftContractM
    "mkEntryUpdateList: Could not get Entry Datum Hash"
    (unwrap txOut).dataHash
  logInfo_ "mkEntryUpdateList: datum hash" dHash
  listDatum <-
    liftedM
      "mkEntryUpdateList: Cannot get Entry's datum" $ getDatumByHash dHash
  bondedListDatum :: BondedStakingDatum <-
    liftContractM
      "mkEntryUpdateList: Cannot extract NFT State datum"
      $ fromData (unwrap listDatum)
  -- The get the entry datum
  case bondedListDatum of
    EntryDatum { entry } -> do
      let e = unwrap entry
      calculatedRewards <-
        calculateRewards
          interest
          e.deposited
      assocListTn <-
        liftContractM
          "mkEntryUpdateList: Could not create token name for user"
          $ mkTokenName e.key
      -- Update the entry datum
      let
        recentRewards = roundUp calculatedRewards
        updatedDeposited = e.deposited + recentRewards
        newRewards = e.rewards + mkRatUnsafe (recentRewards % one)
        -- Datum and redeemer creation
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry $ e
              { newDeposit = zero -- reset to zero
              , deposited = updatedDeposited
              , staked = updatedDeposited -- redundant
              , rewards = newRewards
              }
          }
        valRedeemer = Redeemer $ toData AdminAct
        -- Build asset datum and value types
        assetDatum = Datum $ toData AssetDatum
        assetParams = unwrap bondedAssetClass
        assetCs = assetParams.currencySymbol
        assetTn = assetParams.tokenName
        depositValue = singleton assetCs assetTn recentRewards
        entryValue = singleton assocListCs assocListTn one

        -- Build constraints and lookups
        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustPayToScript valHash assetDatum depositValue
            , mustPayToScript valHash entryDatum entryValue
            , mustSpendScriptOutput txIn valRedeemer
            ]
      entryDatumLookup <-
        liftContractM
          "mkEntryUpdateList: Could not create state datum lookup"
          $ ScriptLookups.datum entryDatum
      pure (constraints /\ entryDatumLookup)
    _ -> throwContractError
      "mkEntryUpdateList: Datum not Entry constructor"

-- | Calculates user rewards
calculateRewards
  :: Rational -- interest
  -> BigInt -- deposited
  -> Contract () Rational
calculateRewards interest deposited = do
  when (deposited == zero) $
    throwContractError "calculateRewards: totalDeposited is zero"
  let
    recentRewards = interest * mkRatUnsafe (deposited % one)
  when (recentRewards < zero) $ throwContractError
    "calculateRewards: invalid rewards amount"
  pure recentRewards
