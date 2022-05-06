module CreatePool (createPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData (PlutusData, Datum(Datum), toData)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValue
  , mustPayToScript
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (getCurrencySymbol, scriptCurrencySymbol, singleton)
import Data.Argonaut (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (head)
import Data.Map (toUnfoldable)
import Scripts.BondedListNFT (mkBondedListNFTPolicy)
import Scripts.BondedPoolValidator (mkBondedPoolValidator)
import Scripts.BondedStateNFT (mkBondedStateNFTPolicy)
import Serialization.Address (addressBech32)
import Settings (bondedStakingTokenName, hardCodedParams)
import Types (BondedStakingDatum(StateDatum))
import Utils (logInfo_)

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createPoolContract :: Contract () Unit
createPoolContract = do
  networkId <- getNetworkId
  adminPkh <- liftedM "createPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "Admin PaymentPubKeyHash" adminPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "createPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "createPoolContract: Cannot get user Utxos" $ utxosAt adminAddr
  logInfo_ "Admin's UTXOs" adminUtxos
  txOutRef <- liftContractM "createPoolContract: Could not get head UTXO"
    $ fst
    <$> (head $ toUnfoldable $ unwrap adminUtxos)
  logInfo_ "`toData` utxo" $ toData txOutRef
  logInfo_ "Admin's head UTXO" adminUtxos
  -- Get the minting policy and currency symbol from the state NFT:
  statePolicy <- liftedE $ mkBondedStateNFTPolicy txOutRef
  logInfo_ "State NFT (applied)" statePolicy
  nftCs <-
    liftedM "createPoolContract: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol statePolicy
  logInfo_ "State NFT Currency Symbol" nftCs
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkBondedListNFTPolicy nftCs
  logInfo_ "List NFT" listPolicy
  assocListCs <-
    liftedM "createPoolContract: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol listPolicy
  logInfo_ "List NFT CurrencySymbol" assocListCs
  -- May want to hardcode this somewhere:
  tokenName <- liftContractM "createPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  -- We define the parameters of the pool
  params <- liftContractM "createPoolContract: Failed to create parameters" $
    hardCodedParams adminPkh nftCs assocListCs
  logInfo_ "`toData` Pool Parameters" $ toData params
  -- Get the bonding validator and hash
  validator <- liftedE' "createPoolContract: Cannot create validator" $
    mkBondedPoolValidator params
  logInfo_ "Bonded Pool Validator" validator
  valHash <- liftedM "createPoolContract: Cannot hash validator"
    (validatorHash validator)
  logInfo_ "Bonded Pool Validator's hash" valHash
  let
    mintValue = singleton nftCs tokenName one
    scriptAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "BondedPool Validator's address" scriptAddr
  let
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      }

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.mintingPolicy statePolicy
      , ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs (unwrap adminUtxos)
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToScript valHash bondedStateDatum mintValue
        , mustMintValue mintValue
        , mustSpendPubKeyOutput txOutRef
        ]

  unattachedBalancedTx <-
    liftedE (ScriptLookups.mkUnbalancedTx lookup constraints)
  -- `balanceAndSignTx` does the following:
  -- 1) Balance a transaction
  -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
  -- 3) Attach datums and redeemers to transaction.
  -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "createPoolContract: Cannot balance, reindex redeemers, attach datums/\
      \redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_ "createPoolContract: Transaction successfully submitted with hash"
    transactionHash