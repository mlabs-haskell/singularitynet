module Utils
  ( big
  , bigIntRange
  , currentRoundedTime
  , currentTime
  , countdownTo
  , findInsertUpdateElem
  , findRemoveOtherElem
  , getAssetsToConsume
  , getUtxoWithNFT
  , hashPkh
  , jsonReader
  , logInfo_
  , mkAssetUtxosConstraints
  , mkBondedPoolParams
  , mkOnchainAssocList
  , mkRatUnsafe
  , nat
  , roundDown
  , roundUp
  , splitByLength
  , submitTransaction
  , toIntUnsafe
  , repeatUntilConfirmed
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (PaymentPubKeyHash)
import Contract.Hashing (blake2b256Hash)
import Contract.Monad
  ( Contract
  , liftedE
  , liftedM
  , logInfo
  , logInfo'
  , logAesonInfo
  , tag
  )
import Contract.Numeric.Natural (Natural, fromBigInt', toBigInt)
import Contract.Numeric.Rational (Rational, numerator, denominator)
import Contract.Prim.ByteArray (ByteArray, hexToByteArray)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (PlutusScript)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , BalancedSignedTransaction
  , balanceAndSignTx
  , submit
  , awaitTxConfirmedWithTimeout
  )
import Contract.TxConstraints (TxConstraints, mustSpendScriptOutput)
import Contract.Utxos (UtxoM(UtxoM))
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , flattenNonAdaAssets
  , getTokenName
  , valueOf
  )
import Control.Alternative (guard)
import Control.Monad.Error.Class
  ( class MonadThrow
  , try
  , liftMaybe
  )
import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField) as Json
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Array
  ( filter
  , head
  , last
  , length
  , partition
  , mapMaybe
  , slice
  , sortBy
  , (..)
  )

import Data.Array as Array
import Data.BigInt (BigInt, fromInt, fromNumber, quot, rem, toInt, toNumber)
import Data.DateTime.Instant (unInstant)
import Data.Map (Map, toUnfoldable)
import Data.Map as Map
import Data.Time.Duration (Seconds, Milliseconds(Milliseconds))
import Data.Unfoldable (unfoldr)
import Effect.Aff (delay)
import Effect.Now (now)
import Effect.Exception (Error, error, throw)
import Effect.Class (class MonadEffect)
import Math (ceil)
import Serialization.Hash (ed25519KeyHashToBytes)
import Types
  ( AssetClass(AssetClass)
  , BondedPoolParams(BondedPoolParams)
  , InitialBondedParams(InitialBondedParams)
  , MintingAction(MintEnd, MintInBetween)
  )
import Types.Interval (POSIXTime(POSIXTime))
import Types.PlutusData (PlutusData)
import Types.Redeemer (Redeemer)

-- | Helper to decode the local inputs such as unapplied minting policy and
-- typed validator
jsonReader
  :: String
  -> Json
  -> Either JsonDecodeError PlutusScript
jsonReader field = do
  caseJsonObject (Left $ TypeMismatch "Expected Object") $ \o -> do
    hex <- Json.getField o field
    case hexToByteArray hex of
      Nothing -> Left $ TypeMismatch "Could not convert to bytes"
      Just bytes -> pure $ wrap bytes

-- | Get the UTXO with the NFT defined by its `CurrencySymbol` and `TokenName`.
-- If more than one UTXO contains the NFT, something is seriously wrong.
getUtxoWithNFT
  :: UtxoM
  -> CurrencySymbol
  -> TokenName
  -> Maybe (Tuple TransactionInput TransactionOutput)
getUtxoWithNFT utxoM cs tn =
  let
    utxos = filter hasNFT $ toUnfoldable $ unwrap utxoM
  in
    if length utxos > 1 then Nothing
    else head utxos
  where
  hasNFT
    :: Tuple TransactionInput TransactionOutput
    -> Boolean
  hasNFT (Tuple _ txOutput') =
    let
      txOutput = unwrap txOutput'
    in
      valueOf txOutput.amount cs tn == one

-- | This receives a `UtxoM` with all the asset UTxOs of the pool and the desired
-- | amount to withdraw. It returns a subset of these that sums at least
-- | the given amount and the total amount
getAssetsToConsume :: AssetClass -> BigInt -> UtxoM -> Maybe (UtxoM /\ BigInt)
getAssetsToConsume (AssetClass ac) withdrawAmt assetUtxos =
  go assetList Map.empty zero
  where
  assetList :: Array (TransactionInput /\ TransactionOutput)
  assetList = Map.toUnfoldable <<< unwrap $ assetUtxos

  go
    :: Array (TransactionInput /\ TransactionOutput)
    -> Map TransactionInput TransactionOutput
    -> BigInt
    -> Maybe (UtxoM /\ BigInt)
  go arr toConsume sum
    | sum >= withdrawAmt = Just $ UtxoM toConsume /\ (sum - withdrawAmt)
    | null arr = Nothing
    | otherwise = do
        input /\ output <- Array.head arr
        arr' <- Array.tail arr
        let
          assetCount = valueOf (unwrap output).amount ac.currencySymbol
            ac.tokenName
          toConsume' = Map.insert input output toConsume
          sum' = sum + assetCount
        go arr' toConsume' sum'

-- | Builds constraints for asset UTxOs
mkAssetUtxosConstraints :: UtxoM -> Redeemer -> TxConstraints Unit Unit
mkAssetUtxosConstraints utxos redeemer =
  foldMap (\(input /\ _) -> mustSpendScriptOutput input redeemer)
    ( Map.toUnfoldable $ unwrap utxos
        :: Array (TransactionInput /\ TransactionOutput)
    )

-- | Convert from `Int` to `Natural`
nat :: Int -> Natural
nat = fromBigInt' <<< fromInt

-- | Convert from `Int` to `BigInt`
big :: Int -> BigInt
big = fromInt

roundUp :: Rational -> BigInt
roundUp r =
  let
    n = numerator r
    d = denominator r
  in
    if d == one then n
    else quot (n + d - (rem n d)) d

roundDown :: Rational -> BigInt
roundDown r =
  let
    n = numerator r
    d = denominator r
  in
    quot (n - (rem n d)) d

-- | Converts a `Maybe Rational` to a `Rational` when using the (%) constructor
mkRatUnsafe :: Maybe Rational -> Rational
mkRatUnsafe Nothing = zero
mkRatUnsafe (Just r) = r

-- | Converts from a contract 'Natural' to an 'Int'
toIntUnsafe :: Natural -> Int
toIntUnsafe = fromMaybe 0 <<< toInt <<< toBigInt

logInfo_
  :: forall (r :: Row Type) (a :: Type)
   . Show a
  => String
  -> a
  -> Contract r Unit
logInfo_ k = flip logInfo mempty <<< tag k <<< show

-- Creates the `BondedPoolParams` from the `InitialBondedParams` and runtime
-- parameters from the user.
mkBondedPoolParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> InitialBondedParams
  -> BondedPoolParams
mkBondedPoolParams admin nftCs assocListCs (InitialBondedParams ibp) = do
  BondedPoolParams
    { iterations: ibp.iterations
    , start: ibp.start
    , end: ibp.end
    , userLength: ibp.userLength
    , bondingLength: ibp.bondingLength
    , interest: ibp.interest
    , minStake: ibp.minStake
    , maxStake: ibp.maxStake
    , admin
    , bondedAssetClass: ibp.bondedAssetClass
    , nftCs
    , assocListCs
    }

hashPkh :: PaymentPubKeyHash -> Aff ByteArray
hashPkh =
  blake2b256Hash <<< unwrap <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

-- | Makes an on chain assoc list returning the key, input and output. We could
-- | be more stringent on checks to ensure the list is genuinely connected
-- | although on chain code should enforce this.
mkOnchainAssocList
  :: CurrencySymbol
  -> UtxoM
  -> Array (ByteArray /\ TransactionInput /\ TransactionOutput)
mkOnchainAssocList assocListCs (UtxoM utxos) =
  sortBy compareBytes $ mapMaybe getAssocListUtxos $ toUnfoldable utxos
  where
  getAssocListUtxos
    :: TransactionInput /\ TransactionOutput
    -> Maybe (ByteArray /\ TransactionInput /\ TransactionOutput)
  getAssocListUtxos utxo@(_ /\ (TransactionOutput txOutput)) = do
    let val = flattenNonAdaAssets txOutput.amount
    cs /\ tn /\ amt <- head val
    guard (length val == one && cs == assocListCs && amt == one)
    pure $ getTokenName tn /\ utxo

compareBytes
  :: forall (t :: Type). ByteArray /\ t -> ByteArray /\ t -> Ordering
compareBytes (bytes /\ _) (bytes' /\ _) = compare bytes bytes'

-- | Find the assoc list element to update or insert. This can be optimised
-- | if we compare pairs and exit early of course. But we'll do this for
-- | simplicity. THIS MUST BE USED ON A SORTED LIST, i.e. with
-- | `mkOnchainAssocList`. We should probably create a type for the output.
findInsertUpdateElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> ByteArray
  -> Maybe
       ( Maybe MintingAction
           /\
             { firstInput :: TransactionInput
             , secondInput :: Maybe TransactionInput
             }
           /\
             { firstOutput :: TransactionOutput
             , secondOutput :: Maybe TransactionOutput
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: Maybe ByteArray
             }
       )
findInsertUpdateElem assocList hashedKey = do
  -- The list should findAssocElem assocList hashedKey = do be sorted so no
  -- need to resort
  let { no, yes } = partition (fst >>> (>=) hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  -- If we're at the last element, it must be an end stake or updating last
  -- element
  if length no == zero then do
    -- Workout whether it's an initial deposit
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintEnd txInputL
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Nothing }
      /\ { firstOutput: txOutputL, secondOutput: Nothing }
      /\ { firstKey: bytesL, secondKey: Nothing }
  -- Otherwise, it is an inbetween stake or updating the first element
  else do
    bytesH /\ txInputH /\ txOutputH <- head no
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintInBetween txInputL txInputH
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Just txInputH }
      /\ { firstOutput: txOutputL, secondOutput: Just txOutputH }
      /\ { firstKey: bytesL, secondKey: Just bytesH }

-- | Find the element to remove from the list. This only works for the
-- | in-between case, since it assumes that some entry will have a key less
-- | than the given one.
findRemoveOtherElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> ByteArray
  -> Maybe
       ( { firstInput :: TransactionInput
         , secondInput :: TransactionInput
         }
           /\
             { firstOutput :: TransactionOutput
             , secondOutput :: TransactionOutput
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: ByteArray
             }
       )
findRemoveOtherElem assocList hashedKey = do
  let { no, yes } = partition (fst >>> (<) hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  bytesH /\ txInputH /\ txOutputH <- head no
  if bytesH /= hashedKey
  -- If the first element not less than `hashedKey` is not equal, then the
  -- entry has not been found
  then Nothing
  -- Otherwise, this is the entry to remove and the last element of the
  -- entries less than `hashedKey` is the previous entry
  else Just
    $ { firstInput: txInputL, secondInput: txInputH }
    /\ { firstOutput: txOutputL, secondOutput: txOutputH }
    /\ { firstKey: bytesL, secondKey: bytesH }

-- Produce a range from zero to the given bigInt (inclusive)
bigIntRange :: BigInt -> Array BigInt
bigIntRange lim =
  unfoldr
    ( \acc ->
        if acc >= lim then Nothing
        else Just $ acc /\ (acc + one)
    )
    zero

-- Get time rounded to the closest integer (ceiling) in seconds
currentRoundedTime
  :: forall m. Monad m => MonadEffect m => MonadThrow Error m => m POSIXTime
currentRoundedTime = do
  POSIXTime t <- currentTime
  t' <-
    liftMaybe (error "currentRoundedTime: could not convert Number to BigInt")
      $ fromNumber
      $ ceil (toNumber t / 1000.0)
      * 1000.0
  pure $ POSIXTime t'

-- Get UNIX epoch from system time
currentTime
  :: forall m. Monad m => MonadEffect m => MonadThrow Error m => m POSIXTime
currentTime = do
  Milliseconds t <- unInstant <$> liftEffect now
  t' <- liftMaybe (error "currentPOSIXTime: could not convert Number to BigInt")
    $
      fromNumber t
  pure $ POSIXTime t'

countdownTo :: forall (r :: Row Type). POSIXTime -> Contract r Unit
countdownTo tf = do
  -- Get current time
  t <- currentRoundedTime
  let
    msg :: String
    msg = "Countdown: " <> (showSeconds $ unwrap tf - unwrap t)
  if t > tf then log "0"
  else logInfo' msg *> wait *> countdownTo tf
  where
  wait :: Contract r Unit
  wait = liftAff $ delay $ Milliseconds 10000.0

  showSeconds :: BigInt -> String
  showSeconds n = show $ toNumber n / 1000.0

-- | Utility function for splitting an array into equal length sub-arrays
-- | (with remainder array length <= size)
splitByLength :: forall (a :: Type). Int -> Array a -> Array (Array a)
splitByLength size array
  | size == 0 || null array = []
  | otherwise =
      let
        sublistCount =
          if (length array) `mod` size == 0 then ((length array) `div` size) - 1
          else (length array) `div` size
      in
        map (\i -> slice (i * size) ((i * size) + size) array) $
          0 .. sublistCount

-- | Submits a transaction with the given list of constraints/lookups
submitTransaction
  :: TxConstraints Unit Unit
  -> ScriptLookups.ScriptLookups PlutusData
  -> Array
       ( Tuple
           (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
  -> Seconds
  -> Int
  -> Contract ()
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
submitTransaction baseConstraints baseLookups updateList timeout maxAttempts =
  do
    let
      constraintList = fst <$> updateList
      lookupList = snd <$> updateList
      constraints = baseConstraints <> mconcat constraintList
      lookups = baseLookups <> mconcat lookupList
    result <- try $ repeatUntilConfirmed timeout maxAttempts do
      -- Build transaction
      unattachedBalancedTx <-
        liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
      logAesonInfo unattachedBalancedTx
      signedTx <-
        liftedM
          "submitTransaction: Cannot balance, reindex redeemers, /\
          \attach datums redeemers and sign"
          $ balanceAndSignTx unattachedBalancedTx
      pure { signedTx }
    case result of
      Left e -> do
        logInfo_ "submitTransaction:" e
        pure updateList
      Right _ ->
        pure []

-- | This function executes a `Contract` that returns a `BalancedSignedTransaction`,
-- | submits it, and waits `timeout` seconds for it to succeed. If it does not,
-- | the function repeats the contract execution and TX submission until it
-- | does or `maxTrials` attempts are completed.
repeatUntilConfirmed
  :: forall (r :: Row Type) (p :: Row Type)
   . Seconds
  -> Int
  -> Contract r { signedTx :: BalancedSignedTransaction | p }
  -> Contract r { signedTx :: BalancedSignedTransaction | p }
repeatUntilConfirmed timeout maxTrials contract = do
  result@{ signedTx } <- contract
  logInfo' "repeatUntilConfirmed: transaction built successfully"
  txHash <- submit signedTx
  logInfo'
    "repeatUntilConfirmed: transaction submitted. Waiting for confirmation"
  confirmation <- try $ awaitTxConfirmedWithTimeout timeout txHash
  case confirmation of
    Left _ -> do
      logInfo'
        "repeatUntilConfirmed: timeout reached, the transaction was not confirmed"
      if maxTrials == 0 then do
        logInfo'
          "repeatUntilConfirmed: no more trials remaining, throwing exception..."
        liftEffect $ throw "Failed to submit transaction"
      else do
        logInfo' $ "repeatUntilConfirmed: running transaction again, "
          <> show maxTrials
          <> " trials remaining"
        repeatUntilConfirmed timeout (maxTrials - 1) contract
    Right _ -> do
      logInfo' "repeatUntilConfirmed: transaction confirmed!"
      logInfo_ "TX Hash" txHash
      pure result
