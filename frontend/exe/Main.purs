module Main (main) where

import Contract.Prelude

import BondedStaking.TimeUtils (startPoolFromNow)
import ClosePool (closeBondedPoolContract)
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad
  ( ContractConfig
  , ConfigParams(ConfigParams)
  , LogLevel(Info)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , launchAff_
  , liftContractM
  , logInfo'
  , mkContractConfig
  , runContract
  , runContract_
  )
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createBondedPoolContract)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int as Int
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Effect.Exception (error)
import Settings (testInitBondedParams)
import Types (BondedPoolParams(..))
import Types.Interval (POSIXTime(..))
import Types.Natural as Natural
import UserStake (userStakeBondedPoolContract)
import UserWithdraw (userWithdrawBondedPoolContract)
import Utils (currentRoundedTime, logInfo_)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
-- import UnbondedStaking.Types
--   ( InitialUnbondedParams(InitialUnbondedParams)
--   , UnbondedPoolParams(..)
--   )
-- import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
-- import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)
-- import Utils (currentRoundedTime)
-- import Types.Interval (POSIXTime(POSIXTime))

-- main :: Effect Unit
-- main = launchAff_ $ do
--   cfg <- mkConfig
--   runContract_ cfg $ do
--     initParams <- liftContractM "main: Cannot initiate bonded parameters"
--       testInitBondedParams
--     bondedParams <- createBondedPoolContract initParams
--     -- sleep in order to wait for tx
--     liftAff $ delay $ wrap $ toNumber 80_000
--     depositBondedPoolContract bondedParams
--     liftAff $ delay $ wrap $ toNumber 80_000
--     closeBondedPoolContract bondedParams

-- Bonded: admin create pool, user stake, admin deposit (rewards), admin close
-- using PureScript (non SDK)
main :: Effect Unit
main = launchAff_ do
  adminCfg <- mkConfig
  ---- Admin creates pool ----
  bondedParams@(BondedPoolParams bpp) <-
    runContract adminCfg do
      logInfo' "STARTING AS ADMIN"
      initParams <- liftContractM "main: Cannot initiate bonded parameters"
        testInitBondedParams
      -- We get the current time and set up the pool to start 80 seconds from now
      let
        startDelayInt :: Int
        startDelayInt = 80_000
      startDelay <- liftContractM "main: Cannot create startDelay from Int"
        $ Natural.fromBigInt
        $ BigInt.fromInt startDelayInt
      initParams' /\ currTime <- startPoolFromNow startDelay initParams
      logInfo_ "Pool creation time" currTime
      bondedParams <- createBondedPoolContract initParams'
      logInfo_ "Pool parameters" bondedParams
      logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
      -- We give 30 seconds of margin for the users and admin to sign the transactions
      liftAff $ delay $ wrap $ Int.toNumber $ startDelayInt + 50_000
      pure bondedParams

  ---- User 1 deposits ----
  userCfg <- mkConfig
  userStake <- liftM (error "main: Cannot create userStake from String") $
    Natural.fromString "4000000"
  runContract_ userCfg do
    userStakeBondedPoolContract bondedParams userStake
    logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
    -- Wait until bonding period
    liftAff $ delay $ wrap $ BigInt.toNumber bpp.userLength
  ---- Admin deposits to pool ----
  runContract_ adminCfg do
    depositBatchSize <-
      liftM (error "Cannot create Natural") $ Natural.fromString "1"
    void $
      depositBondedPoolContract
        bondedParams
        depositBatchSize
        []
        (BigInt.fromInt 100_000)
    logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
    liftAff $ delay $ wrap $ BigInt.toNumber $ bpp.bondingLength -
      BigInt.fromInt 80_000
  ---- User 1 withdraws ----
  runContract_ userCfg do
    userWithdrawBondedPoolContract bondedParams
    logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
    -- Wait until transaction is processed
    liftAff $ delay $ wrap $ Int.toNumber 120_000
  -- Admin closes pool
  runContract_ adminCfg do
    closeBatchSize <-
      liftM (error "Cannot create Natural") $ Natural.fromString "10"
    -- Wait until pool closing time
    POSIXTime currTime <- currentRoundedTime
    logInfo_ "currTime" currTime
    let
      deltaClose :: BigInt
      deltaClose = bpp.end - currTime + BigInt.fromInt 2000
    logInfo_ "deltaClose" deltaClose
    void $ closeBondedPoolContract bondedParams closeBatchSize [] deltaClose
    logInfo' "main: Pool closed"

-- Unbonded: admin create pool, user stake, admin deposit (rewards),
-- user withdraw, admin close using PureScript (non SDK)
-- main :: Effect Unit
-- main = launchAff_ do
--   adminCfg <- mkConfig
--   ---- Admin creates pool ----
--   unbondedParams@(UnbondedPoolParams upp) <-
--     runContract adminCfg do
--       logInfo' "STARTING AS ADMIN"
--       initParams <- liftContractM "main: Cannot initiate bonded parameters"
--         testInitUnbondedParams
--       -- We get the current time and set up the pool to start 80 seconds from now
--       POSIXTime currTime <- currentRoundedTime
--       let iup = unwrap initParams
--           poolDelay = 80_000
--           iupWithTime :: InitialUnbondedParams
--           iupWithTime = InitialUnbondedParams $ iup {
--             start = currTime + BigInt.fromInt poolDelay
--           }
--       logInfo_ "Pool creation time" currTime
--       unbondedParams <- createUnbondedPoolContract iupWithTime
--       logInfo_ "Pool parameters" unbondedParams
--       logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
--       -- We give 30 seconds of margin for the users and admin to sign the transactions
--       liftAff $ delay $ wrap $ Int.toNumber $ poolDelay + 30_000
--       pure unbondedParams
--   ---- User 1 deposits ----
--   userCfg <- mkConfig
--   userStake <- liftM (error "main: Cannot create userStake from String") $
--     Natural.fromString "4000"
--   runContract_ userCfg do
--     userStakeUnbondedPoolContract unbondedParams userStake
--     logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--     -- Wait until bonding period
--     liftAff $ delay $ wrap $ BigInt.toNumber upp.userLength
--   -- ---- User 2 deposits ----
--   -- userCfg <- mkConfig
--   -- userStake <- liftM (error "main: Cannot create userStake from String") $
--   --   Natural.fromString "8000000"
--   -- runContract_ userCfg do
--   --   userStakeUnbondedPoolContract unbondedParams userStake
--   --   logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--   --   -- Wait until bonding period
--   --   liftAff $ delay $ wrap $ BigInt.toNumber upp.userLength
--   ---- Admin deposits to pool ----
--   runContract_ adminCfg do
--     depositBatchSize <-
--       liftM (error "Cannot create Natural") $ Natural.fromString "1"
--     void $ depositUnbondedPoolContract unbondedParams depositBatchSize []
--     -- failedDeposits <- depositUnbondedPoolContract unbondedParams depositBatchSize []
--     -- void $ depositUnbondedPoolContract unbondedParams depositBatchSize failedDeposits
--     -- closeBatchSize <-
--     --   liftM (error "Cannot create Natural") $ Natural.fromString "10"
--     -- void $ closeUnbondedPoolContract unbondedParams closeBatchSize []

--     logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
--     -- Wait until withdrawing period
--     liftAff $ delay $ wrap $ BigInt.toNumber upp.adminLength
--   ---- User 1 withdraws ----
--   runContract_ userCfg do
--     userWithdrawUnbondedPoolContract unbondedParams
--     logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--     -- Wait until closing period
--     logInfo' "Waiting until admin period to close pool..."
--     liftAff $ delay $ wrap $ BigInt.toNumber upp.userLength
--   -- Admin closes pool
--   runContract_ adminCfg do
--     closeBatchSize <-
--       liftM (error "Cannot create Natural") $ Natural.fromString "10"
--     void $ closeUnbondedPoolContract unbondedParams closeBatchSize []
--     logInfo' "END"
--   -- ---- User 2 withdraws ----
--   -- runContract_ userCfg do
--   --   userWithdrawUnbondedPoolContract unbondedParams

-- Bonded: admin create pool, user stake, admin deposit (rewards), admin close
-- using PureScript (SDK)
-- Run *one* at a time:
-- main :: Effect Unit
-- main =
-- bondedCallContractCreatePoolExample1
-- After running the above contract, update `testBondedPoolArgs` accordingly.
-- bondedCallContractUserStakeExample1
-- bondedCallContractAdminDepositExample1
-- bondedCallContractAdminCloseExample1

mkConfig :: Aff (ContractConfig ())
mkConfig = do
  wallet <- Just <$> mkNamiWalletAff
  mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Info
    , extraConfig: {}
    , wallet
    }