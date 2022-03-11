{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transactions in the context of a consensus mode, and other types used in
-- the transaction submission protocol.
--
module Cardano.Api.InMode (

    -- * Transaction in a consensus mode
    TxInMode(..),
   -- fromConsensusGenTx,
    toConsensusGenTx,

    -- * Transaction input in a consensus mode
    TxIdInMode(..),
    toConsensusTxId,

    -- * Transaction validation errors
    TxValidationError(..),
    TxValidationErrorInMode(..),
    fromConsensusApplyTxErr,
  ) where

import           Prelude

import           Data.Proxy
import           Data.SOP.Strict (NS (S, Z))
import           Data.Text (Text)

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Byron
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Embed.Unary as Consensus

import           Cardano.Api.Eras
import           Cardano.Api.Modes
import           Cardano.Api.Tx
import           Cardano.Api.TxBody


-- ----------------------------------------------------------------------------
-- Transactions in the context of a consensus mode
--

-- | A 'Tx' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxSubmission protocol.
--
data TxInMode mode where

     -- | Everything we consider a normal transaction.
     --
     TxInMode :: Tx era -> EraInMode era mode -> TxInMode mode

     -- | Byron has various things we can post to the chain which are not
     -- actually transactions. This covers: update proposals, votes and
     -- delegation certs.
     --
     TxInByronSpecial :: Consensus.GenTx Consensus.ByronBlock
                      -> EraInMode ByronEra mode -> TxInMode mode

deriving instance Show (TxInMode mode)

{-
fromConsensusGenTx
  :: forall mode hfblock block era. ConsensusBlockForMode mode ~ hfblock
  => ConsensusEraForBlock hfblock ~ era
  => ConsensusBlockForEraMode era ~ block
  => ConsensusMode mode -> Consensus.GenTx block -> TxInMode mode
fromConsensusGenTx ByronMode tx'=
  TxInByronSpecial tx' ByronEraInByronMode
fromConsensusGenTx ShelleyMode tx' =
  let Consensus.ShelleyTx _txid shelleyEraTx = tx'
  in TxInMode (ShelleyTx ShelleyBasedEraShelley shelleyEraTx) ShelleyEraInShelleyMode
fromConsensusGenTx CardanoMode tx' =
  case tx' of
    (Consensus.ShelleyTx _txid t@Alonzo.ValidatedTx{}) -> TxInMode (ShelleyTx ShelleyBasedEraAlonzo t) AlonzoEraInCardanoMode
    _ -> error ""

    -}

--fromConsensusGenTx
--  :: forall mode hfblock era block. Consensus.SingleEraBlock block
--  => ConsensusBlockForMode mode ~ hfblock
--  => ConsensusEraForBlock hfblock ~ era
--  => ConsensusBlockForEraMode era mode ~ block
--  => ConsensusMode mode -> Consensus.GenTx block -> TxInMode mode
--fromConsensusGenTx ByronMode (tx@Consensus.ByronTx{})=
--  TxInByronSpecial tx ByronEraInByronMode
--fromConsensusGenTx ShelleyMode (Consensus.ShelleyTx _txid tx) =
--  TxInMode (ShelleyTx ShelleyBasedEraShelley tx) ShelleyEraInShelleyMode
--fromConsensusGenTx CardanoMode tx =
--  case tx of
--    Consensus.ShelleyTx _txid t@Alonzo.ValidatedTx{} -> TxInMode (ShelleyTx ShelleyBasedEraAlonzo t) AlonzoEraInCardanoMode

toConsensusGenTx :: ConsensusBlockForMode mode ~ block
                 => TxInMode mode
                 -> Consensus.GenTx block
toConsensusGenTx (TxInMode (ByronTx tx) ByronEraInByronMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx

toConsensusGenTx (TxInMode (ByronTx tx) ByronEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx
    --TODO: add the above as mkByronTx to the consensus code,
    -- matching mkShelleyTx below

toConsensusGenTx (TxInByronSpecial gtx ByronEraInByronMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInByronSpecial gtx ByronEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInMode (ShelleyTx _ tx) ShelleyEraInShelleyMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) ShelleyEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx')))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) AllegraEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx'))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) MaryEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx')))))
  where
    tx' = Consensus.mkShelleyTx tx

toConsensusGenTx (TxInMode (ShelleyTx _ tx) AlonzoEraInCardanoMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx'))))))
  where
    tx' = Consensus.mkShelleyTx tx

-- ----------------------------------------------------------------------------
-- Transaction ids in the context of a consensus mode
--

-- | A 'TxId' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxMonitoring protocol.
--


type family ForgetHFC blkHFC where
  ForgetHFC (Consensus.HardForkBlock '[Consensus.ByronBlock]) = '[Consensus.ByronBlock]
  ForgetHFC (Consensus.HardForkBlock '[Consensus.ShelleyBlock era]) = '[Consensus.ShelleyBlock era]
  ForgetHFC (Consensus.HardForkBlock  (Consensus.CardanoEras c)) = Consensus.CardanoEras c

--  ConsensusBlockForMode ShelleyMode = Consensus.ShelleyBlockHFC StandardShelley
--  ConsensusBlockForMode CardanoMode = Consensus.CardanoBlock StandardCrypto
data TxIdInMode mode where
  TxIdInMode :: TxId -> EraInMode era mode -> TxIdInMode mode

-- TODO: Write a single  type family that can return hardfork blocks in cardanomode
-- and normal blocks in single eras? Essentially you need to take into consideration
-- the mode and the block
toConsensusTxId
  :: ConsensusBlockForMode mode ~ hfblock
  => ConsensusEraForBlock hfblock ~ era
  => ConsensusBlockForEraMode era mode ~ block
  => EraInMode era mode ->  TxIdInMode mode -> Consensus.TxId (Consensus.GenTx block)
toConsensusTxId _ (TxIdInMode txid ByronEraInByronMode) =
   Consensus.ByronTxId $ toByronTxId txid
toConsensusTxId _ (TxIdInMode txid ShelleyEraInShelleyMode) =
   Consensus.ShelleyTxId $ toShelleyTxId txid
toConsensusTxId ByronEraInCardanoMode (TxIdInMode txid ByronEraInCardanoMode) =
  Consensus.ByronTxId $ toByronTxId txid
toConsensusTxId _ _ = error ""

--toConsensusTxId (TxIdInMode txid ShelleyEraInShelleyMode) =
--  Consensus.ShelleyTxId $ toShelleyTxId txid
--toConsensusTxId (TxIdInMode txid ByronEraInCardanoMode) =  error ""
--toConsensusTxId (TxIdInMode txid ShelleyEraInCardanoMode) =  error ""
--toConsensusTxId (TxIdInMode txid AllegraEraInCardanoMode) =  error ""
--toConsensusTxId (TxIdInMode txid MaryEraInCardanoMode) =  error ""
--toConsensusTxId (TxIdInMode txid AlonzoEraInCardanoMode) =  error ""
  -- Consensus.ShelleyTxId $ error "toShelleyTxId txid"
 --where
 -- obtainEraConstraint
 --   :: ConsensusBlockForEraMode mode ~ hfblock
 --   => ForgetHFC hfblock ~ block
 --   => ConsensusBlockForEraMode era ~ block
 --   => CardanoEra era
 --   -> EraInMode mode era
 --   -> ( (ConsensusBlockForEraMode mode ~ hfblock
 --        , ForgetHFC hfblock ~ block
 --        , ConsensusBlockForEraMode era ~ block
 --        ) => a) -> a
 -- obtainEraConstraint ByronEra _  f = f
 -- obtainEraConstraint ShelleyEra _  f = f
 -- obtainEraConstraint AllegraEra _  f = f
 -- obtainEraConstraint MaryEra _  f = f
 -- obtainEraConstraint AlonzoEra _  f = f
-- ----------------------------------------------------------------------------
-- Transaction validation errors in the context of eras and consensus modes
--

-- | The transaction validations errors that can occur from trying to submit a
-- transaction to a local node. The errors are specific to an era.
--
data TxValidationError era where

     ByronTxValidationError
       :: Consensus.ApplyTxErr Consensus.ByronBlock
       -> TxValidationError ByronEra

     ShelleyTxValidationError
       :: ShelleyBasedEra era
       -> Consensus.ApplyTxErr (Consensus.ShelleyBlock (ShelleyLedgerEra era))
       -> TxValidationError era

-- The GADT in the ShelleyTxValidationError case requires a custom instance
instance Show (TxValidationError era) where
    showsPrec p (ByronTxValidationError err) =
      showParen (p >= 11)
        ( showString "ByronTxValidationError "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraShelley err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraShelley "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAllegra err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAllegra "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraMary err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraMary "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAlonzo err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAlonzo "
        . showsPrec 11 err
        )


-- | A 'TxValidationError' in one of the eras supported by a given protocol
-- mode.
--
-- This is used in the LocalStateQuery protocol.
--
data TxValidationErrorInMode mode where
     TxValidationErrorInMode :: TxValidationError era
                             -> EraInMode era mode
                             -> TxValidationErrorInMode mode

     TxValidationEraMismatch :: EraMismatch
                             -> TxValidationErrorInMode mode

deriving instance Show (TxValidationErrorInMode mode)


fromConsensusApplyTxErr :: ConsensusBlockForMode mode ~ block
                        => ConsensusMode mode
                        -> Consensus.ApplyTxErr block
                        -> TxValidationErrorInMode mode
fromConsensusApplyTxErr ByronMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInByronMode

fromConsensusApplyTxErr ShelleyMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInShelleyMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrByron err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrShelley err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrAllegra err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAllegra err)
      AllegraEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrMary err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraMary err)
      MaryEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrAlonzo err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAlonzo err)
      AlonzoEraInCardanoMode

fromConsensusApplyTxErr CardanoMode (Consensus.ApplyTxErrWrongEra err) =
    TxValidationEraMismatch err

