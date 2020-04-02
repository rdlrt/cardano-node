module Test.Cardano.Api.Gen
  ( genAddress
  , genKeyPair
  , genNetwork
  , genPubKeyInfo
  ) where

import           Cardano.Api
import           Cardano.Prelude

import           Test.Cardano.Crypto.Gen (genProtocolMagicId, genSigningKey, genVerificationKey)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen


genAddress :: Gen Address
genAddress =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, byronPubKeyAddress <$> (PubKeyInfoByron <$> genNetwork <*> genVerificationKey))
    , (1, pure AddressShelley)
    ]

genKeyPair :: Gen KeyPair
genKeyPair =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, KeyPairByron <$> genVerificationKey <*> genSigningKey)
    , (1, pure KeyPairShelley)
    ]

genNetwork :: Gen Network
genNetwork =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genProtocolMagicId
    ]

genPubKeyInfo :: Gen PubKeyInfo
genPubKeyInfo =
  -- When Shelly is sorted out, this should change to `Gen.choose`.
  Gen.frequency
    [ (9, PubKeyInfoByron <$> genNetwork <*> genVerificationKey)
    , (1, pure PubKeyInfoShelley)
    ]
