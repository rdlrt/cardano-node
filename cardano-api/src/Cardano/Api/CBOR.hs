module Cardano.Api.CBOR
  ( addressFromCBOR
  , addressToCBOR
  , keyPairFromCBOR
  , keyPairToCBOR
  , pubKeyInfoToCBOR
  , pubKeyInfoFromCBOR
  ) where

import           Cardano.Api.Error
import           Cardano.Api.Types

import           Cardano.Binary (DecoderError (..), Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import qualified Cardano.Binary as CBOR

import           Cardano.Prelude

import           Data.ByteString.Char8 (ByteString)
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Word (Word8)


addressFromCBOR :: ByteString -> Either ApiError Address
addressFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "Address" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s Address
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        0  -> AddressByron <$> fromCBOR
        1  -> pure AddressShelley
        _  -> cborError $ DecoderErrorUnknownTag "Address" tag

addressToCBOR :: Address -> ByteString
addressToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      AddressByron ba -> mconcat [ toCBOR (0 :: Word8), toCBOR ba ]
      AddressShelley -> toCBOR (1 :: Word8)

keyPairFromCBOR :: ByteString -> Either ApiError KeyPair
keyPairFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "KeyPair" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s KeyPair
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        0  -> KeyPairByron <$> fromCBOR <*> fromCBOR
        1  -> pure KeyPairShelley
        _  -> cborError $ DecoderErrorUnknownTag "KeyPair" tag

keyPairToCBOR :: KeyPair -> ByteString
keyPairToCBOR kp =
  CBOR.serializeEncoding' $
    case kp of
      KeyPairByron vk sk -> mconcat [ toCBOR (0 :: Word8), toCBOR vk, toCBOR sk ]
      KeyPairShelley -> toCBOR (1 :: Word8)

pubKeyInfoFromCBOR :: ByteString -> Either ApiError PubKeyInfo
pubKeyInfoFromCBOR bs =
   first ApiErrorCBOR . CBOR.decodeFullDecoder "PubKeyInfo" decode $ LBS.fromStrict bs
  where
    decode :: Decoder s PubKeyInfo
    decode = do
      tag <- CBOR.decodeWord8
      case tag of
        2  -> PubKeyInfoByron <$> networkFromCBOR <*> fromCBOR
        3  -> pure PubKeyInfoShelley
        _  -> cborError $ DecoderErrorUnknownTag "PubKeyInfo" tag

pubKeyInfoToCBOR :: PubKeyInfo -> ByteString
pubKeyInfoToCBOR pki =
  CBOR.serializeEncoding' $
    case pki of
      PubKeyInfoByron nw vk -> mconcat [ toCBOR (2 :: Word8), networkToCBOR nw, toCBOR vk ]
      PubKeyInfoShelley -> toCBOR (3 :: Word8)

-- -------------------------------------------------------------------------------------------------

networkFromCBOR :: Decoder s Network
networkFromCBOR = do
  tag <- CBOR.decodeWord8
  case tag of
    3  -> pure Mainnet
    4  -> Testnet <$> fromCBOR
    _  -> cborError $ DecoderErrorUnknownTag "Network" tag

networkToCBOR :: Network -> Encoding
networkToCBOR nw =
  case nw of
    Mainnet -> mconcat [toCBOR (3 :: Word8)]
    Testnet pid -> mconcat [toCBOR (4 :: Word8), toCBOR pid]

