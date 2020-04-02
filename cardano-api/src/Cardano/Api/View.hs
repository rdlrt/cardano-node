module Cardano.Api.View
  ( parseAddressView
  , parseKeyPairView
  , parsePubKeyInfoView
  , readKeyPair
  , readPubKeyInfo
  , renderAddressView
  , renderKeyPairView
  , renderPubKeyInfoView
  , writeKeyPair
  , writePubKeyInfo

  -- Exported for testing.
  , rawToMultilineHex
  , unRawToMultilineHex
  ) where

import           Cardano.Api.CBOR
import           Cardano.Api.Types
import           Cardano.Api.Error

import           Cardano.Prelude

import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except.Extra (handleIOExceptT, hoistEither, runExceptT)

import qualified Data.Text as Text


parseAddressView :: ByteString -> Either ApiError Address
parseAddressView bs =
    case BS.lines bs of
      ("AddressByron" : rest) -> parseLines rest
      ("AddressShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parseAddressView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parseAddressView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError Address
    parseLines xs =
      addressFromCBOR =<< unRawToMultilineHex (BS.concat xs)

parseKeyPairView :: ByteString -> Either ApiError KeyPair
parseKeyPairView bs =
    case BS.lines bs of
      ("KeyPairByron" : rest) -> parseLines rest
      ("KeyPairShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parseKeyPairView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parseKeyPairView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError KeyPair
    parseLines xs =
      keyPairFromCBOR =<< unRawToMultilineHex (BS.concat xs)

parsePubKeyInfoView :: ByteString -> Either ApiError PubKeyInfo
parsePubKeyInfoView bs =
    case BS.lines bs of
      ("PubKeyInfoByron" : rest) -> parseLines rest
      ("PubKeyInfoShelley" : rest) -> parseLines rest
      [] -> Left $ ApiError "parsePubKeyInfoView: Empty set of lines."
      (m : _) -> Left $ ApiError (mconcat ["parsePubKeyInfoView: Bad marker ", textShow m, "."])
  where
    parseLines :: [ByteString] -> Either ApiError PubKeyInfo
    parseLines xs =
      pubKeyInfoFromCBOR =<< unRawToMultilineHex (BS.concat xs)

renderAddressView :: Address -> ByteString
renderAddressView kp =
  BS.unlines $
    case kp of
      AddressByron {} -> "AddressByron" : xs
      AddressShelley {} -> "AddressShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ addressToCBOR kp

renderKeyPairView :: KeyPair -> ByteString
renderKeyPairView kp =
  BS.unlines $
    case kp of
      KeyPairByron {} -> "KeyPairByron" : xs
      KeyPairShelley {} -> "KeyPairShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ keyPairToCBOR kp

renderPubKeyInfoView :: PubKeyInfo -> ByteString
renderPubKeyInfoView kp =
  BS.unlines $
    case kp of
      PubKeyInfoByron {} -> "PubKeyInfoByron" : xs
      PubKeyInfoShelley {} -> "PubKeyInfoShelley" : xs
  where
    xs :: [ByteString]
    xs = rawToMultilineHex $ pubKeyInfoToCBOR kp

-- -------------------------------------------------------------------------------------------------

readKeyPair :: FilePath -> IO (Either ApiError KeyPair)
readKeyPair path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parseKeyPairView bs

readPubKeyInfo :: FilePath -> IO (Either ApiError PubKeyInfo)
readPubKeyInfo path =
  runExceptT $ do
    bs <- handleIOExceptT (ApiErrorIO path) $ BS.readFile path
    hoistEither $ parsePubKeyInfoView bs

writeKeyPair :: FilePath -> KeyPair -> IO (Either ApiError ())
writeKeyPair path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderKeyPairView kp)

writePubKeyInfo :: FilePath -> PubKeyInfo -> IO (Either ApiError ())
writePubKeyInfo path kp =
  runExceptT .
    handleIOExceptT (ApiErrorIO path) $ BS.writeFile path (renderPubKeyInfoView kp)

-- -------------------------------------------------------------------------------------------------

-- | Convert a raw ByteString to hexadecimal and then line wrap
rawToMultilineHex :: ByteString -> [ByteString]
rawToMultilineHex = chunksOf . Base16.encode

-- | Convert from multiline hexadecimal to a raw ByteString.
unRawToMultilineHex :: ByteString -> Either ApiError ByteString
unRawToMultilineHex bs =
  case Base16.decode (BS.concat $ BS.lines bs) of
    (raw, "") -> Right raw
    (_, err) -> Left $ ApiError ("unRawToMultilineHex: Unable to decode " <> textShow err)

chunksOf :: ByteString -> [ByteString]
chunksOf bs =
  case BS.splitAt 80 bs of
    (leading, "") -> [leading]
    (leading, trailing) -> leading : chunksOf trailing


textShow :: Show a => a -> Text
textShow = Text.pack . show
