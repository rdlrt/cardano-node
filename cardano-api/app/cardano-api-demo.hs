
import           Cardano.Api

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  keyPair <- genByronKeyPair
  BS.putStrLn $ renderKeyPairView keyPair

  -- Could also be 'Testnet x'.
  let pubKey = mkPubKeyInfo keyPair Mainnet
  BS.putStrLn $ renderPubKeyInfoView pubKey

  let addr = byronPubKeyAddress pubKey
  BS.putStrLn $ renderAddressView addr
