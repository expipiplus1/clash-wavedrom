module Clash.WaveDrom
  ( render
  , renderToSVG
  , wavedrom
  , wavedromWithClock
  , wavedromWithReset
  , ToWave(..)
  , ShowWave(..)
  , BitsWave(..)
  , WithBits(..)
  , WaveDrom(..)
  ) where

import           Clash.WaveDrom.Internal
import           Data.Aeson                     ( encode
                                                , toJSON
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           System.IO                      ( hClose )
import           System.IO.Temp                 ( withSystemTempFile
                                                , withTempFile
                                                )
import           System.Process.Typed

render :: WaveDrom -> ByteString
render = encodePretty

-- | Render a 'Wavedrom' to SVG. This requires the 'wavedrom-cli' tool
renderToSVG :: FilePath -> WaveDrom -> IO ()
renderToSVG fp wd = withSystemTempFile "wavedrom.json" $ \json jsonHandle -> do
  BS.hPutStr jsonHandle (encode (toJSON wd))
  hClose jsonHandle
  runProcess_ $ proc "wavedrom-cli" ["--input", json, "--svg", fp]

