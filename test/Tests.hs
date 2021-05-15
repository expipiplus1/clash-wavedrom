{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver    #-}

module Tests where

import           Clash.Class.Counter
import qualified Clash.Explicit.Prelude        as Clash
import           Clash.Prelude
import           Clash.WaveDrom
import           Control.DeepSeq
import           Control.Exception              ( evaluate )
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Data.Bits
import qualified Data.ByteString.Lazy          as LBS
import           Data.Text                      ( Text )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced     ( goldenTest )

test_wavedrom :: [TestTree]
test_wavedrom =
  [ signalTest "simple"   (render $ wavedrom 4 "out" simple)
  , signalTest "bits"     (render $ wavedrom 10 "out" counterBits)
  , signalTest "count"    (render $ wavedrom 10 "out" counter)
  , signalTest "bitswave" (render $ wavedrom 10 "out" (BitsWave <$> counter))
  , signalTest "withbits"
               (render $ wavedrom 10 "out" (WithBits <$> counterBits))
  , signalTest "show"        (render $ wavedrom 4 "out" (ShowWave <$> simple))
  , signalTest "text"        (render $ wavedrom 4 "out" text)
  , signalTest "nested"      (render $ wavedrom 2 "" nested)
  , signalTest "nestedNamed" (render $ wavedrom 2 "out" nested)
  , signalTest "partialVec"  (render $ wavedrom 2 "out" partialVec)
  , signalTest "partialSignalShow"
               (render $ wavedrom 8 "out" (ShowWave <$> partialSignal))
  , signalTest "partialSignalBits"
               (render $ wavedrom 8 "out" (BitsWave <$> partialSignal))
  , signalTest "partialSignalWithBits"
               (render $ wavedrom 8 "out" (WithBits <$> partialSignal))
  , signalTest "undefinedBitShow"
               (render $ wavedrom 4 "out" (ShowWave <$> undefinedBit))
  , signalTest "undefinedBit"     (render $ wavedrom 4 "out" undefinedBit)
  , signalTest "undefinedBits"    (render $ wavedrom 4 "out" undefinedBits)
  , signalTest "resetNested"      (render $ wavedromWithReset 4 "" nested)
  , signalTest "resetNestedNamed" (render $ wavedromWithReset 4 "out" nested)
  , signalTest "resetCount"       (render $ wavedromWithReset 10 "" counter)
  , signalTest "resetCountNamed"  (render $ wavedromWithReset 10 "cnt" counter)
  , signalTest
    "either"
    (render
      ((wavedromWithClock 10 "either" (WithBits <$> eitherSignal))
        { config = object ["hscale" .= (2 :: Int)]
        }
      )
    )
  , signalTest "readme" (render $ wavedromWithClock 10 "" readmeSignal)
  ]

simple :: Signal System Bit
simple =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ let x = register low (complement <$> x) in x

counter :: Signal System (Unsigned 3)
counter =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ let x = register 0 (satSucc SatWrap <$> x) in x

counterBits :: Signal System (BitVector 3)
counterBits = bitCoerce <$> counter

text :: Signal System Text
text = fromList_lazy ["hello", "world", "foo", "bar"]

nested :: Signal System Foo
nested = withClockResetEnable
  systemClockGen
  systemResetGen
  enableGen
  (pure (Foo (errorX "vec") (Bar "hi" (J $ 1 :> 1 :> Clash.Nil))) :: Signal
      System
      Foo
  )

partialVec :: Signal System (Vec 5 Bit)
partialVec = pure (0 :> 1 :> errorX "foo" :> 0 :> 1 :> Clash.Nil)

undefinedBit :: Signal System (BitVector 5)
undefinedBit = fromList_lazy
  [0, 1, v2bv (0 :> 1 :> errorX "foo" :> 0 :> 1 :> Clash.Nil), 4, 5]

undefinedBits :: Signal System (BitVector 5)
undefinedBits = fromList_lazy
  [0, 1, v2bv (0 :> errorX "bar" :> errorX "foo" :> 0 :> 1 :> Clash.Nil), 4, 5]

partialSignal :: Signal System (Unsigned 4)
partialSignal = withClockResetEnable
  systemClockGen
  systemResetGen
  enableGen
  (let c = register 0 ((\x -> if x >= 3 then errorX "hi" else succ x) <$> c)
   in  c :: Signal System (Unsigned 4)
  )

eitherSignal :: Signal System (Either (Index 8) (Index 3))
eitherSignal =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ let x = register (Right 0) (countSucc <$> x) in x

data Foo = Foo
  { foo :: Vec 5 Bit
  , bar :: Bar
  }
  deriving Generic
  deriving anyclass (NFData, ToWave)

data Bar = Bar
  { baz :: Text
  , qux :: J 2
  }
  deriving Generic
  deriving anyclass (NFData, ToWave)

newtype J n = J
  { unJ :: Vec n Bit
  }
  deriving (Generic)
  deriving anyclass (NFData, ToWave)

----------------------------------------------------------------
-- Readme
----------------------------------------------------------------

data Gray n = Gray
  { count :: Unsigned n
  , gray  :: BitVector n
  }
  deriving stock Generic
  deriving anyclass (NFData, ToWave)

readmeSignal :: Signal System (Gray 3)
readmeSignal =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ let count = register 0 (countSucc <$> count)
          gray  = (\x -> (x `shiftR` 1) `xor` x) . bitCoerce <$> count
      in  Gray <$> count <*> gray

----------------------------------------------------------------
--
----------------------------------------------------------------

signalTest :: TestName -> LBS.ByteString -> TestTree
signalTest name =
  goldenVsStringDiff name
                     (\ref new -> ["diff", "-u", ref, new])
                     ("test/data" </> name <.> "json")
    . pure
