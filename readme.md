# Clash-WaveDrom

Generate wave diagrams from Clash with [WaveDrom](https://wavedrom.com/).

```haskell
data Gray n = Gray
  { cnt  :: Unsigned n
  , gray :: BitVector n
  }
  deriving stock Generic
  deriving anyclass (NFData, ToWave)

readmeSignal :: Signal System (Gray 3)
readmeSignal =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ let cnt  = register 0 (countSucc <$> cnt)
          gray = (\x -> (x `shiftR` 1) `xor` x) . bitCoerce <$> cnt
      in  Gray <$> cnt <*> gray
```

![Gray code wave diagram](./images/gray.svg)
