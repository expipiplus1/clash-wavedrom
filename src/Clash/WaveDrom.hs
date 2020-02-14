{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds #-}
{-# language PolyKinds #-}
{-# language AllowAmbiguousTypes #-}
{-# language QuantifiedConstraints #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language InstanceSigs #-}
{-# language TypeFamilyDependencies #-}
{-# language PartialTypeSignatures #-}
{-# language StandaloneDeriving #-}
{-# language RecordWildCards #-}
{-# language DefaultSignatures #-}
{-# language TupleSections #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language DerivingVia #-}
{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module WaveDrom
  where

import           Data.Monoid                              ( Ap(..) )
import           Data.Coerce
import           Unsafe.Coerce
import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Singletons.Prelude.Enum
import qualified Data.Singletons.TypeLits
import           Data.Singletons.TypeLits                 ( SSymbol(SSym)
                                                          , SSymbol
                                                          )
import           Data.Singletons.Prelude.List             ( SList
                                                          , SList(SNil, SCons)
                                                          , sFoldr
                                                          , Map
                                                          , sMap
                                                          , (%++)
                                                          , type (++)
                                                          )
import           Data.Singletons.Prelude.Show
import           Data.Singletons.Prelude.Num              ( type (-@#@$)
                                                          , type (+@#@$)
                                                          , SNum(..)
                                                          )
import           Data.Singletons.TH                hiding ( type (<=) )
import qualified Clash.Prelude                 as Clash
import           Clash.Prelude                     hiding ( Cons
                                                          , Nil
                                                          , fromList
                                                          , foldr1
                                                          , undefined
                                                          , (++)
                                                          , replicate
                                                          , SSymbol
                                                          )
import           Data.Aeson.Encode.Pretty
import           Data.Vector                              ( fromList )
import           Data.Functor
import           Data.List
import           Debug.Trace
import           Data.Maybe                               ( catMaybes )
import qualified Data.Text                     as T
import           Data.Type.Equality
import           GHC.Generics
import           Data.Text                         hiding ( group
                                                          , foldr1
                                                          , null
                                                          , replicate
                                                          )
import           Data.Aeson
import           Data.Aeson.TH                            ( deriveJSON
                                                          , defaultOptions
                                                          , Options
                                                            ( fieldLabelModifier
                                                            )
                                                          )
import           Data.Char                                ( toLower )
import           Control.DeepSeq
import           GHC.TypeLits
import           Data.Proxy
import           Data.Kind

----------------------------------------------------------------
-- The shape of WaveDrom waves
----------------------------------------------------------------

$(singletons [d|
  data Tree a
    = Leaf a
    | Branch [Tree a] a
  |])

type T = Tree Symbol
type L = Leaf
type B ts = Branch ts

----------------------------------------------------------------
-- A hierarchy of waves, indexed by @Tree@
----------------------------------------------------------------

data Instant = Instant { char :: Char, data' :: Maybe Text }
  deriving (Eq, Show)

data Waves (t :: T) (a :: Type) where
  Single :: KnownSymbol n => a -> Waves (L n) a
  Nil :: KnownSymbol n => Waves (B '[] n) a
  Cons :: Waves t a -> Waves (B ts n) a -> Waves (B (t ': ts) n) a
infixr 5 `Cons`

deriving instance Functor (Waves t)

instance (SingI t, Show a) => Show (Waves t a) where
  show = \case
    Single a -> "Single " <> show a
    Nil -> "Nil"
    Cons w ws -> withSing @t $ \case
      SBranch (SCons t ts) SSym -> withSingI t (show w) <> " `Cons` " <> withSingI ts (show ws)

instance SingI t => Applicative (Waves t) where
  pure x = withSing @t $ \case
    SLeaf SSym        -> Single x
    SBranch SNil SSym -> Nil
    SBranch (SCons t ts) SSym ->
      Cons (withSingI t (pure x)) (withSingI ts (pure x))
  f <*> x = f <**> x
    where
      (<**>) :: Waves t' (a -> b) -> Waves t' a -> Waves t' b
      Single f  <**> Single x  = Single (f x)
      Nil       <**> Nil       = Nil
      Cons f fs <**> Cons x xs = Cons (f <**> x) (fs <**> xs)

instance Semigroup a => Semigroup (Waves t a) where
  Single xs <> Single ys = Single (xs <> ys)
  Nil <> Nil = Nil
  (x `Cons` xs) <> (y `Cons` ys) = (x <> y) `Cons` (xs <> ys)

-- | Render the @Waves@ to WaveDrom JSON
instance ToJSON (Waves t [Instant]) where
  toJSON = \case
    w@(Single is) ->
      object
        $  ["name" .= waveName w, "wave" .= (char <$> is), "data" .= catMaybes (data' <$> is)]
    w@Nil        -> Array (fromList [String (waveName w)])
    w@(Cons _ _) -> Array (fromList (String (waveName w) : values w))
    where
      values :: Waves (B ts n) [Instant] -> [Value]
      values = \case
        Nil       -> []
        Cons t ts -> toJSON t : values ts

-- | The name of the top level wave
waveName :: Waves t a -> Text
waveName = \case
  a@(Single _) -> (\(_ :: Waves (L n) _) -> demote @n) a
  a@Nil        -> (\(_ :: Waves (B _ n) _) -> demote @n) a
  Cons _ ts    -> waveName ts

-- | Replace repeated characters with '.', This is important to get good
-- rendering from the JavaScript renderer.
simplify :: Waves t [Instant] -> Waves t [Instant]
simplify = \case
  Single is -> Single ((\(x:xs) -> x : (Instant '.' Nothing <$ xs)) =<< group is)
  Nil -> Nil
  Cons t ts -> Cons (simplify t) (simplify ts)

----------------------------------------------------------------
-- Things which can be made into waves
----------------------------------------------------------------

class ToWave a where
  type WaveShape a :: Symbol -> T
  type WaveShape a = GWaveShape (Rep a)

  -- | Convert an @a@ into an instant in the wave diagram
  toWave :: KnownSymbol n => a -> Waves (WaveShape a n) Instant

  default toWave
    :: forall n
     . ( Generic a
       , NFData a
       , GToWave (Rep a)
       , WaveShape a ~ GWaveShape (Rep a)
       , KnownSymbol n)
    => a -> Waves (WaveShape a n) Instant
  toWave c = case maybeIsX c of
    Nothing -> withSingI (waveShape (Proxy @a) (sing @n)) $ pure (Instant 'x' Nothing)
    Just c' -> gToWave . from $ c

  -- | This is necessary to still render the correct hierarchy when the input
  -- to @toWave@ is undefined
  --
  -- If @WaveShape a@ is not defined as some type family nonsense then
  -- @waveShape _ _ = sing@ should do.
  waveShape :: Proxy a -> SSymbol n -> Sing (WaveShape a n)
  default waveShape
    :: forall n r
     . ( GToWave (Rep a)
       , WaveShape a ~ GWaveShape (Rep a)
       )
    => Proxy a -> SSymbol n -> Sing (WaveShape a n)
  waveShape _ n = gWaveShape (Proxy @(Rep a)) n

-- | A helper function which fully evaluates the argument, if it contains an
-- @XException@ then the @Instant@ is replaced with an 'x' for undefined.
xInstantLeaf
  :: (NFData a, KnownSymbol n) => (a -> Waves (L n) Instant) -> a -> Waves (L n) Instant
xInstantLeaf f a = case maybeHasX a of
  Nothing -> pure (Instant 'x' Nothing)
  Just i  -> f i

-- | A helper function which evaluates the argument to WHNF, if this results in an
-- @XException@ then the @Instant@ is replaced with an 'x' for undefined.
xInstant
  :: forall a n. (ToWave a, KnownSymbol n)
  => (a -> Waves (WaveShape a n) Instant)
  -> a
  -> Waves (WaveShape a n) Instant
xInstant f a = case isX a of
  Left _ ->
    withSingI (waveShape (Proxy @a) (sing @n)) $ pure (Instant 'x' Nothing)
  Right a' -> f a'

----------------------------------------------------------------
-- Generic @ToWave@
----------------------------------------------------------------

class GToWave f where
  type GWaveShape f :: Symbol -> T
  gToWave :: KnownSymbol n => f a -> Waves (GWaveShape f n) Instant
  gWaveShape :: Proxy f -> SSymbol n -> Sing (GWaveShape f n)

instance GToWave c => GToWave (D1 m c) where
  type GWaveShape (D1 m c) = GWaveShape c
  gToWave = gToWave . unM1
  gWaveShape _ n = gWaveShape (Proxy @c) n

instance (Flattenable s, All ToWave (MapSnd (Flatten s))
         ) => GToWave (C1 (MetaCons c i True) s) where
  type GWaveShape (C1 (MetaCons c i True) s) = B (Shapes (Flatten s))
  gToWave = recToWaves . flatten . unM1
    where
      recToWaves
        :: forall ts n
         . (All ToWave (MapSnd ts), KnownSymbol n)
        => Rec ts
        -> Waves (B (Shapes ts) n) Instant
      recToWaves = \case
        Empty        -> error "Empty record"
        Some t Empty -> Cons (toWave t) (Nil @n)
        Some t ts    -> Cons (toWave t) (recToWaves ts)
  gWaveShape _ SSym = withSingI (flattenShapes (Proxy @s)) sing

instance (() ~ TypeError ('Text "Cannot generate Generic ToWave instance for non-record constructor"))
  => GToWave (C1 (MetaCons c i False) s) where
  type GWaveShape (C1 (MetaCons c i False) s) = L
  gToWave    = error "unreachable"
  gWaveShape = error "unreachable"

type family All c xs :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

type family MapSnd (a :: [(x, y)]) where
  MapSnd '[] = '[]
  MapSnd ('(x, y) ': xs) = y ': MapSnd xs

type family Shapes (a :: [ (Symbol, Type) ]) where
  Shapes '[] = '[]
  Shapes ('(n,a) ': xs) = WaveShape a n ': Shapes xs

shapesDist :: Shapes (xs ++ ys) :~: (Shapes xs ++ Shapes ys)
shapesDist = unsafeCoerce Refl

----------------------------------------------------------------
-- Generic records to Rec
----------------------------------------------------------------

data Rec ts where
  Empty :: Rec '[]
  Some :: KnownSymbol n => t -> Rec ts -> Rec ('(n, t) ': ts)

class Flattenable f where
  type Flatten f :: [ (Symbol, Type) ]
  flatten :: f a -> Rec (Flatten f)
  flattenShapes :: Proxy f -> SList (Shapes (Flatten f))

instance (ToWave a, KnownSymbol s) => Flattenable (S1 (MetaSel (Just s) u t l) (Rec0 a)) where
  type Flatten (S1 (MetaSel (Just s) u t l) (Rec0 a)) = '[ '(s, a) ]
  flatten (M1 (K1 a)) = a `Some` Empty
  flattenShapes _ = waveShape (Proxy @a) (sing @s) `SCons` SNil

instance (Flattenable a, Flattenable b) => Flattenable (a :*: b) where
  type Flatten (a :*: b) = Flatten a ++ Flatten b
  flatten (a :*: b) = flatten a `appendRec` flatten b
  flattenShapes _ = case shapesDist @(Flatten a) @(Flatten b) of
    Refl -> flattenShapes (Proxy @a) %++ flattenShapes (Proxy @b)

appendRec :: Rec xs -> Rec ys -> Rec (xs ++ ys)
appendRec Empty ys = ys
appendRec (Some x xs) ys = Some x (xs `appendRec` ys)

----------------------------------------------------------------
-- ToWave instances
----------------------------------------------------------------

newtype ShowWave a = ShowWave { unShowWave :: a }
  deriving newtype (NFData, BitPack)

newtype BitsWave a = BitsWave { unBitsWave :: a }
  deriving (Generic, NFData, NFDataX, Show, ShowX)

newtype WithBits a = WithBits { unWithBits :: a }
  deriving (Generic, NFData, NFDataX, Show, ShowX)

instance (NFData a, Show a) => ToWave (ShowWave a) where
  type WaveShape (ShowWave a) = L
  toWave = xInstantLeaf $ Single . Instant '=' . Just . tShow . unShowWave
  waveShape _ SSym = sing

instance (NFData a, BitPack a, KnownNat (BitSize a)) => ToWave (BitsWave a) where
  type WaveShape (BitsWave a) = WaveShape (BitVector (BitSize a))
  toWave = xInstant $ toWave . Clash.Prelude.pack . unBitsWave
  waveShape _ = waveShape (Proxy @(BitVector (BitSize a)))

instance (NFData a, Show a, BitPack a, KnownNat (BitSize a)) => ToWave (WithBits a) where
  type WaveShape (WithBits a)
    = B '[WaveShape (ShowWave a) "x", WaveShape (BitsWave a) "bits"]
  toWave (WithBits x) = toWave (ShowWave x) `Cons` toWave (BitsWave x) `Cons` Nil
  waveShape _ = SBranch
    (       waveShape (Proxy @(ShowWave a)) (sing @"x")
    `SCons` waveShape (Proxy @(BitsWave a)) (sing @"bits")
    `SCons` SNil
    )

tShow :: Show a => a -> Text
tShow = T.pack . show

instance ToWave Bit where
  type WaveShape Bit = L
  toWave = xInstant $ \case
    0 -> Single (Instant '0' Nothing)
    1 -> Single (Instant '1' Nothing)
  waveShape _ SSym = sing

instance ToWave Text where
  type WaveShape Text = L
  toWave = xInstantLeaf $ Single . Instant '=' . Just
  waveShape _ SSym = sing

deriving via (ShowWave Integer) instance ToWave Integer

--
-- Vec, along with some nonsense to get singletons to write indexedAsc.
--

-- TODO: remove this when it's in singletons
data SProxy :: forall a. Proxy a -> Type where
  SProxy :: forall a. SProxy ('Proxy @a)
type instance Sing = SProxy

type SToWave = ToWave

sWaveShapeP :: forall a n p. ToWave a => Sing (p :: Proxy a) -> Sing (n :: Symbol) -> Sing (WaveShape a n)
sWaveShapeP _ = waveShape (Proxy @a)

type WaveShapeP (p :: Proxy a) n = WaveShape a n
$(genDefunSymbols [''WaveShapeP])

$(singletonsOnly [d|
  indexedAsc :: ToWave a => Proxy a -> Nat -> Nat -> [T]
  indexedAsc a i n = if n == 0
             then []
             else waveShapeP a (show_ i) : indexedAsc a (i+1) (n-1)
  |])

-- | The positive case of the @IndexedAsc@ equation
indexedAscPositive
  :: forall a i n
   . (1 <= n)
  => Proxy a
  -> IndexedAsc ('Proxy :: Proxy a) i n
     :~: (WaveShape a (Show_ i) : IndexedAsc ('Proxy :: Proxy a) (i+1) (n-1))
indexedAscPositive _ = unsafeCoerce Refl

vecToWave
  :: forall n a s i
   . (KnownNat n, ToWave a, KnownSymbol s)
  => Sing i
  -> Vec n a
  -> Waves (B (IndexedAsc ( 'Proxy :: Proxy a) i n) s) Instant
vecToWave i = \case
  Clash.Nil       -> Nil
  Clash.Cons v vs -> case indexedAscPositive @a @i @n Proxy of
    Refl -> case sShow_ i of
      SSym -> toWave v `Cons` vecToWave @(n-1) (i %+ sing @1) vs

instance (KnownNat n, ToWave a) => ToWave (Vec n a) where
  type WaveShape (Vec n a) = B (IndexedAsc ('Proxy :: Proxy a) 0 n )
  toWave :: forall s. KnownSymbol s => Vec n a -> Waves (WaveShape (Vec n a) s) Instant
  toWave v = case isX v of
               Left _ -> withSingI (waveShape (Proxy @(Vec n a)) (sing @s)) $ pure (Instant 'x' Nothing)
               Right v' -> vecToWave (sing @0) v'
  waveShape _ s = SBranch (sIndexedAsc (SProxy @a) (sing @0) (sing @n)) s

instance KnownNat n => ToWave (BitVector n) where
  type WaveShape (BitVector n) = WaveShape (Vec n Bit)
  -- toWave :: forall s. KnownSymbol s => BitVector n -> Waves (WaveShape (BitVector n) s) Instant
  -- toWave b = case maybeIsX b of
  --   Nothing -> withSingI (waveShape (Proxy @(Vec n Bit)) (sing @s)) $ pure (Instant 'x' Nothing)
  --   Just b' -> toWave @(Vec n Bit) . Clash.Prelude.reverse . Clash.Prelude.unpack $ b'
  toWave = toWave @(Vec n Bit) . Clash.Prelude.reverse . Clash.Prelude.unpack
  waveShape _ = waveShape (Proxy @(Vec n Bit))

----------------------------------------------------------------
--
----------------------------------------------------------------

vecUndefined :: Signal System Foo
vecUndefined =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ (pure
        (Foo (errorX "vec")
             (Bar "hi" (J $ 1 :> 1 :> Clash.Nil))
        ) :: Signal System _
      )

test :: Signal System Foo
test =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ (pure
        (Foo (0 :> 1 :> (errorX "foo") :> 0 :> 1 :> Clash.Nil)
             (Bar "hi" (J $ 1 :> 1 :> Clash.Nil))
        ) :: Signal System _
      )

test' :: Signal System (ShowWave (Unsigned 4))
test' =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ (ShowWave <$> (let c = register 0 ((\x -> if x >= 3 then errorX "hi" else succ x) <$> c) in c :: Signal System _))

test'' :: Signal System (BitsWave (Unsigned 2))
test'' =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ (BitsWave <$> (let c = register 0 ((\x -> if x == 3 then errorX "hi" else succ x) <$> c) in c :: Signal System _))

bwUndefined :: Signal System (BitsWave (Unsigned 2))
bwUndefined =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ (BitsWave <$> (pure (errorX "hi") :: Signal System _))

test''' :: Signal System (WithBits (Unsigned 4))
test''' =
  withClockResetEnable systemClockGen systemResetGen enableGen
    $ (WithBits <$> (let c = register 0 ((\x -> if x == 3 then errorX "hi" else succ x) <$> c) in c :: Signal System _))

wavedrom
  :: forall a dom
   . (ToWave a, KnownDomain dom)
  => Int
  -> Signal dom a
  -> ByteString
wavedrom n =
  encodePretty
    . WaveDrom
    . (toJSON clockWave :)
    . pure
    . toJSON
    . simplify
    . foldr1 (<>)
    . fmap (fmap pure . toWave @_ @"out")
    . sampleN_lazy @dom n
  where
    clockWave = simplify $ Single @"clk"
      (replicate
        n
        (Instant
          (case activeEdge @dom of
            SRising  -> 'p'
            SFalling -> 'n'
          )
          Nothing
        )
      )

data WaveDrom t = WaveDrom { signal :: [Value] }
  deriving(Generic, ToJSON)

data Foo = Foo { foo :: Vec 5 Bit, bar :: Bar }
  deriving (Generic, NFData, ToWave)

data Bar = Bar { baz :: Text, qux :: J 2 }
  deriving (Generic, NFData, ToWave)

data J n = J { unJ :: Vec n Bit }
  deriving(Generic, NFData, ToWave)

