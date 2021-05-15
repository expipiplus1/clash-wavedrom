{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.WaveDrom.Internal where

import qualified Clash.Prelude                 as Clash
import           Clash.Prelude           hiding ( (++)
                                                , Cons
                                                , Nil
                                                , SSymbol
                                                , foldr1
                                                , fromList
                                                , replicate
                                                , undefined
                                                )
import           Control.DeepSeq
import           Control.Lens                   ( transform )
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Lens                ( )
import           Data.Aeson.TH                  ( Options(fieldLabelModifier)
                                                , defaultOptions
                                                , deriveJSON
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Char                      ( toLower )
import           Data.Coerce
import           Data.Functor
import           Data.Kind
import           Data.List
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Monoid                    ( Ap(..) )
import           Data.Proxy
import           Data.Singletons.Prelude.Enum
import           Data.Singletons.Prelude.List   ( (%++)
                                                , type (++)
                                                , type (++@#@$)
                                                , Map
                                                , SList(SCons, SNil)
                                                , sFoldr
                                                , sMap
                                                )
import           Data.Singletons.Prelude.Num    ( type (+@#@$)
                                                , type (-@#@$)
                                                , SNum(..)
                                                )
import           Data.Singletons.Prelude.Proxy  ( SProxy(SProxy) )
import           Data.Singletons.Prelude.Show
import           Data.Singletons.TH      hiding ( type (<=) )
import qualified Data.Singletons.TypeLits
import           Data.Singletons.TypeLits       ( SSymbol(SSym) )
import qualified Data.Text                     as T
import           Data.Text               hiding ( foldr1
                                                , group
                                                , null
                                                , replicate
                                                )
import           Data.Type.Equality
import           Data.Vector                    ( fromList )
import qualified Data.Vector                   as V
import           Debug.Trace
import           GHC.Generics
import           GHC.TypeLits
import           Unsafe.Coerce

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
    w@(Single is) -> object
      [ "name" .= waveName w
      , "wave" .= (char <$> is)
      , "data" .= catMaybes (data' <$> is)
      ]
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

-- | Use the 'Show' instance of a type to make a 'ToWave' instance
newtype ShowWave a = ShowWave { unShowWave :: a }
  deriving newtype (NFData, BitPack)

-- | Use the 'BitPack' instance of a type to make a 'ToWave' instance showing
-- each bit individually
newtype BitsWave a = BitsWave { unBitsWave :: a }
  deriving (Generic, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

-- | Use both 'Show' and 'BitPack' instances to show the text representation of
-- the value as well as the exploded bit view.
newtype WithBits a = WithBits { unWithBits :: a }
  deriving (Generic, Show)
  deriving anyclass (NFData, NFDataX, ShowX)

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
    = B '[WaveShape (ShowWave a) "x" , WaveShape (BitsWave a) "bits"]
  toWave (WithBits x) =
    toWave (ShowWave x) `Cons` toWave (BitsWave x) `Cons` Nil
  waveShape _ = SBranch
    (       waveShape (Proxy @(ShowWave a)) (sing @"x")
    `SCons` waveShape (Proxy @(BitsWave a)) (sing @"bits")
    `SCons` SNil
    )

tShow :: Show a => a -> Text
tShow = T.pack . show

instance ToWave Bit where
  type WaveShape Bit = L
  -- TODO: Why doesn't maybeHasX work here?
  toWave = fromMaybe (Single (Instant 'x' Nothing)) . maybeIsX . \case
    0 -> Single (Instant '0' Nothing)
    1 -> Single (Instant '1' Nothing)
  waveShape _ SSym = sing

instance ToWave Text where
  type WaveShape Text = L
  toWave = xInstantLeaf $ Single . Instant '=' . Just
  waveShape _ SSym = sing

deriving via (ShowWave Integer) instance ToWave Integer

type family Prepend a b where
  Prepend (Leaf a) (Branch xs n) = Branch (Leaf a ': xs)
  Prepend (Leaf a) (Leaf b) = Branch [Leaf a, Leaf b]

data PrependWave (aName :: Symbol) (bName :: Symbol) a b = PrependWave
  { flatA :: a
  , flatB :: b
  }

instance (KnownSymbol p, KnownSymbol i, ToWave a, ToWave b) => ToWave (PrependWave p i a b) where
  type WaveShape (PrependWave p i a b) = Prepend (WaveShape a p) (WaveShape b i)

  toWave
    :: forall n
     . KnownSymbol n
    => PrependWave p i a b
    -> Waves (WaveShape (PrependWave p i a b) n) Instant
  toWave (PrependWave a b) = case toWave @_ @p a of
    Single a -> case toWave @_ @n b of
      b@(Cons _ _) -> Single a `Cons` b
      (  Single b) -> Single a `Cons` Single b `Cons` Nil

  waveShape (_ :: Proxy (PrependWave p i a b)) s =
    case waveShape (Proxy @a) (SSym @p) of
      SLeaf a -> case waveShape (Proxy @b) (SSym @i) of
        SBranch ts b -> SBranch (SLeaf a `SCons` ts) s
        _            -> error ""
      _ -> error ""

data NamedPair (lName :: Symbol) (rName :: Symbol) l r = NamedPair
  { npLeft :: l
  , npRight :: r
  }

instance (KnownSymbol lName, KnownSymbol rName, ToWave l, ToWave r) => ToWave (NamedPair lName rName l r) where
  type WaveShape (NamedPair lName rName l r) = Branch
    '[WaveShape l lName , WaveShape r rName]

  toWave (NamedPair a b) = toWave a `Cons` toWave b `Cons` Nil

  waveShape _ = SBranch
    (       waveShape (Proxy @l) (sing @lName)
    `SCons` waveShape (Proxy @r) (sing @rName)
    `SCons` SNil
    )


--
-- Vec, along with some nonsense to get singletons to write indexedDsc.
--

type SToWave = ToWave

sWaveShapeP
  :: forall a n p
   . ToWave a
  => Sing (p :: Proxy a)
  -> Sing (n :: Symbol)
  -> Sing (WaveShape a n)
sWaveShapeP _ = waveShape (Proxy @a)

type WaveShapeP (p :: Proxy a) n = WaveShape a n
$(genDefunSymbols [''WaveShapeP])

$(singletonsOnly [d|
  indexedDsc :: ToWave a => Proxy a -> Nat -> [T]
  indexedDsc a i = if i == 0
             then []
             else waveShapeP a (show_ (i - 1)) : indexedDsc a (i-1)
  |])

-- | The positive case of the @IndexedDsc@ equation
indexedDscPositive
  :: forall a i
   . (1 <= i)
  => Proxy a
  -> IndexedDsc ('Proxy :: Proxy a) i
     :~: (WaveShape a (Show_ (i - 1)) : IndexedDsc ('Proxy :: Proxy a) (i-1))
indexedDscPositive _ = unsafeCoerce Refl

vecToWave
  :: forall i a s
   . (KnownNat i, ToWave a, KnownSymbol s)
  => Vec i a
  -> Waves (B (IndexedDsc ( 'Proxy :: Proxy a) i) s) Instant
vecToWave = \case
  Clash.Nil       -> Nil
  Clash.Cons v vs -> case indexedDscPositive @a @i Proxy of
    Refl -> case sShow_ (sing @(i - 1)) of
      SSym -> toWave v `Cons` vecToWave @(i - 1) vs

instance (KnownNat n, ToWave a) => ToWave (Vec n a) where
  type WaveShape (Vec n a) = B (IndexedDsc ( 'Proxy :: Proxy a) n)
  toWave
    :: forall s
     . KnownSymbol s
    => Vec n a
    -> Waves (WaveShape (Vec n a) s) Instant
  toWave v = case isX v of
    Left _ -> withSingI (waveShape (Proxy @(Vec n a)) (sing @s))
      $ pure (Instant 'x' Nothing)
    Right v' -> vecToWave @n (Clash.reverse v')
  waveShape _ s = SBranch (sIndexedDsc (SProxy @a) (sing @n)) s

instance KnownNat n => ToWave (BitVector n) where
  type WaveShape (BitVector n) = WaveShape (Vec n Bit)
  toWave = toWave @(Vec n Bit) . Clash.Prelude.reverse . Clash.Prelude.bv2v
  waveShape _ = waveShape (Proxy @(Vec n Bit))

deriving via ShowWave (Unsigned n) instance ToWave (Unsigned n)

----------------------------------------------------------------
--
----------------------------------------------------------------

render :: WaveDrom -> ByteString
render = encodePretty

-- | Generate a wave diagram with no clock or reset
wavedrom
  :: forall a dom
   . (ToWave a, KnownDomain dom)
  => Int
  -> String
  -> Signal dom a
  -> WaveDrom
wavedrom n name sig = WaveDrom . pure $ case someSymbolVal name of
  SomeSymbol (_ :: Proxy s) -> toJSON (signalToWave @_ @_ @s n sig)

-- | Generate a wave diagram with a clock at the top
wavedromWithClock
  :: forall a dom
   . (ToWave a, KnownDomain dom)
  => Int
  -> String
  -> Signal dom a
  -> WaveDrom
wavedromWithClock n name sig = case wavedrom n name sig of
  WaveDrom w -> WaveDrom (toJSON clockWave : w)
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

-- | Generate a wave diagram with clock and reset
wavedromWithReset
  :: forall a dom
   . (ToWave a, KnownDomain dom)
  => Int
  -> String
  -> Signal dom a
  -> WaveDrom
wavedromWithReset n name s =
  let reset = boolToBit <$> unsafeFromReset (resetGen @dom)
  in  case someSymbolVal name of
        SomeSymbol (_ :: Proxy nameS) ->
          wavedromWithClock n "" (NamedPair @"rst" @nameS <$> reset <*> s)

data WaveDrom = WaveDrom { signal :: [Value], config :: Object }

instance ToJSON WaveDrom where
  toJSON wd = flattenUnnamed $ object ["signal" .= toJSON (signal wd)]

flattenUnnamed :: Value -> Value
flattenUnnamed = transform $ \case
  Array xs -> Array $ V.concatMap
    (\case
      Array vs | String "" : vs' <- V.toList vs -> V.tail vs
      x -> V.singleton x
    )
    xs
  x -> x

signalToWave
  :: forall a dom n
   . (ToWave a, KnownDomain dom, KnownSymbol n)
  => Int
  -> Signal dom a
  -> Waves (WaveShape a n) [Instant]
signalToWave n =
   simplify
  . foldr1 (<>)
  . fmap (fmap pure . toWave @_ @n)
  . sampleN_lazy @dom n
