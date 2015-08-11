module Prelim.Prelude(
  module P
) where

import Prelude as P(
--  (!!)
    ($)
--, ($!) 
  , (++)
  , (&&)
--, (.)
  , (<$>)
  , (=<<)
  , Applicative(pure, (<*>), (*>), (<*))
  , Bool(
      False
    , True
    )
  , Bounded(
      minBound
    , maxBound
    )
  , Char
  , Double
  , Either(
      Left
    , Right
    )
--, Enum(
--    succ
--  , pred
--  , toEnum
--  , fromEnum
--  , enumFrom
--  , enumFromThen
--  , enumFromTo
--  , enumFromThenTo
--  )
  , Eq(
      (==)
    , (/=)
    )
  , String
  , Float
  , Floating(
      pi
    , exp
    , log
    , sqrt
    , (**)
    , logBase
    , sin
    , cos
    , tan
    , asin
    , acos
    , atan
    , sinh
    , cosh
    , tanh
    , asinh
    , acosh
    , atanh
    )
    , Foldable(
      foldMap
    , foldr
    , foldl
--  , foldr1
--  , foldl1
    , null
    , length
    , elem
--  , maximum
--  , minimum
    , sum
    , product
    )
    , Fractional(
      (/)
    , recip
    , fromRational
    )
    , Functor(
      fmap
    , (<$)
    )
    , IO
    , IOError
    , Int
    , Integer
    , Integral(
      quot
    , rem
    , div
    , mod
    , quotRem
    , divMod
    , toInteger
    )
    , Maybe(Nothing, Just)
    , Monad(
      (>>=)
    , (>>)
    , return
    , fail
    )
    , Monoid(
      mempty
--  , mappend
--  , mconcat
    )
    , Num(
      (+)
    , (-)
    , (*)
    , negate
    , abs
    , signum
    , fromInteger
    )
    , Ord(
      compare
    , (<)
    , (<=)
    , (>)
    , (>=)
    , max
    , min
    )
    , Ordering(
      LT
    , EQ
    , GT
    )
    , Rational
--  , Read(
--    readsPrec
--  , readList
--  )
--  , ReadS
  )

{-
>> :browse Prelude
class (GHC.Num.Num a, ghc-prim-0.4.0.0:GHC.Classes.Ord a) =>
      GHC.Real.Real a where
  GHC.Real.toRational :: a -> GHC.Real.Rational
class (GHC.Real.RealFrac a, GHC.Float.Floating a) =>
      GHC.Float.RealFloat a where
  GHC.Float.floatRadix ::
    a -> integer-gmp-1.0.0.0:GHC.Integer.Type.Integer
  GHC.Float.floatDigits :: a -> ghc-prim-0.4.0.0:GHC.Types.Int
  GHC.Float.floatRange ::
    a
    -> (ghc-prim-0.4.0.0:GHC.Types.Int, ghc-prim-0.4.0.0:GHC.Types.Int)
  GHC.Float.decodeFloat ::
    a
    -> (integer-gmp-1.0.0.0:GHC.Integer.Type.Integer,
        ghc-prim-0.4.0.0:GHC.Types.Int)
  GHC.Float.encodeFloat ::
    integer-gmp-1.0.0.0:GHC.Integer.Type.Integer
    -> ghc-prim-0.4.0.0:GHC.Types.Int -> a
  GHC.Float.exponent :: a -> ghc-prim-0.4.0.0:GHC.Types.Int
  GHC.Float.significand :: a -> a
  GHC.Float.scaleFloat :: ghc-prim-0.4.0.0:GHC.Types.Int -> a -> a
  GHC.Float.isNaN :: a -> ghc-prim-0.4.0.0:GHC.Types.Bool
  GHC.Float.isInfinite :: a -> ghc-prim-0.4.0.0:GHC.Types.Bool
  GHC.Float.isDenormalized :: a -> ghc-prim-0.4.0.0:GHC.Types.Bool
  GHC.Float.isNegativeZero :: a -> ghc-prim-0.4.0.0:GHC.Types.Bool
  GHC.Float.isIEEE :: a -> ghc-prim-0.4.0.0:GHC.Types.Bool
  GHC.Float.atan2 :: a -> a -> a
class (GHC.Real.Real a, GHC.Real.Fractional a) =>
      GHC.Real.RealFrac a where
  GHC.Real.properFraction :: GHC.Real.Integral b => a -> (b, a)
  GHC.Real.truncate :: GHC.Real.Integral b => a -> b
  GHC.Real.round :: GHC.Real.Integral b => a -> b
  GHC.Real.ceiling :: GHC.Real.Integral b => a -> b
  GHC.Real.floor :: GHC.Real.Integral b => a -> b
class GHC.Show.Show a where
  GHC.Show.showsPrec ::
    ghc-prim-0.4.0.0:GHC.Types.Int -> a -> GHC.Show.ShowS
  GHC.Show.show :: a -> GHC.Base.String
  GHC.Show.showList :: [a] -> GHC.Show.ShowS
type GHC.Show.ShowS = GHC.Base.String -> GHC.Base.String
type GHC.Base.String = [ghc-prim-0.4.0.0:GHC.Types.Char]
class (GHC.Base.Functor t, Data.Foldable.Foldable t) =>
      Data.Traversable.Traversable (t :: * -> *) where
  Data.Traversable.traverse ::
    GHC.Base.Applicative f => (a -> f b) -> t a -> f (t b)
  Data.Traversable.sequenceA ::
    GHC.Base.Applicative f => t (f a) -> f (t a)
  Data.Traversable.mapM ::
    GHC.Base.Monad m => (a -> m b) -> t a -> m (t b)
  Data.Traversable.sequence :: GHC.Base.Monad m => t (m a) -> m (t a)
data ghc-prim-0.4.0.0:GHC.Types.Word
  = ghc-prim-0.4.0.0:GHC.Types.W# ghc-prim-0.4.0.0:GHC.Prim.Word#
(GHC.Real.^) :: (GHC.Num.Num a, GHC.Real.Integral b) => a -> b -> a
(GHC.Real.^^) ::
  (GHC.Real.Fractional a, GHC.Real.Integral b) => a -> b -> a
Data.Foldable.all ::
  Data.Foldable.Foldable t =>
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool)
  -> t a -> ghc-prim-0.4.0.0:GHC.Types.Bool
Data.Foldable.and ::
  Data.Foldable.Foldable t =>
  t ghc-prim-0.4.0.0:GHC.Types.Bool
  -> ghc-prim-0.4.0.0:GHC.Types.Bool
Data.Foldable.any ::
  Data.Foldable.Foldable t =>
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool)
  -> t a -> ghc-prim-0.4.0.0:GHC.Types.Bool
System.IO.appendFile ::
  GHC.IO.FilePath
  -> GHC.Base.String -> ghc-prim-0.4.0.0:GHC.Types.IO ()
GHC.Base.asTypeOf :: a -> a -> a
GHC.List.break ::
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool) -> [a] -> ([a], [a])
Data.Foldable.concat :: Data.Foldable.Foldable t => t [a] -> [a]
Data.Foldable.concatMap ::
  Data.Foldable.Foldable t => (a -> [b]) -> t a -> [b]
GHC.Base.const :: a -> b -> a
Data.Tuple.curry :: ((a, b) -> c) -> a -> b -> c
GHC.List.cycle :: [a] -> [a]
GHC.List.drop :: ghc-prim-0.4.0.0:GHC.Types.Int -> [a] -> [a]
GHC.List.dropWhile ::
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool) -> [a] -> [a]
Data.Either.either ::
  (a -> c) -> (b -> c) -> Data.Either.Either a b -> c
GHC.Err.error :: [ghc-prim-0.4.0.0:GHC.Types.Char] -> a
GHC.Real.even ::
  GHC.Real.Integral a => a -> ghc-prim-0.4.0.0:GHC.Types.Bool
GHC.List.filter ::
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool) -> [a] -> [a]
GHC.Base.flip :: (a -> b -> c) -> b -> a -> c
GHC.Real.fromIntegral ::
  (GHC.Real.Integral a, GHC.Num.Num b) => a -> b
Data.Tuple.fst :: (a, b) -> a
GHC.Real.gcd :: GHC.Real.Integral a => a -> a -> a
System.IO.getChar ::
  ghc-prim-0.4.0.0:GHC.Types.IO ghc-prim-0.4.0.0:GHC.Types.Char
System.IO.getContents ::
  ghc-prim-0.4.0.0:GHC.Types.IO GHC.Base.String
System.IO.getLine :: ghc-prim-0.4.0.0:GHC.Types.IO GHC.Base.String
GHC.List.head :: [a] -> a
GHC.Base.id :: a -> a
GHC.List.init :: [a] -> [a]
System.IO.interact ::
  (GHC.Base.String -> GHC.Base.String)
  -> ghc-prim-0.4.0.0:GHC.Types.IO ()
GHC.IO.Exception.ioError ::
  GHC.IO.Exception.IOError -> ghc-prim-0.4.0.0:GHC.Types.IO a
GHC.List.iterate :: (a -> a) -> a -> [a]
GHC.List.last :: [a] -> a
GHC.Real.lcm :: GHC.Real.Integral a => a -> a -> a
GHC.Read.lex :: Text.ParserCombinators.ReadP.ReadS GHC.Base.String
base-4.8.0.0:Data.OldList.lines ::
  GHC.Base.String -> [GHC.Base.String]
GHC.List.lookup ::
  ghc-prim-0.4.0.0:GHC.Classes.Eq a =>
  a -> [(a, b)] -> GHC.Base.Maybe b
GHC.Base.map :: (a -> b) -> [a] -> [b]
Data.Foldable.mapM_ ::
  (Data.Foldable.Foldable t, GHC.Base.Monad m) =>
  (a -> m b) -> t a -> m ()
Data.Maybe.maybe :: b -> (a -> b) -> GHC.Base.Maybe a -> b
ghc-prim-0.4.0.0:GHC.Classes.not ::
  ghc-prim-0.4.0.0:GHC.Types.Bool -> ghc-prim-0.4.0.0:GHC.Types.Bool
Data.Foldable.notElem ::
  (Data.Foldable.Foldable t, ghc-prim-0.4.0.0:GHC.Classes.Eq a) =>
  a -> t a -> ghc-prim-0.4.0.0:GHC.Types.Bool
GHC.Real.odd ::
  GHC.Real.Integral a => a -> ghc-prim-0.4.0.0:GHC.Types.Bool
Data.Foldable.or ::
  Data.Foldable.Foldable t =>
  t ghc-prim-0.4.0.0:GHC.Types.Bool
  -> ghc-prim-0.4.0.0:GHC.Types.Bool
GHC.Base.otherwise :: ghc-prim-0.4.0.0:GHC.Types.Bool
System.IO.print ::
  GHC.Show.Show a => a -> ghc-prim-0.4.0.0:GHC.Types.IO ()
System.IO.putChar ::
  ghc-prim-0.4.0.0:GHC.Types.Char -> ghc-prim-0.4.0.0:GHC.Types.IO ()
System.IO.putStr ::
  GHC.Base.String -> ghc-prim-0.4.0.0:GHC.Types.IO ()
System.IO.putStrLn ::
  GHC.Base.String -> ghc-prim-0.4.0.0:GHC.Types.IO ()
Text.Read.read :: GHC.Read.Read a => GHC.Base.String -> a
System.IO.readFile ::
  GHC.IO.FilePath -> ghc-prim-0.4.0.0:GHC.Types.IO GHC.Base.String
System.IO.readIO ::
  GHC.Read.Read a =>
  GHC.Base.String -> ghc-prim-0.4.0.0:GHC.Types.IO a
System.IO.readLn ::
  GHC.Read.Read a => ghc-prim-0.4.0.0:GHC.Types.IO a
GHC.Read.readParen ::
  ghc-prim-0.4.0.0:GHC.Types.Bool
  -> Text.ParserCombinators.ReadP.ReadS a
  -> Text.ParserCombinators.ReadP.ReadS a
Text.Read.reads ::
  GHC.Read.Read a => Text.ParserCombinators.ReadP.ReadS a
GHC.Real.realToFrac ::
  (GHC.Real.Real a, GHC.Real.Fractional b) => a -> b
GHC.List.repeat :: a -> [a]
GHC.List.replicate :: ghc-prim-0.4.0.0:GHC.Types.Int -> a -> [a]
GHC.List.reverse :: [a] -> [a]
GHC.List.scanl :: (b -> a -> b) -> b -> [a] -> [b]
GHC.List.scanl1 :: (a -> a -> a) -> [a] -> [a]
GHC.List.scanr :: (a -> b -> b) -> b -> [a] -> [b]
GHC.List.scanr1 :: (a -> a -> a) -> [a] -> [a]
ghc-prim-0.4.0.0:GHC.Prim.seq :: a -> b -> b
Data.Foldable.sequence_ ::
  (Data.Foldable.Foldable t, GHC.Base.Monad m) => t (m a) -> m ()
GHC.Show.showChar ::
  ghc-prim-0.4.0.0:GHC.Types.Char -> GHC.Show.ShowS
GHC.Show.showParen ::
  ghc-prim-0.4.0.0:GHC.Types.Bool -> GHC.Show.ShowS -> GHC.Show.ShowS
GHC.Show.showString :: GHC.Base.String -> GHC.Show.ShowS
GHC.Show.shows :: GHC.Show.Show a => a -> GHC.Show.ShowS
Data.Tuple.snd :: (a, b) -> b
GHC.List.span ::
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool) -> [a] -> ([a], [a])
GHC.List.splitAt ::
  ghc-prim-0.4.0.0:GHC.Types.Int -> [a] -> ([a], [a])
GHC.Num.subtract :: GHC.Num.Num a => a -> a -> a
GHC.List.tail :: [a] -> [a]
GHC.List.take :: ghc-prim-0.4.0.0:GHC.Types.Int -> [a] -> [a]
GHC.List.takeWhile ::
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool) -> [a] -> [a]
Data.Tuple.uncurry :: (a -> b -> c) -> (a, b) -> c
GHC.Err.undefined :: a
base-4.8.0.0:Data.OldList.unlines ::
  [GHC.Base.String] -> GHC.Base.String
GHC.Base.until ::
  (a -> ghc-prim-0.4.0.0:GHC.Types.Bool) -> (a -> a) -> a -> a
base-4.8.0.0:Data.OldList.unwords ::
  [GHC.Base.String] -> GHC.Base.String
GHC.List.unzip :: [(a, b)] -> ([a], [b])
GHC.List.unzip3 :: [(a, b, c)] -> ([a], [b], [c])
GHC.IO.Exception.userError ::
  GHC.Base.String -> GHC.IO.Exception.IOError
base-4.8.0.0:Data.OldList.words ::
  GHC.Base.String -> [GHC.Base.String]
System.IO.writeFile ::
  GHC.IO.FilePath
  -> GHC.Base.String -> ghc-prim-0.4.0.0:GHC.Types.IO ()
GHC.List.zip :: [a] -> [b] -> [(a, b)]
GHC.List.zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
GHC.List.zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
GHC.List.zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
(ghc-prim-0.4.0.0:GHC.Classes.||) ::
  ghc-prim-0.4.0.0:GHC.Types.Bool
  -> ghc-prim-0.4.0.0:GHC.Types.Bool
  -> ghc-prim-0.4.0.0:GHC.Types.Bool
-}