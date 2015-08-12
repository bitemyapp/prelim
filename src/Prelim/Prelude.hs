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
    , Real(
      toRational
    )
    , RealFloat(
      floatRadix
    , floatDigits
    , floatRange
    , decodeFloat
    , encodeFloat
    , exponent
    , significand
    , scaleFloat
    , isNaN
    , isInfinite
    , isDenormalized
    , isNegativeZero
    , isIEEE
    , atan2
    )
    , RealFrac(
      properFraction
    , truncate
    , round
    , ceiling
    , floor
    )
    , Show(
      showsPrec
    , show
    , showList
    )
    , ShowS
    , String
    , Traversable(
      traverse
    , sequenceA -- use semigroupoids?
    , mapM
    , sequence
    )
    , Word
    , (^)
    , (^^)
    , all
    , and
    , any
    , appendFile
    , asTypeOf
    , break
--  , concat
--  , concatMap
--  , const
--  , curry
    , cycle
    , drop
    , dropWhile
    , either
--  , error
    , even
    , filter
--  , flip
    , fromIntegral
    , fst
    , gcd
    , getChar
    , getContents
    , getLine
--  , head
--  , id
--  , init
    , interact
    , ioError
    , iterate
--  , last    
    , lcm
    , lex
    , lines
    , lookup
--  , map
    , mapM_
    , maybe
    , not
    , notElem
    , odd
    , or
    , otherwise
    , print
    , putChar
    , putStr
    , putStrLn
--  , read
    , readFile
--  , readIO
    , readLn
    , readParen
    , reads
    , realToFrac
    , repeat
    , replicate
    , reverse
    , scanl
--  , scanl1
    , scanr
--  , scanr1
--  , seq
    , sequence_
    , showChar
    , showParen
    , showString
    , shows
    , snd
    , span
    , splitAt
--  , tail
    , take
    , takeWhile
    , uncurry
--  , undefined
    , until
    , unwords
    , unzip
    , unzip3
    , userError
    , words
    , writeFile
    , zip
    , zip3
    , zipWith
    , zipWith3
    , (||)
  )
