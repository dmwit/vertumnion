-- Why not just use the multiset package? Because I want to be able to
-- efficiently query how many elements are smaller than a given one. The
-- multiset package's size operation is O(n), so split+size is dominated by the
-- O(n) size calculation; this module's size operation is O(1), so split+size
-- is dominated by the cheaper O(log n) split operation.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Multiset
	( Multiset
	, empty, singleton, insert, union
	, size, multiplicity
	, countLT, countLE, countGT, countGE
	, (!?)
	) where

import Data.Coerce
import Data.FingerTree (Measured(..), FingerTree)
import Data.Monoid
import Data.Semigroup
import Data.Word
import qualified Data.FingerTree as FT

-- TODO: This is making all the wrong tradeoffs. The common case for our use is
-- that all the values in the MultiSet will be distinct, and the Word will just
-- be wasted space.
data Element a = Element
	{ eMultiplicity :: Word
	, eValue :: a
	} deriving (Eq, Ord, Read, Show)

data Measure a = Measure
	{ mMultiplicity :: !Word
	, mValue :: Maybe a
	} deriving (Eq, Ord, Read, Show)

instance Ord a => Semigroup (Measure a) where
	m <> m' = Measure
		{ mMultiplicity = mMultiplicity m + mMultiplicity m'
		, mValue = coerce ((<>) @(Option (Max a))) (mValue m) (mValue m')
		}

instance Ord a => Monoid (Measure a) where
	mempty = Measure
		{ mMultiplicity = 0
		, mValue = Nothing
		}

instance Ord a => Measured (Measure a) (Element a) where
	measure e = Measure
		{ mMultiplicity = eMultiplicity e
		, mValue = Just (eValue e)
		}

newtype Multiset a = Multiset { msFT :: FingerTree (Measure a) (Element a) }
	deriving (Eq, Ord, Show)

empty :: Ord a => Multiset a
empty = Multiset FT.empty

singleton :: Ord a => a -> Multiset a
singleton = Multiset . FT.singleton . Element 1

split :: Ord a => a -> Multiset a -> (Multiset a, Element a, Multiset a)
split a ms = case FT.search (\l _ -> mValue l >= Just a) (coerce ms) of
	FT.Position l h r
		| a == eValue h -> (Multiset l, h, Multiset r)
		| otherwise -> (Multiset l, none, Multiset $ h FT.<| r)
	FT.OnLeft -> (empty, none, ms) -- impossible
	FT.OnRight -> (ms, none, empty)
	FT.Nowhere -> error "internal invariant violation detected in Data.Multiset.split"
	where
	none = Element 0 a

insert :: Ord a => a -> Multiset a -> Multiset a
insert a ms = let (Multiset l, h, Multiset r) = split a ms in
	Multiset (l FT.>< h { eMultiplicity = eMultiplicity h + 1 } FT.<| r)

union :: Ord a => Multiset a -> Multiset a -> Multiset a
union (Multiset ft) (Multiset ft') = Multiset (merge ft ft') where
	merge ft ft'
		| FT.null ft' = ft
		| otherwise = case FT.viewl ft of
			FT.EmptyL -> ft'
			e FT.:< es -> coerce l FT.>< e' FT.<| merge es (coerce r) where
				(l, h, r) = split (eValue e) (coerce ft')
				e' = e { eMultiplicity = eMultiplicity e + eMultiplicity h }

size :: Ord a => Multiset a -> Word
size = mMultiplicity . measure . msFT

countLT, countLE, countGT, countGE :: Ord a => a -> Multiset a -> Word
countLT a ms = let (l, h, r) = split a ms in size l
countLE a ms = let (l, h, r) = split a ms in size l + eMultiplicity h
countGT a ms = let (l, h, r) = split a ms in size r
countGE a ms = let (l, h, r) = split a ms in size r + eMultiplicity h

multiplicity :: Ord a => a -> Multiset a -> Word
multiplicity a ms = let (_, h, _) = split a ms in eMultiplicity h

(!?) :: Ord a => Multiset a -> Word -> Maybe a
Multiset ft !? ix = case FT.search (\l _ -> mMultiplicity l > ix) ft of
	FT.Position _ e _ -> Just (eValue e)
	FT.OnLeft -> error ("impossible: 0 > " ++ show ix ++ " at type Word")
	FT.OnRight -> Nothing
	FT.Nowhere -> error "internal invariant violation detected in Data.Multiset.!?"
