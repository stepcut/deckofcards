{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Type.Equality ((:~:)(Refl),type (==))
import GHC.TypeLits

------------------------------------------------------------------------
--  Rank
------------------------------------------------------------------------

-- | card ranks, with Aces high
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
   deriving (Eq, Ord, Read, Show, Enum)

class RankVal (rank :: Rank) where
    rankVal :: Proxy rank -> Rank

instance RankVal Two   where rankVal _ = Two
instance RankVal Three where rankVal _ = Three
instance RankVal Four  where rankVal _ = Four
instance RankVal Five  where rankVal _ = Five
instance RankVal Six   where rankVal _ = Six
instance RankVal Seven where rankVal _ = Seven
instance RankVal Eight where rankVal _ = Eight
instance RankVal Nine  where rankVal _ = Nine
instance RankVal Ten   where rankVal _ = Ten
instance RankVal Jack  where rankVal _ = Jack
instance RankVal Queen where rankVal _ = Queen
instance RankVal King  where rankVal _ = King
instance RankVal Ace   where rankVal _ = Ace

-- | show the `rank` as a `String`
showRank :: (RankVal rank) => Proxy rank -> String
showRank p = show (rankVal p)

-- | all the card ranks
type Ranks = '[Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

-- | all the card ranks as a proxy value
ranks :: Proxy Ranks
ranks = Proxy

------------------------------------------------------------------------
--  Suit
------------------------------------------------------------------------

data Suit = Clubs | Diamonds | Hearts | Spades
   deriving (Eq, Ord, Read, Show, Enum)

class SuitVal (suit :: Suit) where
    suitVal :: Proxy suit -> Suit

instance SuitVal Clubs    where suitVal _ = Clubs
instance SuitVal Diamonds where suitVal _ = Diamonds
instance SuitVal Hearts   where suitVal _ = Hearts
instance SuitVal Spades   where suitVal _ = Spades

-- | show the `suit` as a `String`
showSuit :: (SuitVal suit) => Proxy suit -> String
showSuit p = show (suitVal p)

-- | all the card suits
type Suits = '[Clubs, Diamonds, Hearts, Spades]

-- | all the card suits as a proxy value
suits :: Proxy Suits
suits = Proxy

------------------------------------------------------------------------
--  Card
------------------------------------------------------------------------

-- | a 'Card' has a 'Rank' and a 'Suit'
data Card :: Rank -> Suit -> * where
    MkCard :: Card rank suit

-- | helper function for building a 'Card' from 'Proxy' values
mkCard :: Proxy (rank :: Rank) -> Proxy (suit :: Suit) -> Card rank suit
mkCard _ _ = MkCard

-- | pretty print the 'Card'
showCard :: forall rank suit. (RankVal rank, SuitVal suit) => Card rank suit -> String
showCard _ = showRank (Proxy :: Proxy rank) <> " of " <> showSuit (Proxy :: Proxy suit)

-- | example card
aceOfSpades :: Card Ace Spades
aceOfSpades = MkCard

------------------------------------------------------------------------
--  type level boolean functions
------------------------------------------------------------------------

data Equal x y
data Not x
data And x y
data Or x y

type family ToBool a where
    ToBool (And True True) = True
    ToBool (And x False) = False
    ToBool (And False y) = False
    ToBool (And x True) = ToBool x
    ToBool (And True y) = ToBool y
    ToBool (Or True y) = True
    ToBool (Or x True) = True
    ToBool (Or False False) = False
    ToBool (Or x False) = ToBool x
    ToBool (Or False y) = ToBool y
    ToBool (Not True)  = False
    ToBool (Not False) = True
    ToBool (Not p) = ToBool (Not (ToBool p))
    ToBool (Equal x x) = True
    ToBool (Equal x y) = False

------------------------------------------------------------------------
--  type level length
------------------------------------------------------------------------

type family Length (list :: [k]) where
    Length '[] = 0
    Length (c ': cs) = 1 + Length cs

------------------------------------------------------------------------
--  type level isElem
------------------------------------------------------------------------

type family IsElem (c :: k) (cs :: [k]) where
    IsElem c '[] = False
    IsElem c (c' ': cs) = ToBool (Or (Equal c c') (IsElem c cs))

------------------------------------------------------------------------
--  type level: check if all elements in a type level list are unique
------------------------------------------------------------------------

type family Unique (cards :: [k]) where
    Unique '[] = True
    Unique (c ': cs) = ToBool (And (Not (IsElem c cs)) (Unique cs))

------------------------------------------------------------------------
--  Cards
------------------------------------------------------------------------

data Cards :: Nat -> [card] -> * where
    -- ensure that there are a specific number of a cards and that each card appears only once
    Cards :: (Length cards :~: (n :: Nat)) -> (Unique cards :~: True) -> Cards n cards

instance Show (Cards (n :: Nat) (cs :: [k])) where
    show (Cards Refl Refl) = "Cards Refl Refl"

-- | helper function to make 'Cards'
mkCards :: (Length cards ~ (n :: Nat), Unique cards ~ True) => Cards n cards
mkCards = Cards Refl Refl

-- | add a card to a set of cards
--
-- card must not already be in the deck
addCard :: ((1 + n) ~ (1 + Length cards), Unique (Card rank suit ': cards) ~ True) => Card rank suit -> Cards n cards -> Cards (1 + n) (Card rank suit ': cards)
addCard c cs = mkCards

------------------------------------------------------------------------
--  example cards
------------------------------------------------------------------------

-- | an empty set of cards
cards0 :: Cards 0 '[]
cards0 = Cards Refl Refl

-- | just the ace of spaces
cards1 :: Cards 1 '[Card Ace Spades]
cards1 = Cards Refl Refl

-- | the ace of spaces and the ace of diamonds
cards2 :: Cards 2 '[Card Ace Spades, Card Ace Diamonds]
cards2 = mkCards

------------------------------------------------------------------------
--  SimpleCard
------------------------------------------------------------------------

-- | A card type which does not have its 'Rank' and 'Suit' in the type level
data SimpleCard = SimpleCard Rank Suit
   deriving (Eq, Ord, Read, Show)

-- | convert a 'Card' to a 'SimpleCard'
toSimpleCard :: forall rank suit. (RankVal rank, SuitVal suit) => Card rank suit -> SimpleCard
toSimpleCard _ = SimpleCard (rankVal (Proxy :: Proxy rank)) (suitVal (Proxy :: Proxy suit))

------------------------------------------------------------------------
--  SimpleCards
------------------------------------------------------------------------

-- | a list of cards
type SimpleCards = [SimpleCard]

-- | convert a list of 'Cards' to '[SimpleCard]'
class ToSimpleCards a where
    toSimpleCards :: a -> SimpleCards

instance ToSimpleCards (Cards 0 '[]) where
    toSimpleCards _ = []

instance forall rank suit n cs. (RankVal rank, SuitVal suit, Unique cs ~ True, Length cs ~ (n - 1), ToSimpleCards (Cards (n - 1) cs)) => ToSimpleCards (Cards n ((Card rank suit) ': cs)) where
    toSimpleCards _ = toSimpleCard (MkCard :: Card rank suit) : toSimpleCards (mkCards :: Cards (n - 1) cs)


type family Append (a :: [k]) (b :: [k]) where
    Append '[] bs = bs
    Append (a ': as) bs = a ': (Append as bs)

type family GenCards (r :: [Rank]) (s :: [Suit]) where
    GenCards r '[] = '[]
    GenCards '[] s = '[]
    GenCards (r ': rr) (s ': ss)  = Append ((Card r s) ': GenCards (r ': '[]) ss) (GenCards rr (s ': ss))

gen :: Proxy (GenCards '[Two, Three] '[Clubs, Diamonds])
gen = undefined

deckOfCards :: Cards 52 (GenCards Ranks Suits)
deckOfCards = mkCards


-- mkCardsProxy :: (Length cards ~ (n :: Nat), Unique cards ~ True) -> Proxy cards -> Cards n cards

{-
cards3 :: Cards 3 '[Card Ace Spades, Card Ace Diamonds, Card Ace Spades]
cards3 = Cards Refl Refl
-}

-- data SomeCard = forall rank suit. (RankVal rank, SuitVal suit) => SomeCard (Proxy rank) (Proxy suit)

-- someCard :: Rank -> Suit -> SomeCard
-- someCard r s = SomeCard (Proxy ::  Proxy
-- data R :: Rank -> * where
--           Mk :: Rank -> R 'Two
-- data Foo :: Rank -> * where
--    R :: 'Two -> Foo Int
-- data Foo :: Rank -> * where
--     R ::  -> Foo 'Two
{-
data RankList
    = RankNil
    | RankCons Rank RankList

-- rnil :: '[]
-- rnil =

data RList :: [Rank] -> *  where
    RNil :: RList '[]
    RCons :: Rank -> RList (t :: [Rank]) -> RList (a ': t)
-}


{-
rnil :: RList '[]
rnil = RNil

rtwo :: RList '[Two]
rtwo = RCons Two RNil

rtwothree :: RList '[Two, Three]
rtwothree = RCons Two (RCons Three RNil)

rtwothree' :: RList '[Three, Two]
rtwothree' = RCons Two (RCons Three RNil)
-}
{-
-- one :: RList '[Two, Three, Four]
-- one = (RCons Two (RCons Three (RCons Four RNil)))

rnil :: RList '[]
rnil = RNil

-- rone :: RList '[Two]
rone = RCons Two RNil
-- one = HCons Two (HCons Three HNil)
-- onetwothree :: RList (Two, ())
-- onetwothree = undefined
-- onetwothree = HCons Two HNil


-}
{-
-- data Deck :: * where
--   EmptyDeck :: Deck

deck :: Proxy '[Card Ace Spades, Card Two Spades]
deck = undefined

emptyDeck :: Proxy '[]
emptyDeck = Proxy

addCard :: (ToBool (Equal (Card rank suit) c) ~ False , NotMember (Card rank suit) cs ~ True) =>
           Card rank suit
        -> Proxy (c ': cs)
        -> Proxy (Card rank suit ': c ': cs)
addCard _ _ = Proxy
-}
{-
test1 = addCard aceOfSpades (Proxy :: Proxy '[Card Ace Diamonds, Card Two Diamonds])
-}
