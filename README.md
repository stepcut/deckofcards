tjakway
asks\](https://www.reddit.com/r/haskell/comments/3r8x5m/best\_way\_to\_model\_a\_deck\_of\_cards/),
"\[What is the\] best way to model a deck of cards?"

This is an excellent question. For the sake of simplicity, let's make a
few simplifications:

    1. Aces high
    2. No jokers
    3. Single deck

A deck of cards has several important properties:

    1. Each card is a combination of a rank and suit
    2. There are 13 ranks and 4 suits
    3. Each card appears only once in the deck
    4. There are 52 cards in the deck
    5. All combinations of rank and suit are valid

Clearly the 'best' model is going to enforce these properties.

It is clear that some properties can be derived from the others. For
example, in order for 2, 3, and 4 to all be true, 5 must be true.

In order to enforce these properties, we need to make sure they are
represented at the type level so that the type checker can enforce the
properties at compile time.

First we are going to enable a giant mess of LANGUAGE pragmas:

``` {.sourceCode .literate .haskell}
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
```

Next we have some imports. We do not depend on anything that is not in
`base`.

``` {.sourceCode .literate .haskell}
import Data.Proxy (Proxy(..))
import Data.Monoid ((<>))
import Data.Type.Equality ((:~:)(Refl),type (==))
import GHC.TypeLits
```

Next we will define a simple data-type to represent the `Rank` of a
card:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  Rank
------------------------------------------------------------------------

-- | card ranks, with Aces high
data Rank
    = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace
   deriving (Eq, Ord, Read, Show, Enum)
```

We automatically derive `Ord` and `Enum` instances which assume Aces
high. A `newtype` could be used to change the sort order if needed,
though other changes would be required.

One of the `LANGUAGE` pragmas we enabled above is `DataKinds`. As
normal, the above `data` declaration creates a type construction and a
bunch of data constructors:

    Rank :: *   -- type 'Rank' with kind '*'
    Two :: Rank -- value 'Two' with type 'Rank'
    Three :: Rank
    etc

But because of the `DataKinds` extension we also get a bunch of
additional type constructors and a new kind:

    Rank          -- the kind `Rank`
    Two :: Rank   -- type 'Two' with kind 'Rank'
    Three :: Rank -- type 'Three' with kind 'Rank'

There are some occasions where the newly created types can result in
ambigous type errors, so there are also aliases which prefix the types
with a single quote:

    'Two :: Rank   -- type 'Two' with kind 'Rank'
    'Three :: Rank -- type 'Three' with kind 'Rank'

Sometimes we want to convert a `type`, like `Two` to the data value of
the same name. We can do that by creating a type class:

``` {.sourceCode .literate .haskell}
class RankVal (rank :: Rank) where
   rankVal :: Proxy rank -> Rank
```

Note that the `kind` of `rank` is `Rank` not `*`. That means we can only
create instances for the `Rank` types:

``` {.sourceCode .literate .haskell}
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
```

If we tried to do:

``` {.haskell}
instance RankVal Int   where rankVal _ = Ace
```

We would get the error:

        The first argument of ‘RankVal’ should have kind ‘Rank’,
          but ‘Int’ has kind ‘*’
        In the instance declaration for ‘RankVal Int’
    Failed, modules loaded: none.

We can use the `rankVal` function to create a helper function for pretty
printing the rank:

``` {.sourceCode .literate .haskell}
-- | show the `rank` as a `String`
showRank :: (RankVal rank) => Proxy rank -> String
showRank p = show (rankVal p)
```

For our convenience we will create a type alias that lists all the ranks
in asecending order:

``` {.sourceCode .literate .haskell}
-- | all the card ranks
type Ranks = '[ Two, Three, Four, Five, Six, Seven, Eight, Nine
              , Ten, Jack, Queen, King, Ace]
```

Note that the list is `'[]` instead of the typical `[]`. That is because
we are using the promoted list value that `DataKinds` introduces.

Next we have a little helper function which gives us a `Proxy` value
with all the `Ranks`.

``` {.sourceCode .literate .haskell}
-- | all the card ranks as a proxy value
ranks :: Proxy Ranks
ranks = Proxy
```

Next we do the exact same for the `Suits`

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  Suit
------------------------------------------------------------------------

-- | Suit - sorted low to high
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
```

Now that we have `Rank` and `Suit` types we can create a `Card`:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  Card
------------------------------------------------------------------------

-- | a 'Card' has a 'Rank' and a 'Suit'
data Card :: Rank -> Suit -> * where
   MkCard :: Card rank suit

instance Show (Card rank suit) where
    show MkCard = "MkCard"
```

`MkCard` is a nullary constructor. It does not take any arguments -- the
`Rank` and `Suit` appear only in the type-level.

Note that because the kind is `Rank -> Suit -> *` and not `* -> * -> *`
types like `Card Int Char` are not valid.

If we have `Rank` and `Suit` `Proxy` values we can use this helper
function to construct the `Card`:

``` {.sourceCode .literate .haskell}
-- | helper function for building a 'Card' from 'Proxy' values
mkCard :: Proxy (rank :: Rank) -> Proxy (suit :: Suit) -> Card rank suit
mkCard _ _ = MkCard
```

The following function uses the `showRank` and `showSuit` functions from
above to pretty print the `Card`.

``` {.sourceCode .literate .haskell}
-- | pretty print the 'Card'
showCard :: forall rank suit. (RankVal rank, SuitVal suit) => Card rank suit -> String
showCard _ = showRank (Proxy :: Proxy rank) <> " of " <> showSuit (Proxy :: Proxy suit)
```

Now we can create an example card:

``` {.sourceCode .literate .haskell}
-- | example card
aceOfSpades :: Card Ace Spades
aceOfSpades = MkCard
```

If we evaluate the card in GHCi we get back what we put in:

    *Main> aceOfSpades
    MkCard
    *Main> :t aceOfSpades
    aceOfSpades :: Card 'Ace 'Spades

The value is just `MkCard` and the type is `Card Ace Spades`. Note that
GHCi used the type names prefix with a single quote for clarity.

Next we need a little bit of Boolean logic to enforce our properties.
First we declare some type-level logic types:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  type level boolean functions
------------------------------------------------------------------------

data Equal x y
data Not x
data And x y
data Or x y
```

These types have no data constructors, so we can't create any values of
those types, but we can use them as arguments to type functions.

Let's create a type family which evaluates Boolean expressions and
returns `True` or `False`.

``` {.sourceCode .literate .haskell}
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
```

Note that these are all type-level calculations, so we could have used
`'True` and `'False` for clarity.

Next we are going to create a type-level function which calculates the
length of a type-level list:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  type level length
------------------------------------------------------------------------

type family Length (list :: [k]) where
   Length '[] = 0
   Length (c ': cs) = 1 + Length cs
```

Although we are operating at the type level, our `Length` function looks
very similar to the classic value-level length definition:

``` {.haskell}
length :: [a] -> Int
length [] = 0
length (c:cs) = 1 + length cs
```

Along similar lines, we can create a type-level `IsElem` function:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  type level isElem
------------------------------------------------------------------------

type family IsElem (c :: k) (cs :: [k]) where
   IsElem c '[] = False
   IsElem c (c' ': cs) = ToBool (Or (Equal c c') (IsElem c cs))
```

We can use the `IsElem` and Boolean logic functions to test if all the
elements of a type-level list are unique or not.

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  type level: check if all elements in a type level list are unique
------------------------------------------------------------------------

type family IsUnique (list :: [k]) where
   IsUnique '[] = True
   IsUnique (c ': cs) = ToBool (And (Not (IsElem c cs)) (IsUnique cs))
```

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  type level: check if all elements in the list are cards
------------------------------------------------------------------------
```

``` {.sourceCode .literate .haskell}
type family IsCards (list :: [*]) where
    IsCards '[] = True
    IsCards (Card r s ': cs) = IsCards cs
    IsCards k = False
```

Now we have all the pieces required to create a set of cards:

``` {.sourceCode .literate .haskell}

------------------------------------------------------------------------
--  Cards
------------------------------------------------------------------------

data Cards :: Nat -> [*] -> * where
    -- ensure that there are a specific number of a cards and that each card appears only once
   Cards :: ((IsCards cards) :~: True) -> (Length cards :~: (n :: Nat))
         -> (IsUnique cards :~: True) -> Cards n cards


instance Show (Cards (n :: Nat) (cs :: [*])) where
    show (Cards Refl Refl Refl) = "Cards Refl Refl Refl"
```

The `Cards` type constructor takes two parameters, the first is the
number of cards in the list, and the second is the list of unique cards.

The `Cards` data constructor takes three values. The first value is the
proof that the list only contains cards. The second value is the proof
that the length of the list is equal to the number of cards we are
supposed to have. The third value is proof that the cards in the list
are unique.

We can create a little helper function for generating a specific set of
cards:

``` {.sourceCode .literate .haskell}
-- | helper function to make 'Cards'
mkCards :: (IsCards cards ~ True, Length cards ~ (n :: Nat), IsUnique cards ~ True) =>
           Cards n cards
mkCards = Cards Refl Refl Refl
```

We can also create a helper function which adds a new cards to an
existing set of cards:

``` {.sourceCode .literate .haskell}
-- | add a card to a set of cards
--
-- card must not already be in the deck
addCard :: ((1 + n) ~ (1 + Length cards), IsUnique (Card rank suit ': cards) ~ True
           , IsCards cards ~ True) =>
           Card rank suit
        -> Cards n cards
        -> Cards (1 + n) (Card rank suit ': cards)
addCard c cs = mkCards
```

So let's look at this in action now:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  example cards
------------------------------------------------------------------------

-- | an empty set of cards
cards0 :: Cards 0 '[]
cards0 = Cards Refl Refl Refl

-- | just the ace of spaces
cards1 :: Cards 1 '[Card Ace Spades]
cards1 = Cards Refl Refl Refl

-- | the ace of spaces and the ace of diamonds
cards2 :: Cards 2 '[Card Ace Spades, Card Ace Diamonds]
cards2 = mkCards -- use mkCards for variety sake
```

Note that the value of all these cards are the same, only the types are
different.

If we try to stick a non-Card value in the list:

``` {.haskell}
-- | the ace of spaces and the ace of diamonds
cards3 :: Cards 2 '[Card Ace Spades, Card Ace Diamonds, Int]
cards3 = mkCards -- use mkCards for variety sake
```

we will get the error:

        Couldn't match type ‘'False’ with ‘'True’
        Expected type: 'True
          Actual type: IsCards
                         '[Card 'Ace 'Spades, Card 'Ace 'Diamonds, Int]
        In the expression: mkCards
        In an equation for ‘cards3’: cards3 = mkCards

Sometimes we might actually want a value. Perhaps we need to store the
cards in a database or serialize them to JSON or something else. Or
maybe we just want to through type safety out the window. We can define
a simple card type as follows:

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  SimpleCard
------------------------------------------------------------------------

-- | A card type which does not have its 'Rank' and 'Suit' in the type level
data SimpleCard = SimpleCard Rank Suit
   deriving (Eq, Ord, Read, Show)
```

Here `Rank` and `Suit` are acting like normal Haskell types.

We can easily convert a `Card` to a `SampleCard` using our `rankVal` and
`suitVal` functions:

``` {.sourceCode .literate .haskell}
-- | convert a 'Card' to a 'SimpleCard'
toSimpleCard :: forall rank suit. (RankVal rank, SuitVal suit) =>
                Card rank suit
             -> SimpleCard
toSimpleCard _ =
    SimpleCard (rankVal (Proxy :: Proxy rank)) (suitVal (Proxy :: Proxy suit))
```

Of course, we also want to convert a `Cards` to `[SimpleCard]`.

``` {.sourceCode .literate .haskell}
------------------------------------------------------------------------
--  SimpleCards
------------------------------------------------------------------------

-- | a list of cards
type SimpleCards = [SimpleCard]

```

To do the conversion, we will use a simple type class with two
instances. One for the base case, and one for the recursive case.

The class definition is pretty straight-forward:

``` {.sourceCode .literate .haskell}
-- | convert a list of 'Cards' to '[SimpleCard]'
class ToSimpleCards a where
    toSimpleCards :: a -> SimpleCards
```

Our base case looks like:

``` {.sourceCode .literate .haskell}
instance ToSimpleCards (Cards 0 '[]) where
   toSimpleCards _ = []
```

And the recursive case:

``` {.sourceCode .literate .haskell}
instance forall rank suit n cs. (RankVal rank, SuitVal suit, IsCards cs ~ True
                                , IsUnique cs ~ True
                                , Length cs ~ (n - 1)
                                , ToSimpleCards (Cards (n - 1) cs)) =>
    ToSimpleCards (Cards n ((Card rank suit) ': cs)) where
   toSimpleCards _ = toSimpleCard (MkCard :: Card rank suit)
                   : toSimpleCards (mkCards :: Cards (n - 1) cs)
```

While we have a lot more noise, we can see our classic recursive
pattern. If we were dealing with normal values we would have:

``` {.haskell}
toSimpleCards :: [Card] -> [SimpleCard]
toSimpleCards [] = []
toSimpleCards (c:cs) = (toSimpleCard c) : (toSimpleCards cs)
```

Now let's create a deck of cards. Writing out all 52 cards by hand would
be tedious, so lets take the cartesian product instead. First we need an
type-level `Append` function:

``` {.sourceCode .literate .haskell}
type family Append (a :: [k]) (b :: [k]) where
    Append '[] bs = bs
    Append (a ': as) bs = a ': (Append as bs)
```

And now we can create our cartesian product generator:

``` {.sourceCode .literate .haskell}
type family GenCards (r :: [Rank]) (s :: [Suit]) where
    GenCards r '[] = '[]
    GenCards '[] s = '[]
    GenCards (r ': rr) (s ': ss) =
        Append ((Card r s) ': GenCards (r ': '[]) ss) (GenCards rr (s ': ss))
```

And for our grand finally, the deck of cards:

``` {.sourceCode .literate .haskell}
deckOfCards :: Cards 52 (GenCards Ranks Suits)
deckOfCards = mkCards
```

If we load it up into GHCi we can inspect the value and the type:

``` {.haskell}
*Main> deckOfCards
Cards Refl Refl
*Main> :t deckOfCards
deckOfCards
  :: Cards
       52
       '[Card 'Two 'Clubs, Card 'Two 'Diamonds, Card 'Two 'Hearts,
         Card 'Two 'Spades, Card 'Three 'Clubs, Card 'Three 'Diamonds,
         Card 'Three 'Hearts, Card 'Three 'Spades, Card 'Four 'Clubs,
         Card 'Four 'Diamonds, Card 'Four 'Hearts, Card 'Four 'Spades,
         Card 'Five 'Clubs, Card 'Five 'Diamonds, Card 'Five 'Hearts,
         Card 'Five 'Spades, Card 'Six 'Clubs, Card 'Six 'Diamonds,
         Card 'Six 'Hearts, Card 'Six 'Spades, Card 'Seven 'Clubs,
         Card 'Seven 'Diamonds, Card 'Seven 'Hearts, Card 'Seven 'Spades,
         Card 'Eight 'Clubs, Card 'Eight 'Diamonds, Card 'Eight 'Hearts,
         Card 'Eight 'Spades, Card 'Nine 'Clubs, Card 'Nine 'Diamonds,
         Card 'Nine 'Hearts, Card 'Nine 'Spades, Card 'Ten 'Clubs,
         Card 'Ten 'Diamonds, Card 'Ten 'Hearts, Card 'Ten 'Spades,
         Card 'Jack 'Clubs, Card 'Jack 'Diamonds, Card 'Jack 'Hearts,
         Card 'Jack 'Spades, Card 'Queen 'Clubs, Card 'Queen 'Diamonds,
         Card 'Queen 'Hearts, Card 'Queen 'Spades, Card 'King 'Clubs,
         Card 'King 'Diamonds, Card 'King 'Hearts, Card 'King 'Spades,
         Card 'Ace 'Clubs, Card 'Ace 'Diamonds, Card 'Ace 'Hearts,
         Card 'Ace 'Spades]
```

Using the `Cards` type we can now generate an entire deck of cards, or
some subset of the deck. Note that although we did not explicitly limit
the number of the cards in the deck to being 52 or less, we got that
'for free'. Because we insist on each card being unique, there is simply
no way to generate a 53rd card.

The implementation here is probably not the most efficient.
Additionally, I may have recreated some functions that already exist
elsewhere in `base` but have escaped my seacrhing. If you have
improvements, please submit a pull request.

And, believe it or not, this code could be more general. For example,
the `Cards` type is basically a vector of unique values. The first
argument to the `Cards` constructor forces the elements to be cards, but
that could be made for flexible.
