-- | The Card module defines the basic data types for a card game, as well as a
-- shuffling function for lists.
module Card ( newDeck, shuffle
            , Card, HasValue(..)
            ) where


import Data.List ( sortBy )
import System.Random ( randoms
                     , RandomGen )


-- | An unsorted deck with all possible Cards
newDeck :: [Card]
newDeck = [ Card v s | v <- vs, s <- ss ]
  where
    vs = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack,
          Queen, King]
    ss = [Club, Diamond, Heart, Spade]


-- | Decorate xs with random ints, sort the decorated pairs, then undecorate.
shuffle :: RandomGen r => r -> [a] -> [a]
shuffle rgen xs = map snd (sortBy (\(x, _) (y, _) -> compare x y) zlist)
  where
    zlist = zip (randoms rgen :: [Int]) xs


class HasValue a where
    getVal :: a -> Int


data Suit = Club
          | Diamond
          | Spade
          | Heart
            deriving (Eq)

instance Show Suit where
    show Club    = "c"
    show Diamond = "d"
    show Spade   = "s"
    show Heart   = "h"


data Value = Ace
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
           | Ten
           | Jack
           | Queen
           | King
             deriving (Eq)

instance Show Value where
    show Ace   = "A"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "T"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"

instance HasValue Value where
    getVal Ace   = 1
    getVal Two   = 2
    getVal Three = 3
    getVal Four  = 4
    getVal Five  = 5
    getVal Six   = 6
    getVal Seven = 7
    getVal Eight = 8
    getVal Nine  = 9
    getVal Ten   = 10
    getVal Jack  = 10
    getVal Queen = 10
    getVal King  = 10


data Card = Card Value Suit
            deriving (Eq)

instance Show Card where
    show (Card v s) = show v ++ show s

instance HasValue Card where
    getVal (Card v _) = getVal v
