-- 
import Data.List

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
            deriving (Eq, Ord, Bounded, Enum, Show, Read)

data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Eq, Enum, Show, Read)

data Card = Card Rank Suit deriving (Eq, Show, Read)

type Hand = [Card]

-- Returns True if all consequitive pairs satisfy f

allPairs f [] = True
allPairs f [x] = True
allPairs f (x:y:ys) = f x y && allPairs f (y:ys)

-- Returns true if a sorted hand contains n sets, eacg of size at most k 
setCount n k hand = let sets = groupBy (\(Card r1 _) (Card r2 _) -> r1 == r2) hand
                    in (lenght sets == n) && (maximum (map length sets) == k )

-- we can then define the hands as follow:
pair = setCounts 4 2 
twoPair = setCounts 3 2
threeOfAKind = setCount 3 3
FourOfAKind = setCount 2 4 
fullHouse = setCount 2 3
straight = allPairs (\(Card r1 _) (Card r2 _) -> r1 == pred r2)
flush = allPairs (\(Card _ s1) (Card _ s2) -> s1 == s2)
straightFlush hand = straight hand && flush hand
