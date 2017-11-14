module Domino where
  import Data.Maybe
  import Data.List
  import System.Random
  import MergeSort
  import Debug.Trace
  
 -- Assignment 2
  type DomsPlayer = Hand -> Board -> (Domino,End)
    
  simplePlayer :: DomsPlayer
  simplePlayer hand@(h:t) board 
    | playDom h board L /= Nothing = (h, L) -- If you can play head(hand) domino on LEFT then return it on LEFT
    | playDom h board R /= Nothing = (h, R) -- If you can play head(hand) domino on RIGHT then return it on RIGHT
    | otherwise = simplePlayer t board -- Recursive call on tail of the hand

  hsdPlayer :: DomsPlayer
  hsdPlayer hand@(h:t) board 
    | playDom h board R /= Nothing && scoreBoard(fromJust(playDom h board R)) == maxScore =  (h, R) -- If domino can be put on RIGHT and scores max then return it
    | playDom h board L /= Nothing && scoreBoard(fromJust(playDom h board L)) == maxScore =  (h, L) -- If domino can be put on LEFT and scores max then return it
    | otherwise = hsdPlayer t board -- Recursive call on tail of the hand
    where maxScore = getMaxScore (createScores hand board L ) (createScores hand board R) -- Calculates maximum possible score to be scored with the hand on both ends
    
  -- Creates a list of scores which can be scored by putting hand's dominoes to the given end
  createScores :: Hand -> Board -> End -> [Int]
  createScores [] _ _ = [] --Empty hand means empty score list(nothing can be scored)
  createScores hand@(h:t) board end
    | playDom h board end == Nothing = -1 : createScores t board end -- If domino CAN'T be played then put -1 to list and recursive call on tail
    | otherwise = scoreBoard(fromJust(playDom h board end)) : createScores t board end -- otherwise calculate score by putting it and recursive call on tail
  
  -- Gets maximum score of 2 lists
  getMaxScore :: [Int] -> [Int] -> Int
  getMaxScore l1 l2 = (maximum (l1 ++ l2)) -- Combines 2 lists and gets the maximum
  
  -- Calculates maximum value that can be scored with given hand on given board
  calculateMaxScore :: Hand -> Board -> Int
  calculateMaxScore hand board = getMaxScore (createScores hand board L)(createScores hand board L)

  -- Shuffles all dominos
  shuffleDoms :: Int -> [Domino]
  shuffleDoms int = map fst (mergesort (\(_,n1) (_,n2) -> n1<n2)(zip allDominos (take 28 (randoms(mkStdGen int) :: [Int]))))
  
  -- Plays a round of dominoes and returns the scores scored by both players
  playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
  playDomsRound p1 p2 seed = (playARound p1 p2 [] hand1 hand2, playARound p2 p1 (makeAMove p1 [] hand1) hand2 hand1) -- 2nd player starting board is board after 1st player's move
    where hand1 = makeAHand 1 seed -- Creates hand for 1st player
          hand2 = makeAHand 2 seed -- Creates hand for 2nd player
        
  -- Plays a round of dominos and returns the score scored by first player
  playARound :: DomsPlayer -> DomsPlayer -> Board -> Hand -> Hand -> Int
  playARound _ _ _ [] _ = 0 -- Empty first players hand means score is 0
  playARound p1 p2 board h1 h2
    | knockingP h1 board && knockingP h2 board = 0 -- Both players are knocking then score is 0
    | knockingP h1 board = 0 + playARound p1 p2 (makeAMove p2 (makeAMove p1 board h1) h2) h1 h2 -- If player 1 is knocking then move's score is 0 and recursive call
    | otherwise = snd (makeAMoveAndCalcScore p1 board h1) + playARound p1 p2 (makeAMove p2 (makeAMove p1 board h1) h2) h1 h2 -- add score and recursive call
    
  --Makes a move and returns a board
  makeAMove :: DomsPlayer -> Board -> Hand -> Board
  makeAMove player board hand 
    | knockingP hand board = board -- If player is knocking then keep the board the same
    | otherwise = fromJust newBoard -- Otherwise return new board
    where newBoard = playDom (fst  (player hand board)) board (snd (player hand board)) -- newBoard is a board after player move
  
  -- Makes a move and calculates score scored
  makeAMoveAndCalcScore :: DomsPlayer -> Board -> Hand -> (Board, Int)
  makeAMoveAndCalcScore player board hand = (newBoard, scoreBoard newBoard) -- Returning newBoard and score of newBoard
    where newBoard = makeAMove player board hand -- newBoard is a board after player move
    
  -- Makes a hand for player 1 or player 2.
  makeAHand :: Int -> Int -> Hand
  makeAHand num seed
    | num == 1 = take 7 doms -- Take first 7 dominoes of shuffled dominoes
    | num == 2 = take 7 (reverse doms) -- Take last 7 dominoes of shuffled dominoes
    where doms = shuffleDoms seed -- doms is all shuffled domineos
    
  simplePlayer_Test0 = simplePlayer [(0,3), (2,4), (3,4), (6,2)] [(3,5), (5,4)] -- Result: Just ((0,3),L). Testing left end.
  simplePlayer_Test1 = simplePlayer [(0,5), (2,4), (3,4), (6,2)] [(3,5), (5,4)] -- Result: Just ((2,4),R). Testing right end.
  simplePlayer_Test2 = simplePlayer [(0,5), (2,4), (3,4), (6,2)] [] -- Result: Just ((0,5),L). Testing empty board.
  simplePlayer_Test3 = simplePlayer [(0,5), (0,6)] [(0,0)] -- Result: Just ((0,5),L). Testing if first domino will be placed(both can be).
  
  hsdPlayer_Test0 = hsdPlayer [(0,3), (2,4), (3,4), (6,2)] [(3,5), (5,4)] -- Result: ((3,4),R). Testing right end(Scores 2 points) 
  hsdPlayer_Test1 = hsdPlayer [(4,1), (3,1)] [(3,6), (6,4)] -- Result: ((3,1),L). Testing left end.
  hsdPlayer_Test2 = hsdPlayer [(0,5), (2,4), (6,6), (6,2)] [] -- Result: ((6,6),R). Testing empty board.
  hsdPlayer_Test3 = hsdPlayer [(0,5), (0,6)] [(0,0)] -- Result: ((0,6),R).
  
  shuffleDoms_Test0 = shuffleDoms 0 -- Result: [(0,5),(2,3),(0,1),(3,6),(1,3),(3,4),(1,5),(2,5),(4,4),(3,5),(3,3),(5,5),(2,6),(0,3),(0,6),(1,4),(4,5),(6,6),(0,2),(1,1),(1,2),(5,6),(4,6),(0,4),(2,2),(2,4),(1,6),(0,0)]
  shuffleDoms_Test1 = shuffleDoms 1 -- Result: [(1,3),(1,1),(0,4),(4,5),(1,6),(6,6),(5,5),(0,6),(4,6),(0,5),(3,4),(4,4),(1,5),(0,1),(3,3),(0,2),(2,3),(1,4),(3,5),(2,4),(2,5),(1,2),(2,2),(0,0),(3,6),(2,6),(0,3),(5,6)]
  shuffleDoms_Test2 = shuffleDoms 2 -- Result: [(3,3),(0,3),(1,4),(2,6),(3,4),(0,1),(0,4),(3,5),(0,6),(2,3),(0,0),(4,6),(4,4),(1,6),(1,2),(4,5),(2,4),(1,3),(2,2),(1,5),(0,2),(0,5),(6,6),(1,1),(2,5),(5,5),(5,6),(3,6)]
  shuffleDoms_Test3 = length (shuffleDoms 0) -- Result: 28. Testing if all the dominos are shuffled and returned.

  playDomsRound_Test0 = playDomsRound simplePlayer hsdPlayer 0 -- Result: (1,15)
  {--
  How the game is played: 
    Hand1: [(0,5),(2,3),(0,1),(3,6),(1,3),(3,4),(1,5)] Hand2: [(0,0),(1,6),(2,4),(2,2),(0,4),(4,6),(5,6)]
    p1: Putting ((0,5),L) Board: [(0,5)].   Scores: (1,0)
    p2: Putting ((0,4),L) Board: [(4,0),(0,5)].   Scores: (1,3)
    p1: Putting ((3,4),L) Board: [(3,4),(4,0),(0,5)].   Scores: (1,3)
    p2: Putting ((5,6),R) Board: [(3,4),(4,0),(0,5),(5,6)].   Scores: (1,6) 
    p1: Putting ((2,3),L) Board: [(2,3),(3,4),(4,0),(0,5),(5,6)].   Scores: (1,6)
    p2: Putting ((2,4),L) Board: [(4,2),(2,3),(3,4),(4,0),(0,5),(5,6)].   Scores: (1,8)
    p1: Putting ((3,6),R) Board: [(4,2),(2,3),(3,4),(4,0),(0,5),(5,6),(6,3)].   Scores: (1,8)
    p2: Putting ((4,6),L) Board: [(6,4),(4,2),(2,3),(3,4),(4,0),(0,5),(5,6),(6,3)].   Scores: (1,11)
    p1: Putting ((1,3),R) Board: [(6,4),(4,2),(2,3),(3,4),(4,0),(0,5),(5,6),(6,3),(3,1)].   Scores: (1,11) 
    p2: Putting ((1,6),R) Board: [(6,4),(4,2),(2,3),(3,4),(4,0),(0,5),(5,6),(6,3),(3,1),(1,6)].   Scores: (1,15)
  --} 
  
  playDomsRound_Test1 = playDomsRound hsdPlayer simplePlayer 0 -- Result: (8,0). Testing swapped p1 and p2. Also testing turn skips(when knocking)
  {--
  How the game is played:
    Hand1: [(0,5),(2,3),(0,1),(3,6),(1,3),(3,4),(1,5)] Hand2: [(0,0),(1,6),(2,4),(2,2),(0,4),(4,6),(5,6)]
    p1: Putting ((3,6),R) Board: [(3,6)].   Scores: (3,0)
    p2: Putting ((1,6),R) Board: [(3,6),(6,1)].   Scores: (3,0)
    p1: Putting ((1,3),R) Board: [(3,6),(6,1),(1,3)]. Scores: (5,0)
    p2: CAN'T PUT ANYTHING(SKIPS). Scores (5,0)
    p1: Putting ((2,3),R) Board: [(3,6),(6,1),(1,3),(3,2)].   Scores: (6,0)
    p2: Putting ((2,4),R) Board: [(3,6),(6,1),(1,3),(3,2),(2,4)].   Scores: (6,0)
    p1: Putting ((3,4),R) Board: [(3,6),(6,1),(1,3),(3,2),(2,4),(4,3)].   Scores: (8, 0)
  --}
  
  playDomsRound_Test2 = playDomsRound simplePlayer hsdPlayer 2 -- Result: (11,9)
  {--
  How the game is played:
    Hand1: [(3,3),(0,3),(1,4),(2,6),(3,4),(0,1),(0,4)] Hand2: [(3,6),(5,6),(5,5),(2,5),(1,1),(6,6),(0,5)]
    p1: Putting ((3,3),L) Board: [(3,3)]. Scores (2,0)
    p2: Putting ((3,6),R) Board: [(3,3),(3,6)]. Scores (2,4)
    p1: Putting ((0,3),L) Board: [(0,3),(3,3),(3,6)]. Scores (4,4)
    p2: Putting ((6,6),R) Board: [(0,3),(3,3),(3,6),(6,6)]. Scores (4,8)
    p1: Putting ((2,6),R) Board: [(0,3),(3,3),(3,6),(6,6),(6,2)]. Scores (4,8)
    p2: Putting ((2,5),R) Board: [(0,3),(3,3),(3,6),(6,6),(6,2),(2,5)]. Scores (4,9)
    p1: Putting ((0,1),L) Board: [(1,0),(0,3),(3,3),(3,6),(6,6),(6,2),(2,5)] . Scores (6,9)
    p2: Putting ((5,6),R) Board: [(1,0),(0,3),(3,3),(3,6),(6,6),(6,2),(2,5),(5,6)] . Scores (6,9)
    p1: Putting ((1,4),L) Board: [(4,1),(1,0),(0,3),(3,3),(3,6),(6,6),(6,2),(2,5),(5,6)] . Scores (8,9)
    p2: CAN'T PUT ANYTHING(SKIPS). Scores (8,9)
    p1: Putting ((3,4),L) Board: [(3,4),(4,1),(1,0),(0,3),(3,3),(3,6),(6,6),(6,2),(2,5),(5,6)] . Scores (11,9)
  --}
  
  playDomsRound_Test3 = playDomsRound hsdPlayer simplePlayer 5 -- Result: (10,6)
  {--
  How the game is played:
    Hand1: [(2,6),(2,3),(2,4),(0,6),(5,6),(5,5),(1,1)] Hand2: [(2,5),(3,6),(1,2),(4,4),(0,0),(3,3),(4,6)]
    p1: Putting ((5,5),R) Board: [(5,5)]. Scores (2,0)
    p2: Putting ((2,5),L) Board: [(2,5),(5,5)]. Scores (2,4)
    p1: Putting ((2,6),L) Board: [(6,2),(2,5),(5,5)].  Scores (2,4)
    p2: Putting ((3,6),L) Board: [(3,6),(6,2),(2,5),(5,5)].  Scores (2,4)
    p1: Putting ((2,3),L) Board: [(2,3),(3,6),(6,2),(2,5),(5,5)].  Scores (6,4)
    p2: Putting ((1,2),L) Board: [(1,2),(2,3),(3,6),(6,2),(2,5),(5,5)].  Scores (6,4)
    p1: Putting ((1,1),L) Board: [(1,1),(1,2),(2,3),(3,6),(6,2),(2,5),(5,5)].  Scores (10,4).
    p2: CAN'T PUT ANYTHING(SKIPS). Scores (10,4)
    p1: Putting ((5,6),R) Board: [(1,1),(1,2),(2,3),(3,6),(6,2),(2,5),(5,5),(5,6)].  Scores (10,4)
    p2: Putting ((4,6),R) Board: [(1,1),(1,2),(2,3),(3,6),(6,2),(2,5),(5,5),(5,6),(6,4)].  Scores (10,6)
    p1: Putting ((2,4),R) Board: [(1,1),(1,2),(2,3),(3,6),(6,2),(2,5),(5,5),(5,6),(6,4),(4,2)].  Scores (10,6)
  --}
  
  
  
  
  
  
  
  
  
  -- Assignment 1
  
  
  
  -- All possible dominoes
  allDominos :: [Domino]
  allDominos = [(0,0),(0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (2,2), (2,3), (2,4), (2,5), (2,6), (3,3), (3,4), (3,5), (3,6), (4,4), (4,5), (4,6), (5,5), (5,6), (6,6)]
  
  -- Data structures
  type Domino = (Int, Int)
  type Hand = [Domino]
  type Board = [Domino]
  
  data End = L | R
    deriving (Eq, Show)
    
  -- Swaps domino's values
  swapDomino :: Domino -> Domino
  swapDomino dom = (snd dom, fst dom)
  
  goesP :: Domino -> Board -> End -> Bool
  goesP _ [] _ = True -- Empty board means any domino can be placed thus returning True
  goesP handDomino board end
    |playedP handDomino board = False -- If domino is arleady played then domino can't be played
    |end == L && (fst (head board) == fst handDomino || fst (head board) == snd handDomino) = True -- True if domino can be placed on LEFT end
    |end == R && (snd (last board) == fst handDomino || snd (last board) == snd handDomino) = True -- True if domino can be placed on RIGHT end
    |otherwise = False -- If domino can't be placed on left or right end then returning False
      
  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True -- Empty hand means can't place any domino
  knockingP (h : t) board
    |goesP h board R || goesP h board L = False -- If hand's first domino can be played on right or left then returning False
    |otherwise = knockingP t board -- Otherwise recursive call on tail of the hand

  playedP :: Domino -> Board -> Bool
  playedP _ [] = False -- If board is empty then no dominoes are played thus returning False
  playedP domino (h:t)
    |domino == h || swapDomino domino == h = True -- If domino or swapped domino is head of the board then True
    |otherwise = playedP domino t -- Otherwise recursive call on the tail of the board
    
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  possPlays hand board = (possPlaysEnd hand board L, possPlaysEnd hand board R) -- Calling helper function on left and right
  
  -- Returns all dominoes which can be placed on the end from your hand
  possPlaysEnd :: Hand -> Board -> End -> [Domino]
  possPlaysEnd [] _ _ = [] -- If hand is empty nothing can be placed thus returning empty list
  possPlaysEnd hand board end
    -- If head hand domino can go to board at given end then adding list of that domino to recursive call of same function with tail of the hand
    |goesP (head hand) board end = [head hand] ++ possPlaysEnd (tail hand) board end 
    |otherwise = possPlaysEnd (tail hand) board end -- Otherwise recursive call on the tail of the hand without adding the domino
    
  playDom :: Domino -> Board -> End -> Maybe Board
  playDom domino board end
    |null board = Just [domino] -- If board is empty then returning board with that domino
    |end == L && goesP domino board end = Just ((checkSwap domino board end) : board) -- If left end and domino can go to that end then adding domino(checks if domino needs to get values swapped) to the left of the board.
    |end == R && goesP domino board end = Just (board ++ [(checkSwap domino board end)]) -- If right end and domino can go to that end then adding domino(checks if domino needs to get values swapped) to the right of the board
    |otherwise = Nothing -- If domino cant go to any end then returning Nothing
    
  -- Checks if domino's values needs to be swapped to be legally placed on the end of the board
  checkSwap :: Domino -> Board -> End -> Domino
  checkSwap domino board end
    |end == L && snd domino == fst (head board) = domino -- Checks if domino can be legally placed without swaps to left
    |end == R && fst domino == snd (last board) = domino -- Checks if domino can be legally placed without swaps to right
    |otherwise = swapDomino domino -- If it can't be legally placed then swapping domino's values
    
  scoreBoard :: Board -> Int
  scoreBoard [] = 0 -- Score of empty board is 0
  scoreBoard board = calculateScore 3 board + calculateScore 5 board -- Adding calls helper function with 3 and 5 as parameters(3's and 5's game)
  
  -- Calculates score given divider
  calculateScore :: Int -> Board -> Int
  calculateScore num board
    | mod (sum `div` 2) num == 0 && length board == 1 = (sum `div` 2) `div` num
    | mod sum num == 0 = sum `div` num -- If sum modulus given number is 0 then returning sum divided by num
    | otherwise = 0 -- Otherwise returning 0
    where sum =  addDoubleDominos (head board) L + addDoubleDominos (last board) R -- sum is dominos sum of ends

  -- Adds double dominoes values e.g. when domino is (5,5) then returns 10 if not double then returns the outermost value of the domino
  addDoubleDominos :: Domino -> End -> Int
  addDoubleDominos domino end
    |fst domino == snd domino = fst domino + snd domino -- If both values are the same then returning sum of them
    |end == L = fst domino -- If end is left then returning first value of domino(leftmost)
    |end == R = snd domino -- If end is right then returning second value of domino(rightmost)
  
  
  scoreN :: Board -> Int -> ([Domino], [Domino])
  scoreN board int = (scoreNEnd board allDominos int L, scoreNEnd board allDominos int R) -- Calling helper function on both ends
    
  -- Returns all dominoes which can be played at the given end to get required score
  scoreNEnd :: Board -> [Domino] -> Int -> End -> [Domino]
  scoreNEnd _ [] _ _ = [] -- If there are no dominoes left to try then returning empty set
  scoreNEnd board dominos int end
    |playDom (head dominos) board end == Nothing = [] ++ scoreNEnd board (tail dominos) int end -- If domino can't go to any end then returning empty list plus recursive call on the tail of dominos
    |scoreBoard (fromJust (playDom (head dominos) board end)) == int = [head dominos] ++ scoreNEnd board (tail dominos) int end -- If score required can be get by playing domino at given end then adding that domino to recursive call on the tail of dominos
    |otherwise = [] ++ scoreNEnd board (tail dominos) int end -- Otherwise returning empty set plus recursivve call on tail of dominos