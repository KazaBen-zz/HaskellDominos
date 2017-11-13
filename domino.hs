module Domino where
  import Data.Maybe
  import Data.List
  import System.Random
  import MergeSort
  import Debug.Trace

  -- All possible dominoes
  allDominos :: [Domino]
  allDominos = [(0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (2,2), (2,3), (2,4), (2,5), (2,6), (3,3), (3,4), (3,5), (3,6), (4,4), (4,5), (4,6), (5,5), (5,6), (6,6)]
  
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
    |mod sum num == 0 = sum `div` num -- If sum modulus given number is 0 then returning sum divided by num
    |otherwise = 0 -- Otherwise returning 0
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
  

  -- Assignment 2
  
  type DomsPlayer = Hand -> Board -> Maybe (Domino,End)
    
  simplePlayer :: DomsPlayer
  simplePlayer [] _ = Nothing
  simplePlayer (h:t) board 
    | playDom h board L /= Nothing = Just (h, L)
    | playDom h board R /= Nothing = Just (h, R)
    | otherwise = simplePlayer t board

  hsdPlayer :: DomsPlayer
  hsdPlayer [] _ = Nothing
  hsdPlayer hand@(h:t) board 
    | playDom h board R /= Nothing && scoreBoard(fromJust(playDom h board R)) == maxScore = Just (h, R)
    | playDom h board L /= Nothing && scoreBoard(fromJust(playDom h board L)) == maxScore = Just (h, L)
    | otherwise = hsdPlayer t board
    where maxScore = getMaxScore (createScores hand board L ) (createScores hand board R)
  
  createScores :: Hand -> Board -> End -> [Int]
  createScores [] _ _ = []
  createScores hand@(h:t) board end
    | playDom h board end == Nothing = [-1] ++  createScores t board end
    | otherwise = [scoreBoard(fromJust(playDom h board end))] ++ createScores t board end
  
  getMaxScore :: [Int] -> [Int] -> Int
  getMaxScore l1 l2 = maximum (l1 ++ l2)
  
  -- Shuffles all dominos
  shuffleDoms :: Int -> Hand
  shuffleDoms int = map fst (mergesort (\(_,n1) (_,n2) -> n1<n2)(zip allDominos (take 28 (randoms(mkStdGen int) :: [Int]))))
  
  playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
  playDomsRound p1 p2 seed = trace("Hand1: " ++ show hand1 ++ " Hand2: " ++ show hand2)(playARound p1 p2 [] hand1 hand2, playARound p2 p1 (makeAMove p1 [] hand1) hand2 hand1)
    where hand1 = makeAHand 1 seed
          hand2 = makeAHand 2 seed
        
  -- Plays a round of dominos and returns the score scored by first player
  playARound :: DomsPlayer -> DomsPlayer -> Board -> Hand -> Hand -> Int
  playARound _ _ _ [] _ = 0
  playARound p1 p2 boardd h1 h2
    | p1 h1 boardd == Nothing && p2 h2 boardd == Nothing = trace ("Both") 0
    | p1 h1 boardd == Nothing = trace ("Player 1 cant put anything.Board :" ++ show boardd) 0 + snd (makeAMoveAndCalcScore p1 boardd h1) + playARound p1 p2 (makeAMove p2 (makeAMove p1 boardd h1) h2) h1 h2
    | otherwise = trace ("Putting " ++ show (fromJust (p1 h1 boardd))++" Board: " ++ show (makeAMove p1 boardd h1)) snd (makeAMoveAndCalcScore p1 boardd h1) + playARound p1 p2 (makeAMove p2 (makeAMove p1 boardd h1) h2) h1 h2
    
  --Makes a move and returns a board
  makeAMove :: DomsPlayer -> Board -> Hand -> Board
  makeAMove player board hand 
    | player hand board == Nothing = board
    | otherwise = fromJust newBoard
    where newBoard = playDom (fst (fromJust (player hand board))) board (snd (fromJust (player hand board)))
  
  makeAMoveAndCalcScore :: DomsPlayer -> Board -> Hand -> (Board, Int)
  makeAMoveAndCalcScore player board hand = (newBoard, scoreBoard newBoard)
    where newBoard = makeAMove player board hand
    
  -- Makes a hand for player 1 or player 2.
  makeAHand :: Int -> Int -> Hand
  makeAHand num seed
    | num == 1 = take 7 doms
    | num == 2 = take 7 (reverse doms)
    where doms = shuffleDoms seed
  
  -- If a person doesnt put any dominos will he get points for the board???
  -- Scoreboard (3,3) gives result 4 rather than 2 :(
  -- Can i use Maybe at DomsPlayer???
  
  simplePlayer_Test0 = simplePlayer [(0,3), (2,4), (3,4), (6,2)] [(3,5), (5,4)] -- Result: Just ((0,3),L). Testing left end.
  simplePlayer_Test1 = simplePlayer [(0,5), (2,4), (3,4), (6,2)] [(3,5), (5,4)] -- Result: Just ((2,4),R). Testing right end.
  simplePlayer_Test2 = simplePlayer [(0,5), (2,4), (3,4), (6,2)] [] -- Result: Just ((0,5),L). Testing empty board.
  simplePlayer_Test3 = simplePlayer [] [(3,5), (5,4)] -- Result: Nothing. Testing empty hand.
  simplePlayer_Test4 = simplePlayer [] [] -- Result: Nothing. Testing empty hand, empty board.
  simplePlayer_Test5 = simplePlayer [(0,5), (0,6)] [(0,0)] -- Result: Just ((0,5),L). Testing if first domino will be placed(both can be).
  
  hsdPlayer_Test0 = hsdPlayer [(0,3), (2,4), (3,4), (6,2)] [(3,5), (5,4)] -- Result: Just ((3,4),R)).Testing RIGHT end(Scores 2 points) 
  hsdPlayer_Test1 = hsdPlayer [(0,3), (3,1)] [(3,6), (6,4)] -- Result: Just ((3,1),L). Testing LEFT end.
  hsdPlayer_Test2 = hsdPlayer [(0,5), (2,4), (6,6), (6,2)] [] -- Result: Just ((6,6),R). Testing empty board.
  hsdPlayer_Test3 = hsdPlayer [] [(3,5), (5,4)] -- Result: Nothing. Testing empty hand.
  hsdPlayer_Test4 = hsdPlayer [] [] -- Result: Nothing. Testing empty hand, empty board.
  hsdPlayer_Test5 = hsdPlayer [(0,5), (0,6)] [(0,0)] -- Result: Just ((0,6),R). Testing if most scored domino will be placed(both can be).
  
  

