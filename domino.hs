module Domino where
  import Data.Maybe

  type Domino = (Int, Int)
  type Hand = [Domino]
  type Board = [Domino]
  
  data End = L | R -- Naming?
    deriving Eq
        
  
  swapDomino :: Domino -> Domino
  swapDomino dom = (snd dom, fst dom)
  
  goesP :: Domino -> Board -> End -> Bool
  goesP _ [] _ = True
  goesP handDomino board end
    |end == L && (fst (head board) == fst handDomino || fst (head board) == snd handDomino) = True -- True if domino can be placed on LEFT end
    |end == R && (snd (last board) == fst handDomino || snd (last board) == snd handDomino) = True -- True if domino can be placed on RIGHT end
    |otherwise = False
      
  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True 
  knockingP (h:t) board
    |goesP h board R || goesP h board L = False
    |otherwise = knockingP t board

  playedP :: Domino -> Board -> Bool
  playedP _ [] = False
  playedP domino (h:t)
    |domino == h || swapDomino domino == h = True
    |otherwise = playedP domino t
    
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  possPlays hand board = (possPlaysEnd hand board L, possPlaysEnd hand board R)


  possPlaysEnd :: Hand -> Board -> End -> [Domino] -- Is it ok to have helper function?
  possPlaysEnd [] _ _ = []
  possPlaysEnd hand board end
    |goesP (head hand) board end = [head hand] ++ possPlaysEnd (tail hand) board end
    |otherwise = possPlaysEnd (tail hand) board end
    
  playDom :: Domino -> Board -> End -> Maybe Board -- Should I create my own Maybe?
  playDom domino board end
    |end == L && goesP domino board end = Just ((checkSwap domino board end) : board)
    |end == R && goesP domino board end = Just (board ++ [(checkSwap domino board end)])
    |otherwise = Nothing
    
  checkSwap :: Domino -> Board -> End -> Domino
  checkSwap domino board end
    |end == L && snd domino == fst (head board) = domino
    |end == R && fst domino == snd (last board) = domino
    |otherwise = swapDomino domino
    
  scoreBoard :: Board -> Int
  scoreBoard [] = 0
  scoreBoard board = calculateScore 3 board + calculateScore 5 board
  
  calculateScore :: Int -> Board -> Int
  calculateScore num board
    |mod sum num == 0 = sum `div` num
    |otherwise = 0
    where sum =  addDoubleDominos (head board) L + addDoubleDominos (last board) R

  addDoubleDominos :: Domino -> End -> Int
  addDoubleDominos domino end
    |fst domino == snd domino = fst domino + snd domino
    |end == L = fst domino
    |end == R = snd domino
  
  scoreN :: Board -> Int -> ([Domino], [Domino])
  dominos :: [Domino]
  dominos = [(0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (2,2), (2,3), (2,4), (2,5), (2,6), (3,3), (3,4), (3,5), (3,6), (4,4), (4,5), (4,6), (5,5), (5,6), (6,6)]
  scoreN board int = (c board dominos int L, c board dominos int R)
    
  c :: Board -> [Domino] -> Int -> End -> [Domino]
  c _ [] _ _ = []
  c board dominos int end
    |playDom (head dominos) board end == Nothing = [] ++ c board (tail dominos) int end
    |scoreBoard (fromJust (playDom (head dominos) board end)) == int = [head dominos] ++ c board (tail dominos) int end
    |otherwise = [] ++ c board (tail dominos) int end
    
  -- Tests (IS IT OK TO USE NOT REALISTIC BOARDS?)
  domino12 :: Domino
  domino12 = (1,2)
  
  domino02 :: Domino
  domino02 = (0,2)
  
  domino23 :: Domino
  domino23 = (2,3)
  
  domino66 :: Domino
  domino66 = (6,6)
  
  domino15 :: Domino
  domino15 = (1,5)
  
  domino51 :: Domino
  domino51 = (5,1)
  
  domino25 :: Domino
  domino25 = (2,5)
  
  domino53 :: Domino
  domino53 = (5,3)

  domino32 :: Domino
  domino32 = (3,2)

  domino00 :: Domino
  domino00 = (0,0)
  
  domino61 :: Domino
  domino61 = (6,1)

  domino44 :: Domino
  domino44 = (4,4)
  
  domino45 :: Domino
  domino45 = (4,5)
  
  domino52 :: Domino
  domino52 = (5,2)
  
  domino24 :: Domino
  domino24 = (2,4)
  
  domino40 :: Domino
  domino40 = (4,0)
  
  board0 :: Board
  board0 = [domino52, domino24, domino44, domino40]
  
  board1 :: Board
  board1 = [domino15, domino52, domino24]
  
  board2 :: Board
  board2 = [domino00, domino02, domino23, domino32, domino24]

  board3 :: Board
  board3 = [domino00, domino02, domino25]
  
  board4 :: Board
  board4 = []
  
  board5 :: Board
  board5 = [domino15, domino52]
  
  board6 :: Board
  board6 = [domino15]
  
  board7 :: Board
  board7 = [domino66, domino61, domino15, domino53]
  
  endL :: End
  endL = L
  
  endR :: End
  endR = R
  
  hand1 :: Hand
  hand1 = [domino15, domino53]
  
  hand2 :: Hand
  hand2 = [domino00, domino61]
  
  hand3 :: Hand
  hand3 = [domino44]
  
  hand4 :: Hand
  hand4 = [domino15, domino61, domino44, domino51, domino53]
  
  hand5 :: Hand
  hand5 = []
  
  -- Tests for goesP
  test_goesP0 = goesP domino40 board1 endR -- Domino CAN be played RIGHT side. Expected result: True, Actual result: True
  test_goesP1 = goesP domino12 board1 endL -- Domino CAN be played LEFT side. Expected result: True, Actual result: True
  test_goesP2 = goesP domino12 board1 endR -- Domino CAN'T be played RIGHT side but can on LEFT. Expected result: False
  test_goesP3 = goesP domino45 board1 endL -- Domino CAN'T be played LEFT side but can on RIGHT. Expected result: False
  test_goesP4 = goesP domino32 board2 endL -- Domino CAN'T be played anywhere. Expected result: False, Actual result: False
  test_goesP5 = goesP domino32 board2 endR -- Domino CAN'T be played anywhere. Expected result: False, Actual result: False
  test_goesP6 = goesP domino40 board2 endR -- Domino CAN be played on BOTH sides. Expected result: True, Actual result: True
  test_goesP7 = goesP domino40 board2 endL -- Domino CAN be played on BOTH sides. Expected result: True, Actual result: True
  test_goesP8 = goesP domino02 board0 endR -- Domino CAN be played after swap. Expected result: True, Actual result: False
  test_goesP9 = goesP domino12 board4 endR -- Domino CAN be played because board is empty. Expected: True, Actual result: True
  test_goesP10 = goesP domino12 board4 endL -- Domino CAN be played because board is empty. Expected: True, Actual result: True

  -- Tests for knockingP
  test_knockingP0 = knockingP hand1 board0 -- Domino15 CAN be played on LEFT. Expected result: False, Actual result: False
  test_knockingP1 = knockingP hand2 board0 -- Domino00 CAN be played on RIGHT. Expected result: False, Actual result: False 
  test_knockingP2 = knockingP hand4 board2 -- Domino44 CAN be played on RIGHT. Expected result: False, Actual result: False 
  test_knockingP3 = knockingP hand3 board0 -- NO dominoes can be played on board. Expected result: True, Actual: True
  test_knockingP4 = knockingP hand5 board3 -- Empty hand. Expected result: True, Actual result: True
  
  -- Tests for playedP
  test24 = playedP domino15 board1 -- Expected result: True
  test25 = playedP domino25 board1 -- Expected result: True
  test26 = playedP domino53 board1 -- Expected result: True
  test27 = playedP domino44 board1 -- Expected result: False
  test28 = playedP domino32 board1 -- Expected result: False
  test28a = playedP domino51 board1 -- Expected result: True
  
  
  -- Tests for possPlays
  test29 = possPlays hand1 board1 -- Expected result: ([(1,5)],[(5,3)])
  test30 = possPlays hand2 board1 -- Expected result: ([(6,1)],[])
  test30a = possPlays hand2 board2 -- Expected result: ([(0,0)],[])
  test30b = possPlays hand4 board2 -- Expected result: ([(0,0),(0,2)],[(1,5),(5,1),(5,3)])
  
  -- Tests for playDom
  test31 = playDom domino15 board1 L -- Expected result: Just [(5,1),(1,5),(2,5),(5,3)]
  test32 = playDom domino15 board1 R -- Expected result: Nothing
  test33 = playDom domino00 board1 L -- Expected result: Nothing
  test34 = playDom domino44 board2 L -- Expected result: Nothing
  test35 = playDom domino44 board2 R -- Expected result: Nothing
  test36 = playDom domino32 board1 R -- Expected result: Just [(1,5),(2,5),(5,3),(3,2)]
  test37 = playDom domino32 board1 L -- Expected result: Nothing
  test37a = playDom domino23 board1 R -- Expected result: Just [(1,5),(2,5),(5,3),(3,2)]
  
  -- Tests for scoreBoard
  test38 = scoreBoard board1 -- Expected result: 0
  test39 = scoreBoard board2 -- Expected result: 1
  test40 = scoreBoard board3 -- Expected result: 0
  test41 = scoreBoard board4 -- Expected result: 0
  test42 = scoreBoard board5 -- Expected result: 2
  test43 = scoreBoard board6 -- Expected result: 2
  test44 = scoreBoard board7 -- Expected result: 8
  
  -- Tests for scoreN
  test45 = scoreN board1 3
  test46 = scoreN board0 2