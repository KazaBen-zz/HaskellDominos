module Domino where
  type Domino = (Int, Int)
  type Hand = [Domino]
  type Board = [Domino]
  
  data End = L | R -- Naming?
    deriving Eq
    
  data Maybe a = Nothing | Just a -- Is naming a ok?
    deriving Show -- Is only Show enough or should i do Eq, Ord, Read, Show as in notes(not neccessary in this assignment)
    
  swapDomino :: Domino -> Domino
  swapDomino dom = (snd dom, fst dom)
  
  dominoHasNumber :: Domino->Int -> Bool
  dominoHasNumber domino number
    |fst domino == number = True 
    |snd domino == number = True
    |otherwise = False
    
  dominosHasSameDots :: Domino -> Domino -> Bool
  dominosHasSameDots boardDomino handDomino
    |otherwise = False
  
  goesP :: Domino -> Board -> End -> Bool
  goesP handDomino board end
    |null board = error "Board is empty"
    |end == L && (fst (head board) == fst handDomino || fst (head board) == snd handDomino) = True -- True if domino can be placed on LEFT end
    |end == R && (snd (last board) == fst handDomino || snd (last board) == snd handDomino) = True -- True if domino can be placed on RIGHT end
    |otherwise = False
  
  getEndDomino :: Board -> End -> Domino
  getEndDomino board end
    |end == R = last board
    |end == L = head board   
      
  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True -- I need this because eventually board tail is empty(when knocking) but is it ok?
  knockingP (h:t) board
    |goesP h board R = False
    |goesP h board L = False
    |otherwise = knockingP t board

  playedP :: Domino -> Board -> Bool
  playedP _ [] = False
  playedP domino (h:t)
    |domino == h = True
    |swapDomino domino == h = True
    |otherwise = playedP domino t
    
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  possPlays hand board = (a hand board L, a hand board R)


  a :: Hand -> Board -> End -> [Domino]
  a [] _ _ = []
  a hand board end
    |goesP (head hand) board end = [head hand] ++ a (tail hand) board end
    |otherwise = a (tail hand) board end
    
  playDom :: Domino -> Board -> End -> Domino.Maybe Board -- Is Domino.Maybe ok?
  playDom domino board end
    |end == L && goesP domino board end = Domino.Just ((checkSwap domino board end) : board)
    |end == R && goesP domino board end = Domino.Just (board ++ [(checkSwap domino board end)])
    |otherwise = Domino.Nothing
    
  checkSwap :: Domino -> Board -> End -> Domino
  checkSwap domino board end
    |end == L && snd domino == fst (head board) = domino
    |end == R && fst domino == snd (last board) = domino
    |otherwise = swapDomino domino
    
  scoreBoard :: Board -> Int
  scoreBoard [] = 0
  scoreBoard board = b 3 board + b 5 board
  
  b :: Int -> Board -> Int
  b num board
    |mod sum num == 0 = sum `div` num
    |otherwise = 0
    where sum =  addDoubleDominos (head board) L + addDoubleDominos (last board) R

  addDoubleDominos :: Domino -> End -> Int
  addDoubleDominos domino end
    |fst domino == snd domino = fst domino + snd domino
    |end == L = fst domino
    |end == R = snd domino
  
  scoreN :: Board -> Int -> ([Domino], [Domino])
  scoreN board int
    | scoreBoard (playDom domino board R) == int
  
  -- Tests (IS IT OK TO USE NOT REALISTIC BOARDS?)
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
  
  board1 :: Board
  board1 = [domino15, domino25, domino53]
  
  board2 :: Board
  board2 = [domino00, domino61, domino32, domino25, domino15]

  board3 :: Board
  board3 = [domino00, domino25, domino32]
  
  board4 :: Board
  board4 = []
  
  board5 :: Board
  board5 = [domino15, domino45]
  
  board6 :: Board
  board6 = [domino15]
  
  board7 :: Board
  board7 = [domino66, domino00, domino02, domino23]
  
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
  hand4 = [domino15, domino00, domino61, domino32, domino44, domino51, domino53, domino02]

  -- Tests for dominoHasNumber
  test1 = dominoHasNumber domino15 1 -- Expected result: True
  test2 = dominoHasNumber domino15 5 -- Expected result: True
  test3 = dominoHasNumber domino15 7 -- Expected result: False
  
  -- Tests for dominosHasSameDots
  test4 = dominosHasSameDots domino15 domino15 -- Expected result: True
  test5 = dominosHasSameDots domino25 domino15 -- Expected result: True
  test6 = dominosHasSameDots domino32 domino25 -- Expected result: True
  test7 = dominosHasSameDots domino15 domino61 -- Expected result: True
  test8 = dominosHasSameDots domino00 domino32 -- Expected result: False
  
  -- Tests for goesP
  test9 = goesP domino15 board1 endR -- Expected result: False
  test10 = goesP domino15 board1 endL -- Expected result: True
  test11 = goesP domino61 board1 endL -- Expected result: True
  test12 = goesP domino32 board1 endR -- Expected result: True
  test14 = goesP domino32 board2 endL -- Expected result: False
  test15 = goesP domino00 board2 endR -- Expected result: False
  
  -- Tests for knockingP
  test16 = knockingP hand1 board2 -- Expected result: False
  test17 = knockingP hand1 board3 -- Expected result: True
  test18 = knockingP hand2 board2 -- Expected result: False
  test19 = knockingP hand2 board3 -- Expected result: False
  test20 = knockingP hand2 board1 -- Expected result: False
  test21 = knockingP hand3 board1 -- Expected result: True
  test22 = knockingP hand3 board2 -- Expected result: True
  test23 = knockingP hand3 board3 -- Expected result: True

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