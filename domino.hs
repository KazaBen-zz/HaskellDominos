module Domino where
  type Domino = (Int, Int)
  type Hand = [Domino]
  type Board = [Domino]
  data End = L | R
    deriving Eq

  domino1 :: Domino
  domino1 = (3,66)
  
  dominoHasNumber :: Domino->Int -> Bool
  dominoHasNumber domino number
    | fst domino == number = True 
    | snd domino == number = True
    | otherwise = False
    
  dominosHasSameDots :: Domino -> Domino -> Bool
  dominosHasSameDots d1 d2
    | dominoHasNumber d2 (fst d1) = True
    | dominoHasNumber d2 (snd d1) = True
    | otherwise = False
  
  goesP :: Domino -> Board -> End -> Bool
  goesP domino board end
    | null board = error "Board is empty"
    | dominosHasSameDots (head board) domino && end == L = True
    | dominosHasSameDots (last board) domino && end == R = True
    | otherwise = False
  
  getEndDomino :: Board -> End -> Domino
  getEndDomino board end
    | end == R = last board
    | end == L = head board   
  
  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True -- I need this because eventually board tail is empty(when knocking) but is it ok?
  knockingP (h:t) board
    | dominosHasSameDots h (getEndDomino board R) = False
    | dominosHasSameDots h (getEndDomino board R) = False
    | otherwise = knockingP t board
    
  playedP :: Domino -> Board -> Bool
  playedP _ [] = False
  playedP domino (h:t)
    | domino == h = True
    | otherwise = playedP domino t
    
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  possPlays hand board = (a hand board L, a hand board R)


  a :: Hand -> Board -> End -> [Domino]
  a [] _ _ = []
  a hand board end
    | goesP (head hand) board end = [head hand] ++ a (tail hand) board end
    | otherwise = a (tail hand) board end
  -- Tests
  
  domino15 :: Domino
  domino15 = (1,5)
  
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
  
  board1 :: Board
  board1 = [domino15, domino25, domino53]
  
  board2 :: Board
  board2 = [domino00, domino61, domino32, domino25, domino15]

  board3 :: Board
  board3 = [domino00, domino25, domino32]
  
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
  test9 = goesP domino15 board1 endR -- Expected result: True
  test10 = goesP domino15 board1 endL -- Expected result: True
  test11 = goesP domino61 board1 endL -- Expected result: True
  test12 = goesP domino32 board1 endR -- Expected result: True
  test14 = goesP domino32 board2 endL -- Expected result: False
  test15 = goesP domino00 board2 endR -- Expected result: False
  
  -- Tests for knockingP
  test16 = knockingP hand1 board2 -- Expected result: False
  test17 = knockingP hand1 board3 -- Expected result: False
  test18 = knockingP hand2 board2 -- Expected result: False
  test19 = knockingP hand2 board2 -- Expected result: False
  test20 = knockingP hand2 board1 -- Expected result: True
  test21 = knockingP hand3 board1 -- Expected result: True
  test22 = knockingP hand3 board2 -- Expected result: True
  test23 = knockingP hand3 board3 -- Expected result: True

  -- Tests for playedP
  test24 = playedP domino15 board1 -- Expected result: True
  test25 = playedP domino25 board1 -- Expected result: True
  test26 = playedP domino53 board1 -- Expected result: True
  test27 = playedP domino44 board1 -- Expected result: False
  test28 = playedP domino32 board1 -- Expected result: False
  
  -- Tests for possPlays
  test29 = possPlays hand1 board1 -- Expected result: ([(1,5),(5,3)],[(1,5),(5,3)])
  test30 = possPlays hand2 board1 -- Expected result: ([(6,1)],[])