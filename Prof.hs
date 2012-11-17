module Main where

import GameOfLife



main =
    do
      let createBoard 0       b = b
          createBoard (n + 1) b = createBoard n (((n*8, n*8, n*4) `translateBrd` (head creatures)) `joinBoards` b) 
      let nb = updateBoard (createBoard 25 emptyBoard) 2
      putStrLn (show nb)
      return ()
    where updateBoard :: Board -> Int -> Board
          updateBoard b 0 = b
          updateBoard b (n + 1) = 
              let (_,_,_,b') = update_life b 
              in updateBoard b' n