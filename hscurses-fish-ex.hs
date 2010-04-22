#! /usr/bin/env runhaskell

{- Copyright 2010  Dino Morelli <dino@ui3.info>
   Released under BSD3

   Simple curses aquarium written in Haskell to illustrate using 
   the hscurses library.
   Number of fish can be given as an argument, defaults to 20

   This code lives here:
   http://ui3.info/d/????

   Inspired by this C code:
   http://alexeyt.freeshell.org/code/aquarium.c
-}

import Control.Concurrent ( threadDelay )
import Control.Concurrent.MVar
   ( MVar, newMVar, putMVar, readMVar, takeMVar )
import Control.Monad ( liftM, mplus, replicateM )
import Data.Char ( ord )
import Data.Maybe ( fromJust )
import Safe ( headMay, readMay )
import System.Environment ( getArgs )
import System.Posix.Signals
   ( Handler (Catch), installHandler, sigINT, sigTERM )
import System.Random ( randomRIO )
import UI.HSCurses.Curses
   ( Pair (..), attr0, attrSet, endWin, initCurses, initPair, mvAddCh
   , mvWAddStr, refresh, scrSize, startColor, stdScr, wMove
   )
import UI.HSCurses.CursesHelper
   ( black, blue, cyan, green, magenta, red, white, yellow )
import UI.HSCurses.Widgets ( getWidth )


{- The data needed to track each fish
-}
data Fish = Fish
   Int   -- y
   Int   -- x
   Pair  -- color
   Bool  -- fish is facing right


interruptHandler :: MVar Bool -> IO ()
interruptHandler mvRunStatus = do
   -- We don't care what it is, we just want to take it from everyone else
   _ <- takeMVar mvRunStatus

   -- Make note in the state that the user wants to quit
   putMVar mvRunStatus True


worker :: MVar Bool -> [Fish] -> IO ()
worker mvRunStatus priorSchool = do
   -- Update fish positions
   school <- mapM swim priorSchool

   -- Draw the fish
   mapM_ drawFish school
   refresh
   
   threadDelay 100000

   -- Pull the run state..
   stopNow <- readMVar mvRunStatus

   -- examine it to figure out what to do next
   if stopNow == True
      then return ()
      else worker mvRunStatus school


{- Construct a fish on a random row, facing a random direction
   Sets the starting column to be just offscreen
-}
spawn :: IO Fish
spawn = do
   (maxRows, maxCols) <- scrSize

   y <- randomRIO (0, maxRows - 1)
   right <- randomRIO (True, False)
   let x = case right of
         True  -> -1
         False -> maxCols
   color <- liftM Pair $ randomRIO (1, 7)

   return $ Fish y x color right


{- Returns a copy of a fish moved to a random column onscreen
   Used for first-time fish initialization
-}
randomXPos :: Fish -> IO Fish
randomXPos (Fish y _ color right) = do
   maxCols <- liftM getWidth scrSize
   newX <- randomRIO (0, maxCols - 1)
   return $ Fish y newX color right


{- Move a fish forward. If it's fully offscreen, spawn a random new 
   fish to replace it.
-}
swim :: Fish -> IO Fish

-- Right-facing fish
swim (Fish y x color right@True) = do
   let modFish = Fish y (x + 1) color right

   maxCols <- liftM getWidth scrSize
   if x > (maxCols + 2)
      then spawn
      else return modFish

-- Left-facing fish
swim (Fish y x color right@False) = do
   let modFish = Fish y (x - 1) color right

   if x < -3
      then spawn
      else return modFish


{- Draw a fish
   The reason for this complicated draw-each-char is that mvWAddStr
   crashes if part of the string is past the bottom right corner of 
   the window.
   Not so with calling mvAddCh even with out-of-bounds coords.
-}
drawPart :: Int -> (Int, Char) -> IO ()
drawPart y (x, c) = do
   mvAddCh y x $ fromIntegral . ord $ c


drawFish :: Fish -> IO ()

-- Right-facing fish
drawFish (Fish y x color True) = do
   attrSet attr0 color
   mapM_ (drawPart y) $ zip [(x - 3) .. x] " ><>"
   wMove stdScr 0 0

-- Left-facing fish
drawFish (Fish y x color False) = do
   attrSet attr0 color
   mapM_ (drawPart y) $ zip [x .. (x + 3)] "<>< "
   wMove stdScr 0 0


{- Clear the screen by drawing spaces with the given color's background
-}
paintEntireScr :: Pair -> IO ()
paintEntireScr color = do
   (maxRows, maxCols) <- scrSize

   let blanks = replicate (maxCols - 1) ' '

   attrSet attr0 color
   mapM_ (\y -> mvWAddStr stdScr y 0 blanks) [0 .. (maxRows - 1)]
   refresh


main :: IO ()
main = do
   -- Get number of fish from the command-line, or use a default value
   args <- getArgs
   let nfish = fromJust $ (return args >>= headMay >>= readMay)
         `mplus` return 20

   initCurses

   -- Color initialization
   startColor
   initPair (Pair 1) green black
   initPair (Pair 2) red black
   initPair (Pair 3) yellow black
   initPair (Pair 4) blue black
   initPair (Pair 5) magenta black
   initPair (Pair 6) cyan black
   initPair (Pair 7) white black

   paintEntireScr $ Pair 7

   -- Randomly generate the first school of fish
   school <- mapM randomXPos =<< replicateM nfish spawn

   -- Data for communicating between threads
   mvRunStatus <- newMVar False

   -- A map to install the same handler for multiple signals
   mapM_ ( \signal -> installHandler signal 
      (Catch $ interruptHandler mvRunStatus) Nothing ) [sigINT, sigTERM]

   -- This starts the endless work loop
   worker mvRunStatus school

   paintEntireScr $ Pair 7

   endWin
