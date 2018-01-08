#! /usr/bin/env runhaskell

{- Copyright 2010-2018  Dino Morelli <dino@ui3.info>
   Released under ISC

   Simple curses aquarium written in Haskell to learn about
   the hscurses library.
   Number of fish can be given as an argument, defaults to 20

   This code lives here:
   http://ui3.info/darcs/hscurses-fish-ex/

   Inspired by this C code:
   http://alexeyt.freeshell.org/code/aquarium.c
-}

import Control.Arrow ( first )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.MVar
   ( MVar, newMVar, putMVar, readMVar, takeMVar )
import Control.Monad ( liftM, mplus, replicateM, zipWithM_ )
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


data FishType = SmallFish | WideFish | HighFish | BigFish
  deriving (Bounded, Enum)

data Fish = Fish
   Int   -- y
   Int   -- x
   Pair  -- color
   Bool  -- fish is facing right
   FishType


{- Give the ascii art lines and their horizontal offsets for a fish type.
-}
fishGfx :: FishType -> ([(Int, String)], [(Int, String)])
fishGfx SmallFish = (,) [(0, " ><>")] [(0, "<>< ")]
fishGfx WideFish = (,) [(0, " ><{{{*>")] [(0, "<*}}}>< ")]
fishGfx HighFish = (,)
   [(2,   " _"),
    (0, " ><_>")]
   [(1,  "_ "),
    (0, "<_>< ")]
fishGfx BigFish = (,)
   [(7,        " \\:."),
    (0, " \\;,   ,;\\\\\\,,"),
    (1,  " \\\\\\;;:::::::o"),
    (1,  " ///;;::::::::<"),
    (1,  " /;' \"'/////''"),
    (8,          " /;'")]
   [(5,      ".:/ "),
    (2,   ",,///;,   ,;/ "),
    (1,  "o:::::::;;/// "),
    (0, ">::::::::;;\\\\\\ "),
    (1,  "''\\\\\\\\\\'\" ';\\ "),
    (4,     "';\\ ")]


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


-- Helper for spawn and swim. Compute length for a given fish type.
fishTypeLen :: FishType -> Int
fishTypeLen = maximum . map (length . snd) . fst . fishGfx


{- Construct a fish on a random row, facing a random direction
   Sets the starting column to be just offscreen
-}
spawn :: IO Fish
spawn = do
   (maxRows, maxCols) <- scrSize

   y <- randomRIO (0, maxRows - 1)
   right <- randomRIO (True, False)
   fishType <- liftM toEnum $ 
      randomRIO (0, fromEnum (maxBound :: FishType))
   let x = case right of
         True  -> negate (fishTypeLen fishType)
         False -> maxCols
   color <- liftM Pair $ randomRIO (1, 7)

   return $ Fish y x color right fishType


{- Returns a copy of a fish moved to a random column onscreen
   Used for first-time fish initialization
-}
randomXPos :: Fish -> IO Fish
randomXPos (Fish y _ color right t) = do
   maxCols <- liftM getWidth scrSize
   newX <- randomRIO (0, maxCols - 1)
   return $ Fish y newX color right t


{- Move a fish forward. If it's fully offscreen, spawn a random new 
   fish to replace it.
-}
swim :: Fish -> IO Fish

-- Right-facing fish
swim (Fish y x color right@True fishType) = do
   let modFish = Fish y (x + 1) color right fishType

   maxCols <- liftM getWidth scrSize
   if x > maxCols - 2 + fishTypeLen fishType
      then spawn
      else return modFish

-- Left-facing fish
swim (Fish y x color right@False fishType) = do
   let modFish = Fish y (x - 1) color right fishType

   if x < 1 - fishTypeLen fishType
      then spawn
      else return modFish


{- Draw one line of a fish
   The reason for this complicated draw-each-char is that mvWAddStr
   crashes if part of the string is past the bottom right corner of 
   the window.
   Not so with calling mvAddCh even with out-of-bounds coords.
-}
drawFishLine :: Int -> (Int, String) -> IO ()
drawFishLine y (x, s) = 
   zipWithM_ (mvAddCh y) [x..] $ map (fromIntegral . ord) s

drawFish :: Fish -> IO ()
drawFish (Fish y x color swimsRight t) = do
   attrSet attr0 color
   zipWithM_ drawFishLine [y..] . map (first (+ x)) .
     (if swimsRight then fst else snd) $ fishGfx t
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
