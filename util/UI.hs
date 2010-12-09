module Euterpea.UI 
  ( -- Signal and Event functions
    Signal
  , EventS
  , constant,lift0    -- :: a -> Signal a
  , lift1             -- :: (a -> b) -> Signal a -> Signal b  
  , lift2             -- :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
  , lift3             -- :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
  , join              -- :: Signal a -> Signal b -> Signal (a, b)
  , split             -- :: Signal (a, b) -> (Signal a, Signal b) 
  , fstS, sndS        -- :: Signal (a, b) -> Signal a
  , initS, initS'     -- :: a -> Signal a -> Signal a
  , (>*), (<*)        -- :: (Ord a) => Signal a -> Signal a -> Signal Bool
  , (==*), (/=*)      -- :: Eq a => Signal a -> Signal a -> Signal Bool
  , (>=*),(<=*)       -- :: (Eq a, Ord a) => Signal a -> Signal a -> Signal Bool
  , (&&*), (||*)      -- :: Signal Bool -> Signal Bool -> Signal Bool
  , notS              -- :: Signal Bool -> Signal Bool
  , switch, untilS    -- :: Signal a -> EventS a -> Signal a
  , (=>>)             -- :: EventS a -> (a -> b) -> EventS b
  , (->>)             -- :: EventS a -> b -> EventS b
  , (.|.)             -- :: EventS a -> EventS a -> EventS a
  , unique            -- :: Eq a => Signal a -> EventS a
  , edge, when        -- :: Signal Bool -> EventS ()
  , snapshot          -- :: EventS a -> Signal b -> EventS (a, b)
  , snapshot_         -- :: EventS a -> Signal b -> EventS b
  , hold              -- :: a -> EventS a -> Signal a
  , integral          -- :: Signal Time -> Signal Double -> Signal Double
  , accum             -- :: a -> EventS (a -> a) -> Signal a 
  , accum'	      -- :: [[a]] -> EventS [a] -> Signal [[a]]
    -- UI functions
  , UI 
  , Dimension, Rect
  , topDown, bottomUp, leftRight, rightLeft    -- :: UI a -> UI a
  , setSize           -- :: Dimension -> UI a -> UI a
  , pad               -- :: (Int, Int, Int, Int) -> UI a -> UI a
  , runUI             -- :: String -> UI a -> IO ()
  , runUIEx           -- :: Dimension -> String -> UI a -> IO ()
    -- Widgets
  , label             -- :: String -> UI ()
  , display           -- :: Signal String -> UI ()
  , button            -- :: String -> UI (Signal Bool)
  , checkbox          -- :: String -> Bool -> UI (Signal Bool)
  , radio             -- :: [String] -> Int -> UI (Signal Int)
  , title             -- :: String -> UI a -> UI a
  , hSlider, vSlider  -- :: (RealFrac a) => (a, a) -> a -> UI (Singal a)
  , hiSlider, viSlider   -- :: (Integral a) => a -> (a, a) -> a -> UI (Signal a)
  , canvas            -- :: Dimension -> EventS Graphic -> UI ()
  , time              -- :: UI (Signal Time)
  , timer             -- :: Signal Time -> Signal Double -> EventS ()
  , delayt            -- :: Signal Time -> Signal Double -> EventS a -> EventS a
  , midiIn            -- :: Signal DeviceID -> UI (EventS [MidiMessage])
  , midiOut           -- :: Signal DeviceID -> EventS [MidiMessage] -> UI ()
  , playOut	      -- :: Signal DeviceID -> EventS Midi -> UI ()
  , selectInput, selectOutput    -- :: UI (Signal DeviceID)
  ) where

import Euterpea.UI.Signal
import Euterpea.UI.UIMonad
import Euterpea.UI.Widget
