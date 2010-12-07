> module RecMUI where
> import Euterpea
> import Euterpea.MUI
> import Euterpea.UI
> import qualified Codec.Midi as Midi
> import MidiReader
> import Data.List

Functionality
~~~~~~~~~~~~~
RecMUI is a basic MUI that provides the following features:
(1) Recording/saving keyboard Midi input
(2) Playback of midi input
(3) Transpositions
(4) A quantizer that aligns to nearest 32nd note to account for human error
(5) Metronome

Notes
~~~~~
Convert to Music a --> record == testMidi

Tests
~~~~~

> testA :: Music Pitch
> testA = note (1/4) (A,4)

RecMUI
~~~~~~

> accum' :: [[a]] -> EventS [a] -> Signal [[a]]
> accum' i (Signal e) = Signal (i : f' i e)

> f' :: [[a]] -> [Maybe [a]] -> [[[a]]]
> f' i (x:xs) = case x of 
>   Just y -> i : (f' (y:i) xs)
>   Nothing -> i : f' i xs

> recUI :: UI ()
> recUI = title "Midi Recorder" $ topDown $
>   do	mo <- selectOutput
>	mi <- selectInput	-- Select midi input (keyboard)
>	m <- midiIn mi		--    keyboard midi input
>	rec <- button "RECORD"
>	ap <- title "Absolute Pitch" $ hiSlider 1 (0,100) 0
>	title "Pitch" $ displaySig $ pitchS ap

> recMUI = runUIEx (300,300) "Midi Recorder" recUI
