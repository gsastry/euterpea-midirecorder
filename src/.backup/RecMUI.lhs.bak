> module RecMUI where
> import Euterpea
> import Euterpea.MUI
> import Euterpea.UI
> import qualified Codec.Midi as Midi
> import MidiReading
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

> takeS :: Signal Int -> Signal [a] -> Signal [a]
> takeS = lift2 take

> lengthS :: Signal [a] -> Signal Int
> lengthS = lift1 length

> zipS :: Signal [a] -> Signal [b] -> Signal [(a,b)]
> zipS = lift2 zip

> distributeS :: Signal [(a,[b])] -> Signal [(a,b)]
> distributeS = lift1 distribute

> distribute ::  [(a,[b])] -> [(a,b)]
> distribute [] = []
> distribute (x:xs) = peg x ++ distribute xs

> peg :: (a, [b]) -> [(a,b)]
> peg (y,[]) = []
> peg (y,(p:ps)) = (y,p) : peg (y,ps)

 toMessage :: (Time, MidiMessage) -> [(Ticks, Midi.Message)]
 toMessage (t,m) = 
   case m of
	Std m -> m
	ANote c k v d -> do
	    
> metroFilter :: ([MidiMessage], Bool) -> [MidiMessage]
> metroFilter (ms@(m:_),i) =
>   if i then ms else []

> recUI :: UI ()
> recUI = title "Midi Recorder" $ topDown $
>   do	mo <- selectOutput
>	mi <- selectInput	-- Select midi input (keyboard)
>	m <- midiIn mi		--    keyboard midi input
>	t <- time
>	rec <- button "RECORD"
>	c <- checkbox "Metronome On/Off" True
>	f <- title "Frequency" $ withDisplay $ hSlider (1,10) 1
>	let mss = accum' [[]] m	-- Signal [[MidiMessage]
>	-- ts = accum ...	-- Signal [Int]
>	    ts = takeS (lengthS mss) $ lift0 (repeat 1)
>	    mts = distributeS $ zipS ts mss
>	    mticks = timer t (1.0/f)
>	    aptick = lift0 57	    -- tick at (A,4)
>	    ns1 = snapshot_ mticks aptick =>> \k -> [ANote 0 k 100 0.1]
>	    ns2 = snapshot ns1 c =>> metroFilter
>	midiOut mo ns2

> recMUI = runUIEx (300,300) "Midi Recorder" recUI
