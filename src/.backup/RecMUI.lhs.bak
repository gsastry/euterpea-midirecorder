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

> pegS :: Signal (a, [b]) -> Signal [(a,b)]
> pegS = lift1 peg

> toMessageS :: Signal (Time, MidiMessage) -> Signal (Midi.Ticks, Midi.Message)
> toMessageS = lift1 toMessage

> toMessage :: (Time, MidiMessage) -> (Midi.Ticks, Midi.Message)
> toMessage (t,m) = 
>   case m of
>	Std m -> (fromEnum t `div` 2, m)
>	_ -> error "ANote when expected Std NoteOn/NoteOff"

> lstToMsgS :: Signal [(Time, MidiMessage)] -> Signal [(Midi.Ticks,Midi.Message)]
> lstToMsgS = lift1 (map toMessage)
	    
> metroFilter :: ([MidiMessage], Bool) -> [MidiMessage]
> metroFilter (ms@(m:_),i) =
>   if i then ms else []

> encase :: a -> [a]
> encase l = [l]

> encaseS :: Signal a -> Signal [a]
> encaseS = lift1 encase

> simplifyTrackS :: Signal [(Midi.Ticks, Midi.Message)] -> Signal [SEvent]
> simplifyTrackS = lift1 (simplifyTrack 0)

> eventsToMusicS :: Signal [[SEvent]] -> Signal (Music (Pitch, Volume))
> eventsToMusicS = lift1 eventsToMusic

> recUI :: UI ()
> recUI = leftRight $
>   do	(mi,mo) <- topDown $
>	    do  mi <- selectInput
>	        mo <- selectOutput
>	        return (mi,mo)
>	m <- midiIn mi		--    keyboard midi input
>	t <- time
>	(rec,sav,pb) <- topDown $
>	    do	rec <- button "RECORD"
>		sav <- button "SAVE AS MIDI"
>		pb <- button "PLAYBACK"
>		return (rec,sav,pb)
>	(c,f) <- topDown $
>	    do	c <- checkbox "Metronome On/Off" True
>		f <- title "Frequency" $ withDisplay $ hSlider (1,10) 1
>		return (c,f)
>	let mss = accum' [[]] m	-- Signal [[MidiMessage]]
>	    mts = lstToMsgS (distributeS $ pegS $ join t mss)
>	    sts = encaseS $ simplifyTrackS mts  -- Signal [[SEvent]]
>	    musics = eventsToMusicS sts		-- Signal (Music (P,V))
>	    -- ns3 = snapshot_ (edge pb) musics =>> 
>	    mticks = timer t (1.0/f)
>	    aptick = lift0 57	    -- tick at (A,4)
>	    ns1 = snapshot_ mticks aptick =>> \k -> [ANote 0 k 100 0.1]
>	    ns2 = snapshot ns1 c =>> metroFilter
>	midiOut mo ns2

> recMUI = runUIEx (800,200) "Midi Recorder" recUI
