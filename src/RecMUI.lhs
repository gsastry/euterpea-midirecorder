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
(2) Playback of midi input (TBD)
(3) Transpositions (TBD)
(4) ScaleVolume/reverse/ musical options
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

Music transposition functions:

scaleVolume s m scales the volume of each note m by a factor of s

> scaleVolume :: Rational -> Music (Pitch, Volume) -> Music (Pitch, Volume)
> scaleVolume s m = mMap f m
>   where f (p,v) = (p, round (s * fromIntegral v))

revM goes here

insideOut inverts the role of serial and parallel composition in a Music value.

> insideOut :: Music a -> Music a
> insideOut = mFold (:=:) (:+:) Prim Modify

Euterpea.transpose goes here

The following are utility functions used in the MUI:

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

> musicToMidiS :: Signal (Music (Pitch, Volume)) -> Signal (Midi.Midi)
> musicToMidiS = lift1 testMidi

> pcs :: [(String, Int)]
> pcs = [("A", 0)]

> a440m :: Music Pitch
> a440m = Prim (Note qn (A,4))

> a440mS :: Signal (Music Pitch)
> a440mS = lift0 a440m

> recUI :: UI ()
> recUI = leftRight $
>   do	(mi,mo) <- topDown $
>	    do  mi <- selectInput
>	        mo <- selectOutput
>	        return (mi,mo)
>	m <- midiIn mi		--    keyboard midi input
>	t <- time
>	(sav,pb,x,y0,y1) <- topDown $
>	    do	sav <- button "SAVE AS MIDI"
>		pb <- button "PLAYBACK"
>		x <- button "Play"
>		y0 <- button "Reverse Music"
>		y1 <- button "Inside Out"
>		return (sav,pb,x,y0,y1)
>	(c,f,p) <- topDown $
>	    do	c <- checkbox "Metronome" True
>		f <- title "Frequency" $ withDisplay $ hSlider (1,10) 1
>		p <- title "PitchClass" $ leftRight $
>		    radio (fst $ unzip pcs) 0
>		return (c,f,p)
>	let mss = accum' [[]] m	-- Signal [[MidiMessage]]
>	    mts = lstToMsgS (distributeS $ pegS $ join t mss)
>	    sts = encaseS $ simplifyTrackS mts  -- Signal [[SEvent]]
>	    musics = eventsToMusicS sts		-- Signal (Music (P,V))
>	    midis = musicToMidiS musics		-- Signal (Midi.Midi)
>	    midis' = unique midis =>> (\mid -> mid) -- EventS (Midi.Midi)
>	    ns3 = snapshot_ (edge sav) midis =>> (\mid -> mid)
>	    midi_out = playOut mo ns3
>	    -------- Music operations -----------
>	    y0m = snapshot_ (edge y0) musics =>> (\m -> testMidi $ revM m)
>	    y1m = snapshot_ (edge y1) musics =>> (\m -> testMidi $ insideOut m)
>	    midi_out_m = playOut mo (y0m .|. y1m)
>	    --------RadioButton functions -------
>	    rs = snapshot_ (edge x) a440mS =>> \p -> musicToMidiS
>	    rs1 = unique midis =>> (\mid -> mid)
>	    radio_out = playOut mo rs1
>	    ------- Metronome functions ----------
>	    mticks = timer t (1.0/f)
>	    aptick = lift0 57	    -- tick at (A,4)
>	    ns1 = snapshot_ mticks aptick =>> \k -> [ANote 0 k 100 0.1]
>	    ns2 = snapshot ns1 c =>> metroFilter
>	    metro_out = midiOut mo ns2
>	    midi_out_test = midiOut mo m
>	    --------------------------------------
>	metro_out >> midi_out >> midi_out_m >> radio_out

> recMUI = runUIEx (1000,200) "Midi Recorder" recUI
