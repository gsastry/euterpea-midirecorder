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

Introduction
~~~~~~~~~~~~

Initially, I set out to create a backend/frontend for MUI that allows for 
conversion from midi input to a music value and vice versa. The motivation 
for this was that there are many useful functions that operate only at the 
music level -- in fact, most of the Euterpea library (and the textbook) deals
with this. However, there is no way to use this functions coherently with
midi input -- especially at the MUI/signal level.

The goals for this project have changed since it's conception. Because of
the complexity of the MUI/signal layer, I was unable to complete the 
transformation from Midi to Music and back. Specifically, going from Music
back to Midi values proved to be an especially challenging problem. The 
Music value that is written on the conversion from Midi is in one particular
structure of a parallel composition of sequential Music values. After any
musical transpositions (i.e. revM and insideOut), this structure may have
changed in unknown ways. Converting this back to Midi is the next step 
for this project.

Because of this roadblock, I focused on the initial conversion from Midi
to Music. It appears that this was successful; however, testing this proved
to be a challenge. I attempted to add widgets to the Widget.lhs file to 
facilitate writing the converted output to a file, using the exportFile
function provided by the Codec.Midi library.

Tutorial
~~~~~~~~

First, drop the util/Signal.lhs file in the Euterpea/UI directory and the 
util/Widget.lhs in the Euterpea/ directory (replacing the old versions).
Then, run recMUI after loading the module RecMUI. To disable the input from a
radio button (used for testing purposes) comment out the block of code within
recUI. Right now, when the button "Play" is pushed, an A note will be played.
The MUI defaults to using midi input. This MUI has been tested on 
Windows machines in the closed zoo and in the music lab. If you want to
modify the behavior of the playOut function, it is located in Widget.lhs.
If you want to modify the behavior of accum', it is located in Signal.lhs.
After either modification, you must run a "cabal install" command in the
Euterpea directory before loading the RecMUI module.

I included two examples of Music level transformations (revM and insideOut),
but it is very simple to implement your own (and this is one of the 
motivations for this project). Simply lift the function that operates
at the Music level for it to operate at the MUI level and incorporate it in
the recUI code.

Strategy/Implementation
~~~~~~~~~~~~~~~~~~~~~~~
A bar of music like this:

-------0---------
----0--0---------
------------0----
--------------0--
----------------0

Is converted into something like this in Euterpea:

		:=:
	       /  |
	      /   |
	     /    |
	     :+:  :+:	    ...
	    /  \    | \
	rest 0  A   |  B
		    rest qn

I've defined a series of functions that facilitate the pipeline that enables
a value to go from EventS [MidiMessage] -> Signal/EventS (Music (Pitch, Volume)).

These functions are commented below. In addition, with Donya, there are two
functions in the actual Euterpea library: playOut (Widget.lhs) and accum'
(Signal.lhs). I have copied the comments and type signatures for them, but
these also exist in those files in the util/ directory:

playOut is a function defined by Donya Quick and modified by Girish Sastry. It
mimics midiOut to allow a Haskell Midi "object" to be played at the UI () level.
Within it, we can utilize the IO () monad to include functions for I/O, such as
exportFile. In fact, we can modify exportFile to any IO () function to display
information that is occuring in the Midi stream.

 playOut :: Signal DeviceID -> EventS Midi -> UI ()

accum' is a version of accum that goes from [[a]] -> Signal [[a]], given
an EventS [a]. This is a key component of the pipeline to transform 
Midi input at the MUI level to Music values in Euterpea. It works by
appending in real time the proper lists in the Event Stream that exist
(are not Nothing). f' is a helper function that facilitates this.

 accum' :: [[a]] -> EventS [a] -> Signal [[a]]
 f' :: [[a]] -> [Maybe [a]] -> [[[a]]]

I utilize the MidiReading module provided by Donya Quick to do some conversions
between Midi and Music. Since the MidiReading module uses a datatype called
SEvent (which is a simplification of a MidiMessage), we need accum' to
get the MidiMessage input as a list of SEvents in order to continue with
the conversion.

Next, I use a variety of lifted functions to convert to a Music object
in Euterpea. These functions are viewable below with comments on what they
do. After this, there are musical transformations that can be used at 
the Signal level with the use of lift.

Observations/Conclusions
~~~~~~~~~~~~~~~~~~~~~~~~

The project turned out to be a lot more difficult than originally planned, 
which is OK, because this turns out to be an interesting research question 
for the Euterpea language. My results with the time given are spotty -- I can
write to a midi file, but the midi file is unreadable. My work has been largely
behind the scenes, and hopefully someone can extend this to a nice, usable 
implementation. Then, the possibilities are endless for Musical transformations
with just recorded Midi.

Over the course of the last two weeks, I've learned a lot about the interplay 
of Signals/EventS/Midi and the MUI layer. I've dug through the code in the actual
Euterpea library, seen the Midi code at the lowest level, seen the MUI code at
the lowest level, and the Signal code at the lowest level. Because of this, I
can also understand the design considerations that went into developing the 
suite of MUI tools.

The use of the MUI/Signal layer, which makes wiring up some graphical elements
very easy, does not seem well suited for this conversion. This became a much
harder problem than I (and Donya) thought. As such, I really look forward
to the Arrows implementation of the MUI functionality.

Tests
~~~~~

These functions are used for simple testing (and the implementation of the
test radio button in the MUI)

> testA :: Music Pitch
> testA = note (1/4) (A,4)

> pcs :: [(String, Int)]
> pcs = [("A", 0)]

> a440m :: Music Pitch
> a440m = Prim (Note qn (A,4))

> a440mS :: Signal (Music Pitch)
> a440mS = lift0 a440m

RecMUI
~~~~~~

Music transposition functions:

scaleVolume s m scales the volume of each note m by a factor of s. Taken from 
HW3.lhs.

> scaleVolume :: Rational -> Music (Pitch, Volume) -> Music (Pitch, Volume)
> scaleVolume s m = mMap f m
>   where f (p,v) = (p, round (s * fromIntegral v))

insideOut inverts the role of serial and parallel composition in a Music value. Taken
from HW3.lhs.

> insideOut :: Music a -> Music a
> insideOut = mFold (:=:) (:+:) Prim Modify


The following are utility functions used in the MUI. Most need to operate
at the Signal level, and so must be lifted.

> takeS :: Signal Int -> Signal [a] -> Signal [a]
> takeS = lift2 take

> lengthS :: Signal [a] -> Signal Int
> lengthS = lift1 length

> zipS :: Signal [a] -> Signal [b] -> Signal [(a,b)]
> zipS = lift2 zip

DistributeS is the lifted form of distribute. 

> distributeS :: Signal [(a,[b])] -> Signal [(a,b)]
> distributeS = lift1 distribute

distribute is a function that distributes the "a" type into the lists of "b" types
that are given in its arguments.

> distribute ::  [(a,[b])] -> [(a,b)]
> distribute [] = []
> distribute (x:xs) = peg x ++ distribute xs

peg operates on one tuple and pegs the first element to every element in the list 
in the tuple to form a list of tuples.

> peg :: (a, [b]) -> [(a,b)]
> peg (y,[]) = []
> peg (y,(p:ps)) = (y,p) : peg (y,ps)

pegS is the lifted form of peg.

> pegS :: Signal (a, [b]) -> Signal [(a,b)]
> pegS = lift1 peg

toMessageS is the lifted form of toMessage.

> toMessageS :: Signal (Time, MidiMessage) -> Signal (Midi.Ticks, Midi.Message)
> toMessageS = lift1 toMessage

toMessage converts from the MidiMessage type to a Midi.Message (on the way to 
converting to a Codec.Midi midi object)

> toMessage :: (Time, MidiMessage) -> (Midi.Ticks, Midi.Message)
> toMessage (t,m) = 
>   case m of
>	Std m -> (fromEnum t `div` 2, m)
>	_ -> error "ANote when expected Std NoteOn/NoteOff"

lstToMsgS operates at the signal level, converting the stream of midi signals
into a stream of Midi message objects by applying toMessage.

> lstToMsgS :: Signal [(Time, MidiMessage)] -> Signal [(Midi.Ticks,Midi.Message)]
> lstToMsgS = lift1 (map toMessage)

metroFilter controls when NoteOn/NoteOffs are sent. This is used for the metronome
checkbox in the MUI.
	    
> metroFilter :: ([MidiMessage], Bool) -> [MidiMessage]
> metroFilter (ms@(m:_),i) =
>   if i then ms else []

encase encases an element into a list.

> encase :: a -> [a]
> encase l = [l]

encaseS is the lifted form of encase.

> encaseS :: Signal a -> Signal [a]
> encaseS = lift1 encase

simplifyTrackS is the lifted version of simplifyTrack, which is defined in the
MidiReading module.

> simplifyTrackS :: Signal [(Midi.Ticks, Midi.Message)] -> Signal [SEvent]
> simplifyTrackS = lift1 (simplifyTrack 0)

eventToMusic is the lifted version of eventsToMusic, which is defined in the
MidiReading module.

> eventsToMusicS :: Signal [[SEvent]] -> Signal (Music (Pitch, Volume))
> eventsToMusicS = lift1 eventsToMusic

musicToMidiS converts a Music value to a Midi object at the signal level.

> musicToMidiS :: Signal (Music (Pitch, Volume)) -> Signal (Midi.Midi)
> musicToMidiS = lift1 testMidi

> recUI :: UI ()
> recUI = leftRight $
>   do	(mi,mo) <- topDown $
>	    do  mi <- selectInput
>	        mo <- selectOutput
>	        return (mi,mo)
>	m <- midiIn mi		--    keyboard midi input
>	t <- time
>	(sav,x,y0,y1) <- topDown $
>	    do	sav <- button "SAVE AS MIDI"
>		x <- button "Play single note"
>		y0 <- button "Reverse Music"
>		y1 <- button "Inside Out"
>		return (sav,x,y0,y1)
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
