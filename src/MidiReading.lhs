> module MidiReading where
> import Euterpea
> import Data.List
> import IO
> import Codec.Midi
> import Data.ByteString.Internal
> import Data.List
> import Data.Char

Modified by:
Girish Sastry
2010-12-07

Donya Quick
Code developed for research purposes and adapted for CPSC 431/531 to overcome some problems exhibited by fromMidi in FromMidi.lhs.
Last updated 02-Dec-2010.

This code has functions to read Midi values into an intermediate type before conversion to Music (Pitch, Volume). The following features will be retained from the input file:
- Placement of notes relative to the beat (assumed to be quarternotes).
- The pitch, volume, and instrument of each notes.

Other MIDI information is currently not supported. This includes controllers such as pitch bend as well as tempo changes. 

The following datatype is for a simplification of MIDI events into simple On/off events for pitches occurring at different times. The tuple contains the following information:
- exact onset time, Rational
- absolute pitch, AbsPitch
- volume from 0-127, Volume
- instrument number, Int
- on/off type, NEvent

> data NEvent = On | Off
>   deriving (Eq, Show, Ord)

> type SEvent = (Rational, AbsPitch, Volume, Int, NEvent)

The importFile function places track ticks (Ticks) in a format where each value attached to a message represents the number of ticks that have passed SINCE THE LAST MESSAGE. The following function will convert input in that format into a list of pairs where the ticks are absolute. In otherwords, ticks in the output will represent the exact point in time of an event. This means that unsupported events (e.g. pitch bend) can later be filtered out without affecting the timing of support events.

> addTrackTicks :: Int -> [(Ticks, a)] -> [(Ticks, a)]
> addTrackTicks sum [] = []
> addTrackTicks sum ((t,x):ts) = (t+sum,x) : addTrackTicks (t+sum) ts

The following function addresses a ticks to Music duration conversion.

> applyTD :: TimeDiv -> SEvent -> SEvent
> applyTD (TicksPerBeat td) (t,p,v,i,e)= 
>     let t' = t / (fromIntegral td * 4)
>     in  (t', p, v, i, e)
> applyTD (TicksPerSecond fps tpf) (t,p,v,i,e) = 
>     let t' = t / fromIntegral (fps * tpf)
>     in  (t', p, v, i, e)


The midiToEvents function will take a Midi structure (from importFile, for example) and convert it to a list of lists of SEvents. Each outer list represents a track in the original Midi. 

> midiToEvents :: Midi -> [[SEvent]]
> midiToEvents m = 
>     let ts = map (addTrackTicks 0) (tracks m) -- obtains exact times
>         td = timeDiv m
>         ts' = map (simplifyTrack 0) ts  
>     in  map (map (applyTD td)) ts' 

> simplifyTrack :: Int -> [(Ticks, Message)] -> [SEvent]
> simplifyTrack icur [] = []
> simplifyTrack icur ((t,m):ts) = 
>   case m of (NoteOn c p v) -> 
>               (fromIntegral t, p, v, icur, On) : 
>                  simplifyTrack icur ts
>             (NoteOff c p v) -> 
>                 (fromIntegral t, p, v, icur, Off) : 
>                  simplifyTrack icur ts
>             (ProgramChange c p) -> simplifyTrack p ts 
>             _ -> simplifyTrack icur ts 


The eventsToMusic function will convert a list of lists of SEvents (output from midiToEvents) to a Music(Pitch,Volume) structure. The structure may not be ideal from an analysis standpoint - all notes will be connected together using the (:=:) constructor. For example, the first line of "Frere Jaque" would actually get represented like this:

	(rest 0 :+: c 5 qn) :=:
      (rest qn :+: d 5 qn) :=:
      (rest hn :+: e 5 qn) :=:
      (rest dhn :+: c 5 qn)

The instrument will be set for each note individually. 

> eventsToMusic :: [[SEvent]] -> Music (Pitch, Volume)
> eventsToMusic = chord . map seToMusic where
>   seToMusic :: [SEvent] -> Music (Pitch, Volume)
>   seToMusic [] = rest 0
>   seToMusic (e1@(t,p,v,ins,On):es) = 
>     let fx (t1,p1,v1,ins1,e1) = (p1==p && ins1==ins) && e1==Off
>         is = findIndices fx es
>         i = is !! 0
>         (t1,p1,v1,ins1, e) = es !! i
>         n = rest t :+: note (t1-t) (pitch p,v)
>         n' = instrument (toEnum ins) n
>     in  if v > 0 then 
>              if length is > 0 then n' :=: seToMusic es
>              else seToMusic ((e1:es)++[correctOff e1 es])
>         else seToMusic es
>   seToMusic (t:ts) = seToMusic ts

This function is an error-handling method for MIDI files which have mismatched note on/off events. This seems to be common in output from some software. The solution used here is to assume that the note lasts until the the time of the last event in the list. 

> correctOff (t,p,v,ins,e) [] = (t,p,v,ins,Off)
> correctOff (t,p,v,ins,e) es = 
>     let (t1,p1,v1,ins1,e1) = last es
>     in  (t1,p,v,ins,Off) 


The midiToMusic function wraps the combination of midiToEvents and eventsToMusic.

> midiToMusic :: Midi -> Music (Pitch, Volume)
> midiToMusic m = 
>     let seList = midiToEvents m
>     in  eventsToMusic seList



