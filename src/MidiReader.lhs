> module MidiReader where
> import Euterpea
> import qualified Codec.Midi as Midi
> import Data.List

Girish Sastry
2010-12-04

Module to read Midi files into Midi 'objects' in Euterpea. Heavily based on 
research code by Donya Quick and adapted for the Euterpea-MidiRecorder project.


The following MidiEvent data type is a simple version of Midi where events 
are only represented by their NoteOn/NoteOff values.

> data NoteEvent= On
>		| Off
>   deriving (Eq, Show, Ord)

> type STime = Rational
> type InstNum = Int

> type MidiEvent = (STime, AbsPitch, Volume, InstNum, NoteEvent)

Codec.Midi's importFile function attaches ticks to a message, where each # of
ticks is the number of ticks passed since the last message. The following 
function converts these relative ticks to absolute ticks:

> addTrackTicks :: Int -> [(Midi.Ticks, a)] -> [(Midi.Ticks, a)]
> addTrackTicks sum [] = []
> addTrackTicks sum ((t,x):ts) = (t+sum,x) : addTrackTicks (t+sum) ts

The next function applies a given Time Division. In other words, it allows
us to easily convert between ticks and a Music value's Dur.

> applyTD :: Midi.TimeDiv -> MidiEvent -> MidiEvent
> applyTD (Midi.TicksPerBeat td) (t,p,v,i,e) =
>   let t' = t / (fromIntegral td*4)
>   in (t',p,v,i,e)
> applyTD (Midi.TicksPerSecond fps tpf) (t,p,v,i,e) =
>   let t' = t / fromIntegral (fps*tpf)
>   in (t',p,v,i,e)

midiToEvents goes from a Midi structure (i.e. from Codec.Midi.importFile) to a
list of MidiEvents.

> midiToEvents :: Midi.Midi -> [[MidiEvent]]
> midiToEvents m = 
>     let ts = map (addTrackTicks 0) (Midi.tracks m) -- obtains exact times
>         td = Midi.timeDiv m
>         ts' = map (simplifyTrack 0) ts  
>     in  map (map (applyTD td)) ts' where 
>   simplifyTrack :: Int -> [(Midi.Ticks, Midi.Message)] -> [MidiEvent]
>   simplifyTrack icur [] = []
>   simplifyTrack icur ((t,m):ts) = 
>     case m of (Midi.NoteOn c p v) -> 
>                 (fromIntegral t, p, v, icur, On) : 
>                    simplifyTrack icur ts
>               (Midi.NoteOff c p v) -> 
>                   (fromIntegral t, p, v, icur, Off) : 
>                    simplifyTrack icur ts
>               (Midi.ProgramChange c p) -> simplifyTrack p ts 
>               _ -> simplifyTrack icur ts 

