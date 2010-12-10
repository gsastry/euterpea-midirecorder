> module MidiDiagnostics where
> import Euterpea
> import Codec.Midi
> import MidiReading

Donya Quick
Code developed for research purposes and adapted for CPSC 431/531.
Last updated 02-Dec-2010.


Counts the number of NoteOn and NoteOff events read from a midi file. It is possible for some MIDI files to play correctly in some other program, but for no NoteOff values to be recognized in Euterpea. The problem seems to happen within importFile in Codec.Midi. If you observe odd behaior on a paritcular input, the checkEvents function will tell you whether the On/Off events line up reasonably. It's not unusual to have a small discrepency and still have correct music. However, cases with many NoteOn and very few NoteOff events (such as thousands of Ons and zero Offs) indicate a problem.

User-level functions:

> countOnOffs :: FilePath -> IO (Int, Int)
> countOnOffs fn = do
>   r <- importFile fn
>   case r of 
>     Left err -> error err
>     Right m -> return (checkEvents m)

> countOnOffs2 :: FilePath -> IO (Int, Int)
> countOnOffs2 fn = do
>   r <- importFile fn
>   case r of 
>     Left err -> error err
>     Right m -> return (checkEvents2' $ tracks m)


Helper functions:

> checkEvents2' :: [[(Ticks, Message)]] -> (Int, Int)
> checkEvents2' [] = (0,0)
> checkEvents2' (t:ts) = 
>     let (a,b) = checkEvents2' ts
>         (a',b') = checkEvents2 t
>     in  (a+a', b+b')

> checkEvents2 :: [(Ticks, Message)] -> (Int, Int)
> checkEvents2 [] = (0,0)
> checkEvents2 ((t,m):ts) = 
>     case m of (NoteOn c p v) -> 
>                   let (ons1,offs1) = checkEvents2 ts
>                   in  (1+ons1,offs1)
>               (NoteOff c p v) -> 
>                   let (ons1,offs1) = checkEvents2 ts
>                   in  (ons1,1+offs1)
>               _ -> checkEvents2 ts

> checkEvents :: Midi -> (Int, Int)
> checkEvents m = 
>     let sevs = midiToEvents m
>         f1 = filter (\(t,p,v,i,e) -> e==On)
>         f2 = filter (\(t,p,v,i,e) -> e==Off)
>         ons =  sum $ map (length.f1) sevs
>         offs = sum $ map (length.f2) sevs
>     in  (ons, offs)



Tests a full cycle of input and output. The midi is read, converted to a music value, and then written to file again.

> testMidiMusic :: FilePath -> IO()
> testMidiMusic fn = do
>   r <- importFile fn 
>   case r of
>     Left err -> error err
>     Right m  -> test (midiToMusic m)


The following will play the Music value that results from reading a MIDI file.

> playMidiMusic :: FilePath -> IO()
> playMidiMusic fn = do
>   r <- importFile fn 
>   case r of
>     Left err -> error err
>     Right m  -> play (midiToMusic m)


The loadMidi function will read in a Midi to the SEvent level to be visually inspected.

> loadMidi :: FilePath -> IO [[SEvent]]
> loadMidi fn = do
>   r <- importFile fn 
>   case r of
>     Left err -> error err
>     Right m  -> return (midiToEvents m)

The loadMidi function will read in a Midi to the Music level to be visually inspected.

> loadMidi2 :: FilePath -> IO (Music (Pitch, Volume))
> loadMidi2 fn = do
>   r <- importFile fn 
>   case r of
>     Left err -> error err
>     Right m  -> return (midiToMusic m)




