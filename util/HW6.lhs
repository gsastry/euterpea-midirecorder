CPSC 431/531 Assignment #6 Solutions
Prepared by Donya Quick
Originally posted 19-Nov-2010.
Updated 28-Nov-2010 to have cleaner code.

> module HW6 where
> import Data.Maybe 
> import Euterpea
> import Euterpea.MUI
> import Euterpea.UI
> import qualified Codec.Midi as Midi



Exercise #1
-----------
Provide two pushbuttons, one labeled "Up", the other "Down", such that pressing the up button will increment a note and play it, and pressing the down button will decrement the note and play it. You can start with any note that you like. Be sure to test for "underflow" (going below 0) and "overflow" (going above 127). 

> succ' a = if a<127 then (a+1) else a
> pred' a = if a>0 then (a-1) else a

> buttonMUI = title "MUI 1" $ topDown $ do
>  os <- selectOutput
>  x <- button "Up" 
>  y <- button "Down" 
>  let v = accum 44 ((edge x ->> succ') .|. (edge y ->> pred'))
>      a = title "Pitch" (display (lift1 (show.pitch) v))
>      ns = unique v =>> \k -> [ANote 0 k 100 0.1]
>      mo = midiOut os ns
>  mo >> a

> part1 = runUIEx (300,300) "HW8 Part 1" $ leftRight $ buttonMUI



Exercise #2
-----------
Modify (1) so that it also has two windows, one labeled "PitchClass" and the other labeled "Octave" that display the current pitch class and octave, respectively.


> buttonMUI2 = title "MUI 2" $ topDown $ do
>  os <- selectOutput
>  x <- button "Up" 
>  y <- button "Down" 
>  let v = accum 44 ((edge x ->> succ') .|. (edge y ->> pred'))
>      a = title "PitchClass" 
>              (display (lift1 (show.fst.pitch) v))
>      a' = title "Octave" (display (lift1 (show.snd.pitch) v))
>      ns = unique v =>> \k -> [ANote 0 k 100 0.1]
>      mo = midiOut os ns
>  mo >> leftRight (a >> a')

> part2 = runUIEx (300,300) "HW8 Part 2" $ leftRight $ buttonMUI2



Exercise #3
-----------
Provide a set of 13 radio buttons, labeled C, Cs, D, ..., C to represent, essentially, a one-octave keyboard. Also provide an integral slider labeled "Octave" whose range is 0 to 8. Finally, provide a "Play" pushbutton that will play the selected note once every time it is pushed. 

This solution assumes the note should only play when the button is pressed, rather than playing whenever the radio button selection changes.

> pcs :: [(String, Int)]
> pcs = [("C", 0), ("Cs", 1), ("D", 2), ("Ds", 3), ("E", 4), 
>        ("F", 5), ("Fs", 6), ("G", 7), ("Gs", 8), ("A", 9), 
>        ("As", 10), ("B", 11), ("C", 12)]

> buttonMUI3 = title "MUI 3" $ topDown $ do
>  os <- selectOutput
>  x <- button "PLAY"
>  o <- title "Octave" $ hiSlider 1 (0,10) 4
>  p <- title "PitchClass" $ leftRight $ radio (fst (unzip pcs)) 0
>  let v = lift2 (\x y -> y*12+x) p o
>      ns = snapshot_ (edge x) v =>> \k -> [ANote 0 k 100 0.1]
>  midiOut os ns

> part3 = runUIEx (500,350) "HW8 Part 3" $ leftRight $ buttonMUI3



Exercise #4
-----------
Modify (3) so that it has two (or more) radio buttons that select between two (or more) fixed instruments (say, piano and flute). 


> instrs = [("Piano", 0), ("Bassoon", 1), ("Kalimba", 2)]

> buttonMUI4 = title "MUI 4" $ topDown $ do
>  os <- selectOutput
>  x <- button "PLAY"
>  o <- title "Octave" $ hiSlider 1 (0,10) 4
>  p <- title "PitchClass" $ leftRight $ radio (fst (unzip pcs)) 0
>  i <- title "Instrument" $ leftRight $ radio (fst (unzip instrs)) 0
>  let v = lift2 (\x y -> y*12+x) p o
>      ns = snapshot_ (edge x) v =>> \k -> [ANote 0 k 100 0.1]
>      iNums = [0,70,108]
>      is = unique i =>> \instNum -> 
>                   [Std (Midi.ProgramChange 0 $ iNums !! instNum)]
>  midiOut os (ns .|. is)

> part4 = runUIEx (500,400) "HW8 Part 3" $ leftRight $ buttonMUI4


Exercise #5
-----------
Modify (3) further so that there is a checkbox labeled "Pedal". If it is checked, then every time the "Play" button is pressed, the note is "held". So by selecting several different notes and pressing "Play" each time, a chord will be heard. When the checkbox is de-selected, all the notes should turn off. If the "Play" button is pressed when the checkbox is off, the behavior should be the same as (3). 


This function will regulate whether note-off messages are sent:

> pedalFilter :: ([MidiMessage], Bool) -> [MidiMessage]
> pedalFilter (ms@(m:_),i) = 
>   case m of 
>     Std (Midi.NoteOn c k v) -> ms
>     Std (Midi.NoteOff c k v) -> if i then ms else []
>     ANote c k v t -> 
>         if i then [Std (Midi.NoteOn c k v)] else [ANote c k v t] 
>     _ -> ms


This function ensures that everything gets turned off:

> alloffs = turnOff 127 where
>     turnOff count = 
>         if count<0 then [] 
>         else Std (Midi.NoteOff 0 count 0) : turnOff (count-1)

> buttonMUI5 = title "MUI 5" $ topDown $ do
>  os <- selectOutput
>  x <- button "PLAY"
>  c <- checkbox "Pedal" False
>  o <- title "Octave" $ hiSlider 1 (0,10) 4
>  p <- title "PitchClass" $ leftRight $ radio (fst (unzip pcs)) 0
>  i <- title "Instrument" $ leftRight $ radio (fst (unzip instrs)) 0
>  let v = (lift2 (\x y -> y*12+x) p o)
>      iNums = [0,70,108]
>      stops = unique c =>> \k -> if k then [] else alloffs
>      ns = snapshot_ (edge x) (v) =>> \k -> [ANote 0 k 100 0.1]
>      ns2 = snapshot ns c =>> pedalFilter
>      mo = midiOut os stops
>      is = unique i =>> \instNum -> 
>                   [Std (Midi.ProgramChange 0 $ iNums !! instNum)]
>      mo2 = midiOut os (ns2 .|. is)
>  mo2 >> mo

> part5 = runUIEx (500,400) "HW8 Part 4" $ leftRight $ buttonMUI5



Exercise #6
-----------
Modify the "echo" example from Section 16.5.3 so that instead of taking Midi keyboard input, the program takes its input from the radio buttons defined in (3) above.


> part6 = runUIEx (500,500) "Echo" $ do
>   mo <- selectOutput
>   t <- time
>   r <- title "Decay rate" $ withDisplay (hSlider (0, 0.9) 0.5)
>   f <- title "Echoing frequency" $ withDisplay (hSlider (1, 10) 10)
>   x <- title "Sound" $ button "PLAY"
>   os <- title "Octave" $ hiSlider 1 (0,10) 4
>   pb <- title "PitchClass" $ leftRight $ radio (fst (unzip pcs)) 0
>   let v = (lift2 (\x y -> y*12+x) pb os)
>       ns = snapshot_ (edge x) v =>> \k -> [ANote 0 k 100 0.5]
>       m' = lift1 removeNull $ lift2 merge' ns s
>       s  = delayt t (1.0 / f) (snapshot m' r =>> k)
>       k (ns,r) = mapMaybe (decay 0.1 r) ns          
>   midiOut mo m'

> merge' :: Maybe [a] -> Maybe [a] -> Maybe [a]
> merge' (Just ns1) (Just ns2) = Just (ns1 ++ ns2)
> merge' n1         Nothing    = n1
> merge' Nothing    n2         = n2



Project Proposal
----------------
Provide at least a one-page revised proposal for your final project, based on feedback we have given you. Even if the feedback is positive, give a more detailed breakdown of your project's components, major goals, etc.


