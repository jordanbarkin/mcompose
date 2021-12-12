open Msynth
open Stream


type absolute_degree = A | B | C | D | E | F | G
type accidental = Flat | Sharp | Natural
type key = Key of absolute_degree * accidental | Chromatic
type diatonic_degree = Root | Second | Third | Fourth | Fifth | Sixth | Seventh
type note_name =  Diatonic of diatonic_degree | Absolute of absolute_degree
type octave = int

type note_absolute_position = note_name * accidental * octave

type fixed_duration = | Whole  | Half | Quarter 
                      | Eighth | QuarterTrip 
                      | Sixteenth | EigthTrip

type duration = Fixed of fixed_duration | Free of float

type note = Note of (note_absolute_position * duration) | Rest of duration

type monophonic_phrase = note list

type position = float

type polyphonic_phrase = ((note * position) list)

let dur_to_float (duration : duration) : float = 
  match duration with
  | Fixed x -> 
    match x with
    | Whole  -> 4
    | Half  -> 2
    | Quarter  -> 1
    | QuarterTrip  -> .66
    | Eighth  -> .5
    | EigthTrip  -> .33
    | Sixteenth  -> .25
  | Free x -> x

let mono_to_poly (mono : monophonic_phrase) : polyphonic_phrase = 
  let rec helper result current_pos inp = 
  match inp with 
  | [] -> result
  | hd :: tl -> 
      match hd with
      | Note (nap, dur) -> helper (result @ (nap, current_pos)]) (current_pos +  (dur_to_float dur)) tl
      | Rest dur -> helper result (current_pos +  (dur_to_float dur)) tl in 
  helper [] 0 mono

let note_is_diatonic n = 
  match n with
  | Note (nap, dur) ->
    let n, _, _ = nap in 
    match n with 
    | Diatonic _ -> True 
    | _ -> False
  | _ -> True

let mono_phrase_is_diatonic (mono : monophonic_phrase) : bool = 
  List.for_all note_is_diatonic mono

let poly_phrase_is_diatonic (poly : polyphonic_phrase) : bool = 
  List.forall note_is_diatonic (List.map fst poly)

A, 4 = 69

let absolute_to_offset_from_a (note_name, accidental) = 
  let x = (match accidental with 
  | Flat -> -1 
  | Natural -> 0 
  | Sharp -> 1
  ) in
  let res = match note_name with 
  | A -> 0 
  | B -> 2 
  | C -> 3 
  | D -> 5 
  | E -> 7
  | F -> 8 
  | G -> 10 in 
  res + x

let diatonic_to_offset_from_a note_name, accidental, key = 
  

let nap_to_height ((note_name, accidental, octave) : note_absolute_position) (k : key) : int = 
  match note_name with 
  | Diatonic d 
  | Absolute a -> 21 + (octave * 12) + absolute_to_offset_from_a note_name, accidental


let poly_to_msynth_repr (poly : polyphonic_phrase) = 
  let note_to_note ((nt, pos) : note * position) = 
    match nt with 
    | Note nap, dur ->  pos. , (dur_to_float dur), `Note (71, 1.);
    | Rest dur -> (8. , 0. , `Nop)

let mono_to_msynth_repr mono = 
  mono |> mono_to_poly |> poly_to_msynth_repr
(* 
let tempo = 130.

let s =
  let note ?(detune=false) ?(r=0.1) ?(s=0.5) f ~event ~on_die () =
    let env = adsr ~event ~on_die () ~a:0.01 ~d:0.1 ~s ~r () in
    let s = f () in
    let sd = f () in
    fun freq vol ->
      let s = s freq in
      let sd = sd (freq *. 1.007) in
      let s = if detune then B.cmul 0.8 (B.add s sd) else s in
      let s = B.mul env s in
      B.cmul vol s
  in
  let vm = 1. in
  let melody =
    [
      0.,0.75,`Note (77,vm);
      1.,0.5,`Note (76,vm);
      1.5,1.,`Note (72,vm);
      0.,4.,`Nop;
    ]
  in
  let melody = Pattern.repeat 3 melody in
  let melody = Pattern.append melody [0.,4.,`Nop] in
  let melody = Instrument.play (note ~detune:false ~r:0.3 saw) (Pattern.stream ~loop:true tempo melody) in
  let melody = bind2 (Filter.first_order () `Low_pass) (B.add (cst 600.) (B.cmul 300. (sine () 10.))) melody in
  let melody = B.mul melody (OSC.float "/1/fader2" 1.) in
  let melody = bind3 (Filter.biquad () `Low_pass) (OSC.float "/1/fader3" ~min:0.1 ~max:20. 0.5) (OSC.float "/1/fader4" ~max:10000. 10000.) melody in
  let melody = melody >>= Stereo.of_mono in
  let melody = melody >>= Stereo.dephase () 0.01 in
  let vs = 0.7 in
  let synth1 = Pattern.repeat 16 [0., 0.25, `Chord ([65;69;72],vs); 0.25, 0.25, `Nop] in
  let synth2 = Pattern.repeat 16 [0., 0.25, `Chord ([64;69;72],vs); 0.25, 0.25, `Nop] in
  let synth = Pattern.append synth1 synth2 in
  let synth = Instrument.play (note karplus_strong) (Pattern.stream ~loop:true tempo synth) in
  (* (\* let disto = add (cst (-1.)) (cmul 2. (OSC.float "/1/fader4" 0.5)) in *\) *)
  (* (\* let synth = bind2 disto synth (distortion ~dt) in *\) *)
  let synth = B.mul (OSC.float "/1/fader1" 0.5) synth in
  let synth = synth >>= flanger () ~wet:0.8 0.001 (Note.duration tempo 1.) in
  let vb = 1.1 in
  let bass = [0.,16.,`Nop;0.,3.,`Note (41, vb);4.,3.,`Note (38, vb);8.,3.,`Note (45, vb);12.,3.,`Note (45, vb)] in
  let bass = Instrument.play (note ~s:0.8 ~r:0.4 sine) (Pattern.stream ~loop:true tempo bass) in
  let kick = Pattern.repeat 16 [0.,0.25,`Note(69,1.8);0.,1.,`Nop] in
  let kick = Instrument.play_drum (fun ~on_die _ vol -> B.cmul vol (Note.Drum.kick ~on_die ())) (Pattern.stream ~loop:true tempo kick) in
  let snare = Pattern.repeat 16 [1.,0.25,`Note(69,0.8);0.,2.,`Nop] in
  let snare = Instrument.play_drum (fun ~on_die _ vol -> B.cmul vol (Note.Drum.snare ~on_die ())) (Pattern.stream ~loop:true tempo snare) in
  let s = synth in
  (* (\* let s = bind2 (integrate ~dt 100.) s (Filter.first_order ~dt `Low_pass) in *\) *)
  (* (\* let s = s >>= slicer ~dt 0.01 in *\) *)
  let s = s >>= Stereo.of_mono in
  (* (\* let deph = let deph = Stereo.dephase ~dt 0.1 in fun d x -> deph ~delay:d x in *\) *)
  (* (\* let s = bind2 (sub (cmul 0.1 (OSC.float "/1/fader5" 0.51)) (cst 0.05)) s deph in *\) *)
  let s = Stereo.add s (bass >>= Stereo.of_mono >>= Stereo.dephase () (-0.02)) in
  let s = Stereo.add s (snare >>= Stereo.of_mono >>= Stereo.dephase () (-0.01)) in
  let s = Stereo.add s (kick >>= Stereo.of_mono) in
  let s = Stereo.add s melody in
  let s = Stereo.cmul 0.2 s in
  s

let () =
  Output.play s *)
