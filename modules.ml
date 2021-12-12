open Music
open Utils
open Msynth
open Stream



let transpose (phrase : polyphonic_phrase) (k : key) (degrees : int) : polyphonic_phrase = 
    let as_dia = make_phrase_diatonic phrase k in 
    let transpose_nap (degrees : int)  ((n, a, o) : note_absolute_position) (_k : key) : note_absolute_position = 
        match n with 
        | Diatonic deg -> let dia_deg, oct = to_dia_number deg o |> (( + ) degrees) |> from_dia_number in (Diatonic dia_deg, a, oct)
        | Absolute _ -> raise (Internal "should be diatonic") in 
    transform_phrase_notes (transpose_nap degrees) as_dia k

let transpose_semitones (phrase : polyphonic_phrase) (k : key) (sem : int) : polyphonic_phrase = 
    List.map (fun (n, p) -> ((transpose_chromatic_note n sem k), p)) phrase

let concatenate_time (phrases : polyphonic_phrase list) : polyphonic_phrase = 
    let rec helper phrases offset = 
        match phrases with 
        | [] -> []
        | hd :: tl -> (List.map (fun (n, p) -> (n, p +. offset)) hd) @ helper tl (offset +. 4.) in 
    helper phrases 0.

let mix (phrases : polyphonic_phrase list) : polyphonic_phrase = 
    List.fold_left ( @ ) [] phrases

(* let dotted_quarter_swing (phrase : polyphonic_phrase) : polyphonic_phrase = 
    ()

let diatonic_inversion (phrase : polyphonic_phrase) (k : key) : polyphonic_phrase = 
    ()

let add_seventh (phrase : polyphonic_phrase) (k : key) : polyphonic_phrase = 
    () (* match phrase with 
    |  *) *)

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
  (* let vm = 1. in *)
  let mo = [Note ((Absolute C, Natural, 4), Fixed Quarter);Note ((Absolute D, Natural, 4), Fixed Quarter); Note ((Absolute E, Natural, 4), Fixed Quarter); Note ((Absolute F, Natural, 4), Fixed Quarter); ] in 
  let mo = mono_to_poly mo in
  let mo2 = transpose_semitones mo (Key (C, Natural)) 1 in
  let mo_comb = concatenate_time [mo; mo2] in  
  let melody = poly_to_msynth_repr Chromatic mo_comb  in 
  (* let melody =
    [
      0.,0.75,`Note (77,vm);
      1.,0.5,`Note (76,vm);
      1.5,1.,`Note (72,vm);
      0.,4.,`Nop;
    ]
  in *)
  let melody = Pattern.repeat 3 melody in
  let melody = Pattern.append melody [0.,4.,`Nop] in
  let melody = Instrument.play (note ~detune:false ~r:0.1 sine) (Pattern.stream ~loop:true tempo melody) in
  (* let melody = bind2 (Filter.first_order () `Low_pass) (B.add (cst 600.) (B.cmul 300. (sine () 10.))) melody in *)
  (* let melody = B.mul melody (OSC.float "/1/fader2" 1.) in *)
  (* let melody = bind3 (Filter.biquad () `Low_pass) (OSC.float "/1/fader3" ~min:0.1 ~max:20. 0.5) (OSC.float "/1/fader4" ~max:10000. 10000.) melody in *)
  let melody = melody >>= Stereo.of_mono in
  (* let melody = melody >>= Stereo.dephase () 0.01 in *)
  (* let vs = 0.7 in *)
  (* let synth1 = Pattern.repeat 16 [0., 0.25, `Chord ([65;69;72],vs); 0.25, 0.25, `Nop] in *)
  (* let synth2 = Pattern.repeat 16 [0., 0.25, `Chord ([64;69;72],vs); 0.25, 0.25, `Nop] in *)
  (* let synth = Pattern.append synth1 synth2 in *)
  (* let synth = Instrument.play (note karplus_strong) (Pattern.stream ~loop:true tempo synth) in *)
  (* (\* let disto = add (cst (-1.)) (cmul 2. (OSC.float "/1/fader4" 0.5)) in *\) *)
  (* (\* let synth = bind2 disto synth (distortion ~dt) in *\) *)
  (* let synth = B.mul (OSC.float "/1/fader1" 0.5) synth in *)
  (* let synth = synth >>= flanger () ~wet:0.8 0.001 (Note.duration tempo 1.) in *)
  (* let vb = 1.1 in *)
  (* let bass = [0.,16.,`Nop;0.,3.,`Note (41, vb);4.,3.,`Note (38, vb);8.,3.,`Note (45, vb);12.,3.,`Note (45, vb)] in *)
  (* let bass = Instrument.play (note ~s:0.8 ~r:0.4 sine) (Pattern.stream ~loop:true tempo bass) in *)
  (* let kick = Pattern.repeat 16 [0.,0.25,`Note(69,1.8);0.,1.,`Nop] in *)
  (* let kick = Instrument.play_drum (fun ~on_die _ vol -> B.cmul vol (Note.Drum.kick ~on_die ())) (Pattern.stream ~loop:true tempo kick) in *)
  (* let snare = Pattern.repeat 16 [1.,0.25,`Note(69,0.8);0.,2.,`Nop] in *)
  (* let snare = Instrument.play_drum (fun ~on_die _ vol -> B.cmul vol (Note.Drum.snare ~on_die ())) (Pattern.stream ~loop:true tempo snare) in *)
  (* let s = synth in *)
  (* (\* let s = bind2 (integrate ~dt 100.) s (Filter.first_order ~dt `Low_pass) in *\) *)
  (* (\* let s = s >>= slicer ~dt 0.01 in *\) *)
  (* let s = s >>= Stereo.of_mono in *)
  (* (\* let deph = let deph = Stereo.dephase ~dt 0.1 in fun d x -> deph ~delay:d x in *\) *)
  (* (\* let s = bind2 (sub (cmul 0.1 (OSC.float "/1/fader5" 0.51)) (cst 0.05)) s deph in *\) *)
  (* let s = Stereo.add s (bass >>= Stereo.of_mono >>= Stereo.dephase () (-0.02)) in
  let s = Stereo.add s (snare >>= Stereo.of_mono >>= Stereo.dephase () (-0.01)) in
  let s = Stereo.add s (kick >>= Stereo.of_mono) in
  let s = Stereo.add s melody in
  let s = Stereo.cmul 0.2 s in *)
  melody

let () =
  Output.play s