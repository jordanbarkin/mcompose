(* Defining pitches *)
type absolute_degree = A | B | C | D | E | F | G
type accidental = Flat | Sharp | Natural
type diatonic_degree = Root | Second | Third | Fourth | Fifth | Sixth | Seventh
type note_name =  Diatonic of diatonic_degree | Absolute of absolute_degree
type octave = int

(* This type gives a general interpretation of the pitch of a note *)
type note_absolute_position = note_name * accidental * octave

(* Defining keys *)
type key = Key of absolute_degree * accidental | Chromatic

(* Defining rhythms *)
type fixed_duration = | Whole  | Half | Quarter 
                      | Eighth | QuarterTrip 
                      | Sixteenth | EigthTrip

(* A note can heither have a fixed length, from the above,
   or it can have a float-valued length of arbitrary beats  *)
type duration = Fixed of fixed_duration | Free of float

(* A note (event) is a pitch and its position, or a rest, which has no pitch. *)
type note = Note of (note_absolute_position * duration) | Rest of duration

(* A monophonic phrase is just a set of notes, back to back *)
type monophonic_phrase = note list

(* A position is a real-valued start position within a measure *)
type position = float

(* A polyphonic phrase can have arbitrarily many notes start at arbitrary positions *)
type polyphonic_phrase = ((note * position) list)

let dur_to_float (duration : duration) : float = 
  match duration with
  | Fixed x -> 
    (match x with
    | Whole  -> 4.
    | Half  -> 2.
    | Quarter  -> 1.
    | QuarterTrip  -> 0.66
    | Eighth  -> 0.5
    | EigthTrip  -> 0.33
    | Sixteenth  -> 0.25)
  | Free x -> x

let mono_to_poly (mono : monophonic_phrase) : polyphonic_phrase = 
  let rec helper (result : polyphonic_phrase) (current_pos : float) (inp : monophonic_phrase) = 
  match inp with 
  | [] -> result
  | hd :: tl -> 
      (match hd with
      | Note (_nap, dur) -> (helper (result @ [(hd, current_pos)]) (current_pos +.  (dur_to_float dur)) tl)
      | Rest dur -> (helper result (current_pos +.  (dur_to_float dur)) tl) ) in 
  helper [] 0. mono

let note_is_diatonic n = 
  match n with
  | Note (nap, _dur) ->
    let n, _, _ = nap in 
    (match n with 
    | Diatonic _ -> true 
    | _ -> false)
  | Rest (_d) -> true

let mono_phrase_is_diatonic (mono : monophonic_phrase) : bool = 
  List.for_all note_is_diatonic mono

let poly_phrase_is_diatonic (poly : polyphonic_phrase) : bool = 
  List.for_all note_is_diatonic (List.map fst poly)

let absolute_to_offset_from_a note_name accidental = 
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

(* let diatonic_to_key_of_c (d : diatonic_degree) : absolute_degree = 
  match d with 
  | Root -> C
  | Second -> D 
  | Third  -> E 
  | Fourth -> F 
  | Fifth  -> G
  | Sixth  -> A 
  | Seventh -> B *)

(* 

let semitone_to_nap (s : int) : note_absolute_position = 
  let s = s mod 12 in 
  match s with 
  | 0 -> (Absolute C, Natural, 0)
  | 1 -> (Absolute  C, Sharp, 0) 
  | 2 -> (Absolute  D, Natural, 0)
  | 3 -> (Absolute  D, Sharp, 0)
  | 4 -> (Absolute  E, Natural, 0)
  | 5 -> (Absolute  F, Natural, 0)
  | 6 -> (Absolute  F, Sharp, 0)
  | 7 -> (Absolute  G, Natural, 0)
  | 8 -> (Absolute  G, Sharp, 0)
  | 9 -> (Absolute  A, Natural, 0)
  | 10 -> (Absolute  A, Sharp, 0)
  | 11 -> (Absolute  B, Natural, 0)

let transpose_

let diatonic_to_offset_from_a _note_name _accidental _key = 0

   *)


