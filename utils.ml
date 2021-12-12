open Music 

exception Internal of string


let to_int_absolute (n : absolute_degree) (a : accidental) (o : octave) : int = 
  let x = (match a with 
  | Flat -> -1 
  | Natural -> 0 
  | Sharp -> 1
  ) in
  let res = match n with 
  | A -> 9 
  | B -> 11 
  | C -> 0
  | D -> 2 
  | E -> 4
  | F -> 5 
  | G -> 7 in 
  o * 12 + x + res

let from_int_absolute (inp : int) : (note_name * accidental * octave) = 
    let o = inp / 12 in 
    let inp = inp mod 12 in
    let abs, a = (match inp with 
    | 0 -> (C, Natural)
    | 1 -> (C, Sharp) 
    | 2 -> (D, Natural)
    | 3 -> (D, Sharp)
    | 4 -> (E, Natural)
    | 5 -> (F, Natural)
    | 6 -> (F, Sharp)
    | 7 -> (G, Natural)
    | 8 -> (G, Sharp)
    | 9 -> (A, Natural)
    | 10 -> (A, Sharp)
    | 11 -> (B, Natural) 
    | _x -> raise (Internal "")) in 
    (Absolute abs, a, o)

let to_dia_number (d : diatonic_degree) (o : int) : int = 
  (o * 7) + 
  (match d with 
  | Root -> 0
  | Second -> 1 
  | Third  -> 2 
  | Fourth -> 3 
  | Fifth  -> 4
  | Sixth  -> 5 
  | Seventh -> 6) 

let from_dia_number (i : int) : diatonic_degree * int = 
    let o  = i/7 in 
    let res =  
    (match i mod 7 with
    | 0 -> Root 
    | 1 -> Second  
    | 2 -> Third   
    | 3 -> Fourth  
    | 4 -> Fifth  
    | 5 -> Sixth   
    | 6 -> Seventh 
    | _x -> raise (Internal "")  
) in 
    (res, o)

let to_c (d : diatonic_degree) : absolute_degree = 
  match d with 
  | Root -> C
  | Second -> D 
  | Third  -> E 
  | Fourth -> F 
  | Fifth  -> G
  | Sixth  -> A 
  | Seventh -> B

let from_c d  = 
  match d with 
  | Absolute C -> Diatonic Root  
  | Absolute D -> Diatonic Second   
  | Absolute E -> Diatonic Third    
  | Absolute F -> Diatonic Fourth   
  | Absolute G -> Diatonic Fifth   
  | Absolute A -> Diatonic Sixth    
  | Absolute B -> Diatonic Seventh  
  | Diatonic _ -> d

let key_to_offset (k : key) : int = 
    match k with 
    | Chromatic -> 0 
    | Key (abs, acc) ->  to_int_absolute abs acc 0

let to_int_diatonic  (k : key) (n : diatonic_degree) (a : accidental) (o : octave) = 
    let as_c = to_c n in 
    let as_c_int = to_int_absolute as_c a o in 
    let offset = key_to_offset k in 
    offset + as_c_int

let from_int_diatonic (k : key)  (n : int) = 
    let offset = key_to_offset k in 
    let as_c_int = n - offset in 
    let (n, a, o) =  from_int_absolute as_c_int in 
    (from_c n), a, o

let to_int (k : key) ((n, a, o)  : note_absolute_position) : int = 
    match n with 
    | Diatonic deg -> to_int_diatonic k deg a o 
    | Absolute ab -> to_int_absolute ab a o 

let from_int (k : key) (n : int) (dia : bool)  : note_absolute_position  = 
    if dia then from_int_diatonic k n else from_int_absolute n

let abs_to_dia (n,a,o : note_absolute_position)  key = 
    match n with 
    | Diatonic _deg -> n,a,o
    | Absolute ab -> to_int_absolute ab a o |> (from_int_diatonic key)

let dia_to_abs (n,a,o : note_absolute_position) (k : key) =
    match n with 
    | Diatonic deg -> to_int_diatonic k deg a o |> from_int_absolute
    | Absolute _ab -> (n,a,o)

let transpose_chromatic_nap ((n, a, o) : note_absolute_position) (semitones : int) (k : key) : note_absolute_position = 
    match n with 
    | Diatonic deg -> to_int_diatonic  k deg a o |> (( + ) semitones) |> from_int_diatonic k
    | Absolute ab -> to_int_absolute ab a o |> (( + ) semitones) |> from_int_absolute

let transpose_chromatic_note (n : note) s k = 
    match n with 
    | Rest _ -> n
    | Note (nap, dur) -> Note (transpose_chromatic_nap nap s k, dur) 

let transform_phrase_notes (f : note_absolute_position -> key -> note_absolute_position) phrase k = 
    let transform n k = 
        match n with 
        | Rest _ -> n
        | Note (nap, dur) -> Note (f nap k, dur) in
    List.map (fun (n, p) -> ((transform n k), p)) phrase   

let make_phrase_diatonic (phrase : polyphonic_phrase) (k : key) = 
    transform_phrase_notes abs_to_dia phrase k

let make_phrase_absolute (phrase : polyphonic_phrase) (k : key) = 
    transform_phrase_notes dia_to_abs phrase k
exception NotImplemented of string

let rec nap_to_height ((note_name, accidental, octave) : note_absolute_position) (k : key) : int = 
  match note_name with 
   | Diatonic _d ->  nap_to_height (dia_to_abs  (note_name, accidental, octave) k) k (*21 + (octave * 12) + diatonic_to_offset_from_a d accidental k *)
  | Absolute a -> 21 + (octave * 12) + absolute_to_offset_from_a a accidental


let poly_to_msynth_repr (k : key) (poly : polyphonic_phrase)  = 
  let note_to_note ((nt, pos) : note * position) = 
    match nt with 
    | Note (nap, dur) ->  pos , (dur_to_float dur), `Note ((nap_to_height nap k), 0.5);
    | Rest _dur -> (pos , 0. , `Nop) in
  List.map note_to_note poly

let mono_to_msynth_repr mono k = 
  (mono |> mono_to_poly) |> poly_to_msynth_repr k


