type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
(* V current_problem zabeležim četvero podatkov (vrstica, stolpec, škatla, Some x),
  da teh ne računam znova po nepotrebnem. *)
type state = { 
  problem : Model.problem; 
  current_grid : int option Model.grid;
  current_location : int * int;
  current_list : int option list
  }

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> " " | Some digit -> string_of_int digit)
    state.current_grid

let print_list list =
  let list' = list |> Model.string_of_list Model.string_of_option " " in
  Printf.printf "Sez: %s \n\n" list'

type response = Solved of Model.solution | Unsolved of state | Fail of state

let rec test_aux arr =
  function
  | 8 -> true
  | i ->
    let aaa = arr.(i) in
    let rec aux arr i =
      function
      | 9 -> test_aux arr (i + 1)
      | j -> 
        let bbb = arr.(j) in
        if aaa = bbb && aaa <> None then false else aux arr i (j + 1)
    in
    aux arr i (i + 1)

(* Tri funkcije za preverjanje pravilnosti *)

let check_row rowid grid =
  let row = Model.get_row grid rowid in
  test_aux row 0 

let check_col colid grid =
  let col = Model.get_column grid colid in
  test_aux col 0

let check_box boxid grid =
  let box = Model.get_box None grid boxid |> Model.box_to_row None in
  test_aux box 0

let grid_number i j =
  3 * (i / 3) + (j / 3)

(* Funkcija, ki poišče naslednjega ustreznega kandidata. 
  Po sudokuju se premika v smeri branja *)
let rec search grid i j =
    match grid.(i).(j) with
    | Some _ ->
      if j = 8 then search grid (i + 1) 0 else search grid i (j + 1)
    | None -> (i, j)

let smart_choice i j grid =
  check_row i grid && check_col j grid && check_box (grid_number i j) grid

let find_none (state : state) =
  search state.current_grid 0 0

let set_guess_list (state : state) i j =
  let list = ref [] in
  let grid = Model.copy_grid state.current_grid in
  for s = 9 downto 1 do
    grid.(i).(j) <- Some s;
    if smart_choice i j grid then 
    list := Some s :: !list
  done;
  !list  




let initialize_state (problem : Model.problem) : state = {
  problem = problem; 
  current_grid = Model.copy_grid problem.initial_grid;  
  current_location = (-1, 8);
  current_list = []
  }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  if state.current_list = [Some 0] then None else 
  let i, j = state.current_location in
  let curr_grid = Model.copy_grid state.current_grid in
  let list' =
  match state.current_list with
    | [] -> [Some 0]
    | guess :: list -> 
      curr_grid.(i).(j) <- guess;
    list
  in
  let st2 = { state with current_grid = curr_grid ; current_list = list' } in 
  Some (state, st2)

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  let (i, j) = find_none state in
  let possible = set_guess_list state i j in
  (*print_state state;
  print_list possible;*)
  match possible with
  | [] -> None
  | guess :: list ->
  state.current_grid.(i).(j) <- guess;
  let state' = { state with current_list = list ; current_location = (i, j) } in
  match validate_state state' with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          explore_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state
