(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst


let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big


(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)

let rows grid = List.init 9 (fun ind -> grid.(ind))

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box baza (grid : 'a grid) (box_ind : int) = 
  let start_row = 
    match box_ind with
    | _ when box_ind <= 2 -> 0
    | _ when box_ind >= 6 -> 6
    | _ -> 3
  in
  let start_col = 
    match box_ind with 
    | 0 | 3 | 6 -> 0
    | 1 | 4 | 7 -> 3
    | _ -> 6
  in
  let box = Array.make_matrix 3 3 baza in
  for i = 0 to 2 do
    for j = 0 to 2 do
      box.(i).(j) <- grid.(i + start_row).(j + start_col)
    done;
  done;
  box

let box_to_row baza (box : 'a grid) =
  let row = Array.make 9 baza in
  for i = 0 to 2 do
    for j = 0 to 2 do
      row.(3 * i + j) <- box.(i).(j)
    done;
  done;
  row

let boxes baza grid =
  let rec aux acc = function
    | 9 -> acc
    | i -> aux (get_box baza grid (8 - i) :: acc) (i + 1)
  in
  aux [] 0

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.map (Array.map f) grid

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let string_of_option = function
  | Some x -> string_of_int x
  | _ -> " "

let print_problem problem : unit = print_grid string_of_option problem.initial_grid
  

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid
                                                                                                              
let print_solution solution = print_grid string_of_int solution

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
        if aaa = bbb then false else aux arr i (j + 1)
    in
    aux arr i (i + 1)


let preveri solution =
  let rec aux sol= function
  | 9 -> true
  | i -> 
    let row = get_row sol i 
    and col = get_column sol i 
    and box = get_box 0 sol i |> box_to_row 0 in
    if test_aux row 0 && test_aux col 0 && test_aux box 0 then aux sol (i + 1)
    else false
  in
  aux solution 0

let is_valid_solution problem solution = 
  preveri solution

(*
let print_stanje grid' i' j' list' = 
  let list = list' |> string_of_list string_of_option " "
  in
  print_grid string_of_option cur_grid;
  Printf.printf "\n i: %s   j: %s \n" (string_of_int i) (string_of_int j)
  Printf.printf "seznam: %s" list

let a = "
┏━━━┯━━━┯━━━┓
┃483│921│657┃
┃967│3 5│821┃
┃251│876│493┃
┠───┼───┼───┨
┃548│132│976┃
┃729│ 64│ 38┃
┃136│798│ 45┃
┠───┼───┼───┨
┃372│689│514┃
┃814│253│769┃
┃695│417│382┃
┗━━━┷━━━┷━━━┛"
  |> problem_of_string



let sdk = Array.make_matrix 9 9 0

let spice_up matrix9 =
    for i = 0 to 8 do
        for j = 0 to 8 do
            matrix9.(i).(j) <- i + j 
        done
    done
    
let () = spice_up sdk
*)
