#use "topfind"
#require "batteries"

let keypad = [
  [1; 2; 3];
  [4; 5; 6];
  [7; 8; 9]
]

let examples = [
  (["ULL"; "RRDDD"; "LURD"; "UUUUD"], 1985);
  (["ULL"; "RRD"; "LLDD"; "UUUUD"], 1674)
]

let dir_to_vec dir = 
  match dir with
  | 'U' -> (0, -1)
  | 'D' -> (0, 1)
  | 'L' -> (-1, 0)
  | 'R' -> (1, 0)
  | _   -> (0, 0)

let clamp x =
  min (max x 0) 2

let run_command (x, y) dir =
  let (dx, dy) = dir_to_vec dir in
  (clamp (x + dx), clamp (y + dy))

let pos_to_key (x, y) =
  BatList.at (BatList.at keypad y) x

let process_line start str =
  List.fold_left run_command start (BatString.explode str)

let concat_keys int_list =
  int_list 
  |> (List.mapi (fun i x -> (BatInt.pow 10 i) * x))
  |> (List.fold_left (+) 0)

let process_lines lines =
  let fold_line (from, acc) str =
    let pos = process_line from str in
    let key = pos_to_key pos in 
    (pos, (key::acc)) in
  let _, keys = List.fold_left fold_line ((1, 1), []) lines in
  concat_keys keys

let process_file filename =
  BatFile.lines_of filename
  |> BatList.of_enum
  |> process_lines 

let run_test (lines, expected) = 
  let result = process_lines lines in
  Printf.printf "result: %d\t expected: %d\n" result expected

let () = 
  ignore (List.map run_test examples);
  Printf.printf "result: %d\n" (process_file "two.txt")

