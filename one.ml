#use "topfind"
#require "batteries"

let examples = [
  ("R2, L3", 5);
  ("R2, R2, R2", 2);
  ("R5, L5, R5, R3", 12)
]

let direction_labels = "NESW"

let step_vectors dir = 
  match dir with
  | 0 -> (0, 1)
  | 1 -> (1, 0)
  | 2 -> (0, -1)
  | 3 -> (-1, 0)
  | _ -> (0, 0)

let apply_steps ((x, y), dir) steps =
  let dx, dy = step_vectors dir in
  (x + steps * dx, y + steps * dy)

let apply_turn dir turn =
  if turn == 'L' 
  then (dir - 1 + 4) mod 4 
  else (dir + 1 + 4) mod 4

let parse_command str =
  (str.[0], str |> BatString.slice ~first:1 |> int_of_string)

let apply_command (pos, dir) cmd =
  let turn, steps = parse_command cmd in
  let new_dir = (apply_turn dir turn) in
  let new_pos = apply_steps (pos, new_dir) steps in
  (new_pos, new_dir)

let rec consume_commands point str = 
  match BatString.Exceptionless.split str ~by:", " with
  | Some (h, t) ->
      let next_point = apply_command point h in
      consume_commands next_point t
  | None ->
      apply_command point str

let process_input str =
  let origin = (0, 0) in
  let north = 0 in
  let (x, y), _ = consume_commands (origin, north) str in
  (abs x) + (abs y)

let run_test (str, expected) = 
  let result = process_input str in
  Printf.printf "result: %d\t expected: %d\n" result expected

let process_file filename = 
  BatFile.lines_of filename
  |> BatList.of_enum
  |> List.hd
  |> process_input

let () = 
  ignore (List.iter run_test examples);
  Printf.printf "result: %d\n" (process_file "one.txt")
