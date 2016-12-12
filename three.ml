#use "topfind"
#require "batteries"

let examples = [
  ([" 5 10 25"; " 100 200 200"; " 1 2 3"], 1)  
]

let is_triangle a b c =
  (a + b > c) &&
  (a + c > b) &&
  (b + c > a)

let process_lines lines =
  let count_triangles count str =
    Scanf.sscanf str " %i %i %i" (fun a b c ->
      if is_triangle a b c
      then count + 1
      else count) in
  List.fold_left count_triangles 0 lines

let process_file filename =
  BatFile.lines_of filename
  |> BatList.of_enum
  |> process_lines 

let run_test (lines, expected) = 
  let result = process_lines lines in
  Printf.printf "result: %d\t expected: %d\n" result expected

let () = 
  ignore (List.map run_test examples);
  Printf.printf "result: %d\n" (process_file "three.txt")

