let learnocaml_version = ref false
let kind = ref false

let close_files_noerr oc ic  = 
  if not !learnocaml_version then Printf.fprintf oc ",\n  \"learnocaml_version\" : \"2\"";
  if not !kind then Printf.fprintf oc ",\n  \"kind\" : \"exercise\"";

  Printf.fprintf oc "\n}\n"; (* Write } to close json tree *)
  close_out_noerr oc;        (* flush and close the channel *)
  close_in_noerr ic          (* emergency closing *)

let parse_value value = match value.[1] with
  | '"' -> value
  | '(' | '[' ->  
    let tmp = ref "" in
    String.iter (fun c -> 
      tmp := (!tmp)^(match c with
                      | ')' -> "]"
                      | '(' -> "["
                      | ';' -> ","
                      | _ as c -> Char.escaped c)
    ) value;
    !tmp
  | '0' .. '9' -> (String.trim value)
  | _  -> "\""^(String.trim value)^"\""  

let parse_json in_file out_file =
  let oc = open_out out_file in    (* create or truncate file, return channel *)
  Printf.fprintf oc "{\n";   (* write something *)   
 
  (* Read file and display the first line *)
  let ic = open_in in_file in
  try
    let is_first_line = ref true in
    while true do
      let line = input_line ic in  (* read line from in_channel and discard \n *)
      let result = String.split_on_char '=' line in
      let ident = List.nth (String.split_on_char ' ' (List.nth result 0)) 1 in
      let value = List.nth result 1 in
      
      if not !is_first_line then Printf.fprintf oc ",\n";
      is_first_line := false;

      if ident = "learnocaml_version" then learnocaml_version := true;
      if ident = "kind" then kind := true;
      
      let value = parse_value value in

      (* Write value *)
      Printf.fprintf oc "  \"%s\" :%s" ident value;   

    done;
    close_files_noerr oc ic
    
  with _ -> close_files_noerr oc ic