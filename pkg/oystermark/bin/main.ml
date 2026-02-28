let () =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buf (input_line stdin);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  let input = Buffer.contents buf in
  let doc = Oystermark.of_string input in
  let r = Cmarkit_commonmark.of_doc doc in
  print_string r
