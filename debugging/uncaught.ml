let l = ref []
let find_address (name : string) : string = List.assoc name !l
let add_address (name : string) (address : string) : unit = l := (name, address) :: !l
let () = add_address "IRIA" "Roquencourt";;

print_string (find_address "INRIA");
print_newline ()
