(* Allen Cabrera, Avanish Kulkarni - PA2C2 *)

open Printf

type cool_program = cool_class list
and loc = string (* actually and int *)
and id = loc * string
and cool_type = id
and cool_class = id * (id option) * feature list 
and feature = 
    | Attribute of id * cool_type * (exp option)
    | Method of id * (formal list) * cool_type * exp
and formal = id * cool_type
and exp = loc * exp_kind 
and exp_kind = 
    | Integer of string (* int *)

let main () = begin 
    (* De-serialize CL-AST file *)
    let fname = Sys.argv.(1) in 
    let fin = open_in fname in 

    let rec range k = 
        if k <= 0 then []
        else k :: (range (k - 1))
    in
    
    let read () = input_line fin in 

    let read_list worker = 
        let k = int_of_string(read ()) in 
        let lst = range k in 
        List.map (fun _ -> worker ()) lst 
    in

    let rec read_cool_program () = 
        read_list read_cool_class 

    and read_id () = 
        let loc = read () in
        let name = read () in 
        (loc, name)


    and read_cool_class () = (* Class *)
        let cname = read_id() in 
        let inherits = match read() with 
        | "no_inherits" -> None
        | "inherits" -> 
            let super = read_id () in
            Some(super)
        | x -> failwith ("cannot happen: " ^ x)
        in 
        let features = read_list read_feature in 
        (cname, inherits, features)

    and read_feature () = 
        match read () with 
        | "attribute_no_init" ->
            let fname = read_id () in 
            let ftype = read_id () in 
            Attribute(fname, ftype, None)
        | "attribute_init" ->
            let fname = read_id () in 
            let ftype = read_id () in 
            let finit = read_exp () in 
            Attribute(fname, ftype, (Some finit))
        | "method" ->
            let mname = read_id () in 
            let formals = read_list read_formal in 
            let mtype = read_id () in 
            let mbody = read_exp () in 
            Method(mname, formals, mtype, mbody)
        | x -> failwith ("cannot happen: " ^ x)

    and read_formal () = 
        let fname = read_id () in
        let ftype = read_id () in 
        (fname, ftype)

    and read_exp () = 
        let eloc = read () in 
        let ekind = match read () with 
        (* do the rest of the types *)
        | "integer" ->
            let ival = read () in 
            Integer(ival)
        | x -> 
            failwith ("invalid expression kind: " ^ x)
        in (eloc, ekind)
    in 

    let ast = read_cool_program () in 
    close_in fin;
    printf "CL_AST deserialized, %d classes\n" (List.length ast) ;
end;;
main () ;; 