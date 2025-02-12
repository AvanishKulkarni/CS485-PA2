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

    (* Check for class-related errors *)

    let illegal_inherit_classes = ["Int"; "Bool"; "String"] in 
    let base_classes = [ "Int"; "Bool" ; "String"; "IO"; "Object" ] in
    let user_classes = List.map(fun ((_,cname),_,_) -> cname ) ast in 
    let all_classes = base_classes @ user_classes in 
    let all_classes = List.sort compare all_classes in 

    (* 
        look for inheritance from Int 
        look for inheritance from Undeclared Class
    *)

    List.iter (fun ((cloc, cname), inherits, features) -> 
        match inherits with
        | None -> () 
        | Some(iloc, iname) -> (* inherited type identifier *)
            if List.mem iname illegal_inherit_classes then begin 
                printf "ERROR: %s: Type-Check: inheriting from forbidden class %s\n" iloc iname ;
                exit 1
            end ;
            if not (List.mem iname all_classes) then begin
                printf "ERROR: %s: Type-Check: inheriting from undefined class %s\n" iloc iname ;
                exit 1
            end ;
    ) ast;

    (* Error checking complete *)

    (* Emit CL-TYPE File *)

    let cltname = (Filename.chop_extension fname) ^ ".cl-type" in 
    let fout = open_out cltname in 

    let rec output_exp (eloc, ekind) = 
        fprintf fout "%s\n" eloc;
        match ekind with 
        | Integer (ival) -> fprintf fout "integer\n%s\n" ival 
    in

    fprintf fout "class_map\n%d\n" (List.length all_classes);
    List.iter (fun cname -> 
        fprintf fout "%s\n" cname ;
        let attributes = 
            try 
                let _, inherits, features = List.find (fun ((_,cname2),_,_) -> cname = cname2) ast in 
                List.filter (fun feature -> match feature with 
                | Attribute _ -> true 
                | Method _ -> false 
                ) features
            with Not_found -> 
                [] 
        in 
        fprintf fout "%d\n" (List.length attributes) ;
        List.iter (fun attr -> match attr with 
        | Attribute ((_, aname),(_, atype), None) -> 
            fprintf fout "no_initializer\n%s\n%s\n" aname atype
        | Attribute ((_, aname),(_, atype), Some init) ->
            fprintf fout "initializer\n%s\n%s\n" aname atype ;
            output_exp init
        | Method _ -> failwith "method unexpected"
        ) attributes ;
    ) all_classes;

    close_out fout ; 

end;;
main () ;; 