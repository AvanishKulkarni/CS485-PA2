(* Allen Cabrera, Avanish Kulkarni - PA2C2 *)

open Printf

type static_type = 
| Class of string 
| SELF_TYPE of string 

let type_to_str t = match t with 
| Class(x) -> x
| SELF_TYPE(c) -> "SELF_TYPE"

let rec is_subtype t1 t2 = 
    match t1, t2 with 
    | Class(x), Class(y) when x = y -> true
    | Class(x), Class("Object") -> true
    | Class(x), Class(y) -> false (* check parent map *)
    | _, _ -> false (* self type behavior *)

type cool_program = cool_class list
and loc = string (* actually and int *)
and id = loc * string
and cool_type = id
and cool_class = id * (id option) * feature list 
and feature = 
    | Attribute of id * cool_type * (exp option)
    | Method of id * (formal list) * cool_type * exp
and formal = id * cool_type
and exp = {
    loc : loc;
    exp_kind : exp_kind;
    mutable static_type : static_type option;
}
and exp_kind = 
    | Assign of id * exp (* assign *)
    | Dynamic_Dispatch of exp * id * exp list 
    | Static_Dispatch of exp * id * id * exp list
    | Self_Dispatch of id * exp list 
    | If of exp * exp * exp 
    | While of exp * exp 
    | Block of exp list 
    | New of id 
    | Isvoid of exp 
    | Plus of exp * exp 
    | Minus of exp * exp 
    | Times of exp * exp 
    | Divide of exp * exp 
    | Lt of exp * exp 
    | Le of exp * exp 
    | Eq of exp * exp 
    | Not of exp 
    | Negate of exp 
    | Integer of string (* int *)
    | String of string (* string *)
    | Identifier of id 
    | Bool of string (* bool *)
    | Let of binding list * exp
    | Case of exp * case_elem list
and binding = 
    | Binding of id * id * (exp option)
and case_elem = 
    | Case_Elem of id * id * exp

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

    and read_let_binding () = 
        match read () with 
        | "let_binding_no_init" -> 
            let letvar = read_id () in 
            let lettype = read_id () in
            Binding(letvar, lettype, None) 
        | "let_binding_init" -> 
            let letvar = read_id () in 
            let lettype = read_id () in 
            let letval = read_exp () in 
            Binding(letvar, lettype, Some(letval))
        | x -> failwith ("impossible binding " ^ x)

    and read_case_elem () = 
        let csid = read_id () in 
        let cstype = read_id () in 
        let csbody = read_exp () in 
        Case_Elem(csid, cstype, csbody)
                
    and read_exp () = 
        let eloc = read () in 
        let ekind = match read () with 
        (* do the rest of the types *)
        | "assign" -> 
            let avar = read_id () in 
            let aexp = read_exp () in 
            Assign(avar, aexp)
        | "dynamic_dispatch" ->
            let ddexp = read_exp () in 
            let ddmethod = read_id () in 
            let ddargs = read_list read_exp in 
            Dynamic_Dispatch(ddexp, ddmethod, ddargs)
        | "static_dispatch" ->
            let sdexp = read_exp () in 
            let sdid = read_id () in 
            let sdmethod = read_id () in 
            let sdargs = read_list read_exp in 
            Static_Dispatch(sdexp, sdid, sdmethod, sdargs)
        | "self_dispatch" ->
            let sdmethod = read_id () in 
            let sdargs = read_list read_exp in 
            Self_Dispatch(sdmethod, sdargs)
        | "if" ->
            let ipred = read_exp () in 
            let ithen = read_exp () in
            let ielse = read_exp () in 
            If(ipred, ithen, ielse)
        | "while" ->
            let wpred = read_exp () in 
            let wbody = read_exp () in 
            While(wpred, wbody)
        | "block" ->
            let bbody = read_list read_exp in
            Block(bbody)
        | "new" -> 
            let nclass = read_id () in 
            New(nclass)
        | "isvoid" ->
            let ivexp = read_exp () in 
            Isvoid(ivexp)
        | "plus" ->
            let x = read_exp () in 
            let y = read_exp () in 
            Plus(x, y)
        | "minus" -> 
            let x = read_exp () in 
            let y = read_exp () in 
            Minus(x, y)
        | "times" ->
            let x = read_exp () in 
            let y = read_exp () in 
            Times(x, y)
        | "divide" ->
            let x = read_exp () in 
            let y = read_exp () in
            Divide(x, y)
        | "lt" -> 
            let x = read_exp () in 
            let y = read_exp () in 
            Lt(x, y)
        | "le" ->
            let x = read_exp () in 
            let y = read_exp () in 
            Le(x, y)
        | "eq" -> 
            let x = read_exp () in 
            let y = read_exp () in
            Eq(x, y)
        | "not" ->
            let x = read_exp () in 
            Not(x)
        | "negate" -> 
            let x = read_exp () in 
            Negate(x)
        | "integer" -> 
            let ival = read () in 
            Integer(ival)
        | "string" -> 
            let sval = read () in 
            String(sval)
        | "identifier" -> 
            let idvar = read_id () in 
            Identifier(idvar)
        | "true" ->
            let bval = "true" in 
            Bool(bval)
        | "false" -> 
            let bval = "false" in 
            Bool(bval)
        | "let" -> 
            let letbinding = read_list read_let_binding in 
            let letbody = read_exp () in 
            Let(letbinding, letbody)
        | "case" ->
            let csexp = read_exp () in 
            let cselemlist = read_list read_case_elem in 
            Case(csexp, cselemlist)
        | x -> 
            failwith ("invalid expression kind: " ^ x)
        in {
            loc = eloc;
            exp_kind = ekind;
            static_type = None;
        }

    in 

    let ast = read_cool_program () in 
    close_in fin;

    (* Check for class-related errors *)

    let illegal_inherit_classes = ["Int"; "Bool"; "String"] in 
    let base_classes = [ "Int"; "Bool" ; "String"; "IO"; "Object" ] in
    let user_classes = List.map(fun ((_,cname),_,_) -> cname ) ast in 
    let all_classes = base_classes @ user_classes in 
    let all_classes = List.sort compare all_classes in 
    let inheritance = Hashtbl.create 255 in

    (* Check for missing main in Main *)
    if not (List.mem "Main" all_classes) then begin 
        printf "ERROR: 0: Type-Check: class Main not found\n";
        exit 1
    end;

    (* 
        look for inheritance from Int 
        look for inheritance from Undeclared Class
    *)
    
    List.iter (fun ((cloc, cname), inherits, features) -> 
      if List.mem cname user_classes && List.mem cname base_classes then begin (* Need to add a way to check for duplicates in user_classes - would have same*)
        printf "ERROR: %s: Type-Check: class %s redefined\n" cloc cname ;
        exit 1
      end ;
      match inherits with
        | None -> () 
        | Some(iloc, iname) -> (* inherited type identifier *)
            if List.mem iname illegal_inherit_classes then begin 
                printf "ERROR: %s: Type-Check: class %s inherits from %s\n" iloc cname iname ;
                exit 1
            end ;
            if not (List.mem iname all_classes) then begin
                printf "ERROR: %s: Type-Check: class %s inherits from unknown class %s\n" iloc cname iname ;
                exit 1
            end ;
            Hashtbl.add inheritance iname cname;
            
    ) ast;

    (* Checking for inhertance cycle *)
    let visited = ref [] in
    let cycle = ref [] in
    let rec cycle_check cname = 
      if List.exists (fun x -> x = cname ) !cycle then false
      else if List.exists (fun x -> x = cname ) !visited then true
      else begin
          cycle := cname :: !cycle;
          let res =
            if Hashtbl.mem inheritance cname then 
                let y = Hashtbl.find_all inheritance cname in
                let y = List.sort compare y in
                List.for_all (fun t -> cycle_check t) y
            else
              true
            in
            visited := cname :: !visited;
            cycle := List.tl !cycle;
            res 
      end
    in
    List.iter (fun(cname) ->
      if (cycle_check cname) = false then begin
        printf "ERROR: 0: Type-Check: inheritance cycle:" ;
        let sorted_cycle = List.sort compare !visited in
        let reverse_sorted_cycle = List.rev sorted_cycle in
        List.iter (fun (x) ->
          printf " %s" x
        ) reverse_sorted_cycle ;
        printf "\n";
        exit 1 
      end
    ) all_classes ;
    let all_classes = List.rev !visited in (* top sorted *)

    (* Check for duplicate methods *)

    (* Check for attribute redefitions *)

    (* Check for redefiniting inherited *)

    (* Check for self and SELF_TYPE errors in classes/methods *)

    (* MORE *)

    (* Error checking complete *)

    (* Emit CL-TYPE File *)

    let cltname = (Filename.chop_extension fname) ^ ".cl-type" in 
    let fout = open_out cltname in 

    let rec output_exp e =
        fprintf fout "%s\n" e.loc; 
        (match e.static_type with 
        | None -> fprintf fout ""
        | Some(Class(c)) -> fprintf fout "%s\n" c
        | Some(SELF_TYPE(c)) -> fprintf fout ""
        );
        (
        match e.exp_kind with 
        | Assign (id, exp) -> fprintf fout "assign\n"
        | Dynamic_Dispatch (exp, ddmethod, args) -> fprintf fout ""
        | Static_Dispatch (exp, sdtype, sdmethod, args) -> fprintf fout ""
        | Self_Dispatch (sdmethod, args) -> fprintf fout ""
        | If (pred, thenexp, elseexp) -> fprintf fout ""
        | While (pred, bodyexp) -> fprintf fout ""
        | Block (body) -> fprintf fout ""
        | New (newclass) -> fprintf fout ""
        | Isvoid (e) -> fprintf fout ""
        | Plus (x, y) -> fprintf fout ""
        | Minus (x, y) -> fprintf fout ""
        | Times (x, y) -> fprintf fout ""
        | Divide (x, y) -> fprintf fout ""
        | Lt (x, y) -> fprintf fout ""
        | Le (x, y) -> fprintf fout ""
        | Eq (x, y) -> fprintf fout ""
        | Not (x) -> fprintf fout ""
        | Negate (x) -> fprintf fout ""
        | Integer (ival) -> fprintf fout "integer\n%s\n" ival 
        | String (sval) -> fprintf fout "string\n%s\n" sval
        | Identifier (var) -> fprintf fout ""
        | Bool (bval) -> fprintf fout "%s\n" bval
        | Let (bindlist, body) -> fprintf fout "l"
        | Case (caseexp, elemlist) -> fprintf fout ""
        );
    in

    fprintf fout "class_map\n%d\n" (List.length all_classes);
    let sorted_all_classes = List.sort compare all_classes in 
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
    ) sorted_all_classes;

    close_out fout ; 

end;;
main () ;; 