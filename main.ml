(* Allen Cabrera, Avanish Kulkarni - PA2C2 *)

open Printf

type static_type = 
| Class of string 
| SELF_TYPE of string 
and obj_ident = string
and meth_name = string
and meth_sig = static_type list 

let type_to_str t = match t with 
| Class(x) -> x
| SELF_TYPE(c) -> "SELF_TYPE"

let object_env : (obj_ident, static_type) Hashtbl.t = Hashtbl.create 255
let method_env : ((static_type * meth_name), meth_sig) Hashtbl.t = Hashtbl.create 255

let has_duplicates (lst) : static_type option = 
    let tbl = Hashtbl.create (List.length lst) in 
    let rec helper = function 
    | [] -> None
    | hd :: tl ->
        if Hashtbl.mem tbl hd then Some hd
        else (
            Hashtbl.add tbl hd ();
            helper tl
        )
    in helper lst 



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
    | IOMethod of string
    | ObjectMethod of string
    | StringMethod of string
and binding = 
    | Binding of id * cool_type * (exp option)
and case_elem = 
    | Case_Elem of id * id * exp

let parent_map : (static_type, static_type) Hashtbl.t = Hashtbl.create 255
let class_map_attr : (static_type, feature) Hashtbl.t = Hashtbl.create 255
let class_map_method : (static_type, feature) Hashtbl.t = Hashtbl.create 255

let rec is_subtype t1 t2 = 
    match t1, t2 with 
    | Class(x), Class(y) when x = y -> true
    | Class(x), Class("Object") -> true
    | Class(x), Class(y) -> (* check parent map *)
        let rec dfs_helper vert = (* checking inheritance map *)
            if vert = Class(y) then true
            else
                let children = Hashtbl.find_all parent_map vert in
                List.exists dfs_helper children
            in 
            dfs_helper (Class x)
    | _, _ -> false (* self type behavior *)


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
    and read_cool_class () : cool_class = (* Class *)
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

    let illegal_inherit_classes = [Class "Int"; Class "Bool"; Class "String"] in
    let base_classes = [Class "Int"; Class "Bool" ; Class "String"; Class "IO"; Class "Object"] in 
    let user_classes = List.map(fun ((_,cname),_,_) -> Class cname ) ast in 
    let all_classes = base_classes @ user_classes in 
    let all_classes = List.sort compare all_classes in 

    Hashtbl.add parent_map (Class "Object") (Class "Int");
    Hashtbl.add parent_map (Class "Object") (Class "Bool");
    Hashtbl.add parent_map (Class "Object") (Class "String");
    Hashtbl.add parent_map (Class "Object") (Class "IO");

    (* Object methods *)
    let signature = (Class "Object", "abort") in
    let formals = [Class "Object"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "Object", "type_name") in
    let formals = [Class "String"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "Object", "copy") in
    let formals = [Class "SELF_TYPE"] in
    Hashtbl.add method_env signature formals;

    (* IO Methods *)
    let signature = (Class "IO", "out_string") in
    let formals = [Class "String"; Class "SELF_TYPE"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "IO", "out_int") in
    let formals = [Class "Int"; Class "SELF_TYPE"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "IO", "in_string") in
    let formals = [Class "String"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "IO", "in_int") in
    let formals = [Class "Int"] in
    Hashtbl.add method_env signature formals;
    
    (* String Methods *)
    let signature = (Class "String", "length") in
    let formals = [Class "Int"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "String", "concat") in
    let formals = [Class "String"; Class "String"] in
    Hashtbl.add method_env signature formals;

    let signature = (Class "String", "substr") in
    let formals = [Class "Int"; Class "Int"; Class "String"] in
    Hashtbl.add method_env signature formals;
    
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
        | Assign ((id_loc, exp_name), exp) -> 
            fprintf fout "assign\n%s\n%s\n" id_loc exp_name; output_exp exp;
        | Dynamic_Dispatch (exp, (meth_loc, meth_name), args) -> 
            fprintf fout "dynamic_dispatch\n";
            output_exp exp;
            fprintf fout "%s\n%s\n%d\n" meth_loc meth_name (List.length args);
            List.iter (fun exp -> output_exp exp;) args;
        | Static_Dispatch (exp, (stcl_loc, stcl_name), (sdmt_loc, sdmt_name), args) -> 
            fprintf fout "static_dispatch\n";
            output_exp exp;
            fprintf fout "%s\n%s\n" stcl_loc stcl_name;
            fprintf fout "%s\n%s\n%d\n" sdmt_loc sdmt_name (List.length args);
            List.iter (fun exp -> output_exp exp;) args;
        | Self_Dispatch ((sdloc, sdname), args) -> 
            fprintf fout "self_dispatch\n%s\n%s\n%d\n" sdloc sdname (List.length args);
            List.iter (fun exp -> output_exp exp;) args;
        | If (pred, thenexp, elseexp) -> fprintf fout "if\n"; output_exp pred; output_exp thenexp; output_exp elseexp;
        | While (pred, bodyexp) -> fprintf fout "while\n"; output_exp pred; output_exp bodyexp;
        | Block (body) -> fprintf fout "block\n"; List.iter (fun exp -> output_exp exp;) body;
        | New ((loc, name)) -> fprintf fout "new\n%s\n%s\n" loc name
        | Isvoid (e) -> fprintf fout "isvoid\n"; output_exp e;
        | Plus (x, y) -> fprintf fout "plus\n"; output_exp x; output_exp y;
        | Minus (x, y) -> fprintf fout "minus\n"; output_exp x; output_exp y;
        | Times (x, y) -> fprintf fout "times\n"; output_exp x; output_exp y;
        | Divide (x, y) -> fprintf fout "divide\n"; output_exp x; output_exp y;
        | Lt (x, y) -> fprintf fout "lt\n"; output_exp x; output_exp y;
        | Le (x, y) -> fprintf fout "le\n"; output_exp x; output_exp y;
        | Eq (x, y) -> fprintf fout "eq\n"; output_exp x; output_exp y;
        | Not (x) -> fprintf fout "not\n"; output_exp x;
        | Negate (x) -> fprintf fout "negate\n"; output_exp x;
        | Integer (ival) -> fprintf fout "integer\n%s\n" ival 
        | String (sval) -> fprintf fout "string\n%s\n" sval
        | Identifier (loc, name) -> fprintf fout "identifier\n%s\n%s\n" loc name
        | Bool (bval) -> fprintf fout "%s\n" bval
        | Let (bindlist, body) -> 
            fprintf fout "let\n%d\n" (List.length bindlist);
            List.iter (fun (b: binding)->
                match b with 
                | Binding((bloc, bname), (tloc, tname), Some(exp)) -> 
                    fprintf fout "let_binding_init\n%s\n%s\n%s\n%s\n" bloc bname tloc tname;
                    output_exp exp;
                | Binding((bloc, bname), (tloc, tname), None) -> 
                    fprintf fout "let_binding_no_init\n%s\n%s\n%s\n%s\n" bloc bname tloc tname;
            ) bindlist;
            output_exp body;
        | Case (caseexp, elemlist) -> 
            fprintf fout "case\n";
            output_exp caseexp;
            fprintf fout "%d\n" (List.length elemlist);
            List.iter (fun (Case_Elem((cid, cname), (ctid, ctname), exp)) -> 
                fprintf fout "%s\n%s\n%s\n%s\n" cid cname ctid ctname;
                output_exp exp;
            ) elemlist;
        | StringMethod (_) -> ()
        | IOMethod (_) -> ()
        | ObjectMethod (_) -> ()
        );
    in

    fprintf fout "class_map\n%d\n" (List.length all_classes);
    let sorted_all_classes = List.sort compare all_classes in 
    List.iter (fun class_type -> 
        let cname = match class_type with | Class s -> s | SELF_TYPE s -> s in
        fprintf fout "%s\n" cname ;
        let attributes = 
            try 
                let _, inherits, features = List.find (fun ((_,cname2),_,_) -> (Class cname) = (Class cname2)) ast in 
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