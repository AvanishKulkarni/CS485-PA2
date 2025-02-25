(* Allen Cabrera, Avanish Kulkarni - PA2C2 *)

open Printf

type static_type = Class of string | SELF_TYPE of string

module SeenSet = Set.Make (String)

let type_to_str t = match t with Class x -> x | SELF_TYPE c -> "SELF_TYPE"

type cool_program = cool_class list
and loc = string (* actually and int *)
and name = string
and id = loc * name
and cool_type = id
and cool_class = id * id option * feature list

and feature =
  | Attribute of id * cool_type * exp option
  | Method of id * formal list * cool_type * exp

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

and binding = Binding of id * cool_type * exp option
and case_elem = Case_Elem of id * id * exp

let inheritance : (name, name) Hashtbl.t = Hashtbl.create 255

let class_map_attr : (name, id * cool_type * exp option) Hashtbl.t =
  Hashtbl.create 255

let class_map_method : (name, id * formal list * cool_type * exp) Hashtbl.t =
  Hashtbl.create 255

(* Is x a subtype of y *)
let is_subtype x y =
  match (x, y) with
  | x, y when x = y -> true (* same type *)
  | x, "Object" -> true (* subtype object *)
  | x, y ->
      let rec dfs_helper vert =
        (* checking inheritance map *)
        if vert = x then true
        else
          let children = Hashtbl.find_all inheritance vert in
          List.exists dfs_helper children
      in
      dfs_helper y

let main () =
  (* De-serialize CL-AST file *)
  let fname = Sys.argv.(1) in
  let fin = open_in fname in

  let rec range k = if k <= 0 then [] else k :: range (k - 1) in

  let read () = input_line fin in

  let read_list worker =
    let k = int_of_string (read ()) in
    let lst = range k in
    List.map (fun _ -> worker ()) lst
  in

  let rec read_cool_program () = read_list read_cool_class
  and read_id () =
    let loc = read () in
    let name = read () in
    (loc, name)
  and read_cool_class () =
    (* Class *)
    let cname = read_id () in
    let inherits =
      match read () with
      | "no_inherits" -> None
      | "inherits" ->
          let super = read_id () in
          Some super
      | x -> failwith ("cannot happen: " ^ x)
    in
    let features = read_list read_feature in
    (cname, inherits, features)
  and read_feature () =
    match read () with
    | "attribute_no_init" ->
        let fname = read_id () in
        let ftype = read_id () in
        Attribute (fname, ftype, None)
    | "attribute_init" ->
        let fname = read_id () in
        let ftype = read_id () in
        let finit = read_exp () in
        Attribute (fname, ftype, Some finit)
    | "method" ->
        let mname = read_id () in
        let formals = read_list read_formal in
        let mtype = read_id () in
        let mbody = read_exp () in
        Method (mname, formals, mtype, mbody)
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
        Binding (letvar, lettype, None)
    | "let_binding_init" ->
        let letvar = read_id () in
        let lettype = read_id () in
        let letval = read_exp () in
        Binding (letvar, lettype, Some letval)
    | x -> failwith ("impossible binding " ^ x)
  and read_case_elem () =
    let csid = read_id () in
    let cstype = read_id () in
    let csbody = read_exp () in
    Case_Elem (csid, cstype, csbody)
  and read_exp () =
    let eloc = read () in
    let ekind =
      match read () with
      (* do the rest of the types *)
      | "assign" ->
          let avar = read_id () in
          let aexp = read_exp () in
          Assign (avar, aexp)
      | "dynamic_dispatch" ->
          let ddexp = read_exp () in
          let ddmethod = read_id () in
          let ddargs = read_list read_exp in
          Dynamic_Dispatch (ddexp, ddmethod, ddargs)
      | "static_dispatch" ->
          let sdexp = read_exp () in
          let sdid = read_id () in
          let sdmethod = read_id () in
          let sdargs = read_list read_exp in
          Static_Dispatch (sdexp, sdid, sdmethod, sdargs)
      | "self_dispatch" ->
          let sdmethod = read_id () in
          let sdargs = read_list read_exp in
          Self_Dispatch (sdmethod, sdargs)
      | "if" ->
          let ipred = read_exp () in
          let ithen = read_exp () in
          let ielse = read_exp () in
          If (ipred, ithen, ielse)
      | "while" ->
          let wpred = read_exp () in
          let wbody = read_exp () in
          While (wpred, wbody)
      | "block" ->
          let bbody = read_list read_exp in
          Block bbody
      | "new" ->
          let nclass = read_id () in
          New nclass
      | "isvoid" ->
          let ivexp = read_exp () in
          Isvoid ivexp
      | "plus" ->
          let x = read_exp () in
          let y = read_exp () in
          Plus (x, y)
      | "minus" ->
          let x = read_exp () in
          let y = read_exp () in
          Minus (x, y)
      | "times" ->
          let x = read_exp () in
          let y = read_exp () in
          Times (x, y)
      | "divide" ->
          let x = read_exp () in
          let y = read_exp () in
          Divide (x, y)
      | "lt" ->
          let x = read_exp () in
          let y = read_exp () in
          Lt (x, y)
      | "le" ->
          let x = read_exp () in
          let y = read_exp () in
          Le (x, y)
      | "eq" ->
          let x = read_exp () in
          let y = read_exp () in
          Eq (x, y)
      | "not" ->
          let x = read_exp () in
          Not x
      | "negate" ->
          let x = read_exp () in
          Negate x
      | "integer" ->
          let ival = read () in
          Integer ival
      | "string" ->
          let sval = read () in
          String sval
      | "identifier" ->
          let idvar = read_id () in
          Identifier idvar
      | "true" ->
          let bval = "true" in
          Bool bval
      | "false" ->
          let bval = "false" in
          Bool bval
      | "let" ->
          let letbinding = read_list read_let_binding in
          let letbody = read_exp () in
          Let (letbinding, letbody)
      | "case" ->
          let csexp = read_exp () in
          let cselemlist = read_list read_case_elem in
          Case (csexp, cselemlist)
      | x -> failwith ("invalid expression kind: " ^ x)
    in
    { loc = eloc; exp_kind = ekind; static_type = None }
  in

  let ast = read_cool_program () in
  close_in fin;

  (* Check for class-related errors *)

  (*
    OUTLINE - 
        BLOCK 0
        - class named SELF_TYPE
        - redef base classes 
        - class redef
        - duplicate classes 
        - inherits from illegal class
        - inherits from unknown class
        - missing main class
        - inheritance cycle

        BLOCK 1
        - attribute redefinition 
        - method redefinition 
        - duplicate formal params in methods 

        BLOCK 2
        - attribute of unknown type
        - attribute named self
        - inherited redefined attribute
        - duplicate attribute
        - method with formals of unknown type
        - method with unknown return type
        - redef method return type 
        - redef method formal number
        - redef method formal type 

        Remaining
        - main method not found 
        - main method with 0 params not found 
    
    
    *)
  let illegal_inherit_classes = [ "Int"; "Bool"; "String" ] in
  let base_classes = [ "Int"; "Bool"; "String"; "IO"; "Object" ] in
  let user_classes = List.map (fun ((_, cname), _, _) -> cname) ast in
  let all_classes = base_classes @ user_classes in
  let all_classes = List.sort compare all_classes in
  let seen = ref SeenSet.empty in

  Hashtbl.add inheritance "Object" "Int";
  Hashtbl.add inheritance "Object" "Bool";
  Hashtbl.add inheritance "Object" "String";
  Hashtbl.add inheritance "Object" "IO";

  Hashtbl.add class_map_method "Object"
    ( ("0", "abort"),
      [],
      ("0", "Object"),
      { loc = "0"; exp_kind = ObjectMethod ""; static_type = None } );
  Hashtbl.add class_map_method "Object"
    ( ("0", "type_name"),
      [],
      ("0", "String"),
      { loc = "0"; exp_kind = ObjectMethod ""; static_type = None } );
  Hashtbl.add class_map_method "Object"
    ( ("0", "copy"),
      [],
      ("0", "SELF_TYPE"),
      { loc = "0"; exp_kind = ObjectMethod ""; static_type = None } );

  Hashtbl.add class_map_method "IO"
    ( ("0", "out_string"),
      [ (("0", "x"), ("0", "String")) ],
      ("0", "SELF_TYPE"),
      { loc = "0"; exp_kind = IOMethod ""; static_type = None } );
  Hashtbl.add class_map_method "IO"
    ( ("0", "out_int"),
      [ (("0", "x"), ("0", "Int")) ],
      ("0", "SELF_TYPE"),
      { loc = "0"; exp_kind = IOMethod ""; static_type = None } );
  Hashtbl.add class_map_method "IO"
    ( ("0", "in_string"),
      [],
      ("0", "String"),
      { loc = "0"; exp_kind = IOMethod ""; static_type = None } );
  Hashtbl.add class_map_method "IO"
    ( ("0", "in_int"),
      [],
      ("0", "Int"),
      { loc = "0"; exp_kind = IOMethod ""; static_type = None } );

  Hashtbl.add class_map_method "String"
    ( ("0", "length"),
      [],
      ("0", "Int"),
      { loc = "0"; exp_kind = StringMethod ""; static_type = None } );
  Hashtbl.add class_map_method "String"
    ( ("0", "concat"),
      [ (("0", "s"), ("0", "String")) ],
      ("0", "String"),
      { loc = "0"; exp_kind = StringMethod ""; static_type = None } );
  Hashtbl.add class_map_method "String"
    ( ("0", "substr"),
      [ (("0", "i"), ("0", "Int")); (("0", "l"), ("0", "Int")) ],
      ("0", "String"),
      { loc = "0"; exp_kind = StringMethod ""; static_type = None } );

  (* 
        look for inheritance from Int 
        look for inheritance from Undeclared Class
    *)

  (* BLOCK 0 BEGIN *)
  List.iter
    (fun ((cloc, cname), inherits, features) ->
      if cname = "SELF_TYPE" then (
        printf "ERROR: %s: Type-Check: class named SELF_TYPE\n" cloc;
        exit 1);
      if List.mem cname user_classes && List.mem cname base_classes then (
        printf "ERROR: %s: Type-Check: class %s redefined\n" cloc cname;
        exit 1);

      (* Check for duplicate classes *)
      if SeenSet.mem cname !seen then (
        printf "ERROR: %s: Type-Check: class %s redefined\n" cloc cname;
        exit 1);

      seen := SeenSet.add cname !seen;
      (* iterate through features, match to attribute or method, add to hashtbl *)
      (match features with
      | [] -> printf ""
      | lst ->
          List.iter
            (fun feat ->
              match feat with
              | Attribute (aid, atype, Some aexp) ->
                  Hashtbl.add class_map_attr cname (aid, atype, Some aexp)
              | Attribute (aid, atype, None) ->
                  Hashtbl.add class_map_attr cname (aid, atype, None)
              | Method (mid, formal_list, mtype, mexp) ->
                  Hashtbl.add class_map_method cname
                    (mid, formal_list, mtype, mexp))
            lst);
      match inherits with
      | None -> Hashtbl.add inheritance "Object" cname
      (* inherits from Object by default *)
      | Some (iloc, iname) ->
          (* inherited type identifier *)
          if List.mem iname illegal_inherit_classes then (
            printf "ERROR: %s: Type-Check: class %s inherits from %s\n" iloc
              cname iname;
            exit 1);
          if not (List.mem iname all_classes) then (
            printf
              "ERROR: %s: Type-Check: class %s inherits from unknown class %s\n"
              iloc cname iname;
            exit 1);
          Hashtbl.add inheritance iname cname)
    ast;
  (* Check for missing main in Main *)
  if not (List.mem "Main" all_classes) then (
    printf "ERROR: 0: Type-Check: class Main not found\n";
    exit 1);

  (* Checking for inhertance cycle *)
  let visited = ref [] in
  let cycle = ref [] in
  let rec cycle_check cname =
    if List.exists (fun x -> x = cname) !cycle then false
    else if List.exists (fun x -> x = cname) !visited then true
    else (
      cycle := cname :: !cycle;
      let res =
        if Hashtbl.mem inheritance cname then
          let y = Hashtbl.find_all inheritance cname in
          let y = List.sort compare y in
          List.for_all (fun t -> cycle_check t) y
        else true
      in
      visited := cname :: !visited;
      cycle := List.tl !cycle;
      res)
  in
  List.iter
    (fun cname ->
      if cycle_check cname = false then (
        printf "ERROR: 0: Type-Check: inheritance cycle:";
        let sorted_cycle = List.sort compare !visited in
        let reverse_sorted_cycle = List.rev sorted_cycle in
        List.iter (fun x -> printf " %s" x) reverse_sorted_cycle;
        printf "\n";
        exit 1))
    all_classes;

  (* BLOCK 0 END *)

  (* BLOCK 1 BEGIN *)
  (* Check for duplicate attributes, methods with each class *)
  (* Check for duplicates in formal list *)
  List.iter
    (fun cname ->
      let attr_list = Hashtbl.find_all class_map_attr cname in
      let attr_seen = ref SeenSet.empty in

      (* Check if attribute redefined *)
      List.iter
        (fun ((loc, name), _, _) ->
          if SeenSet.mem name !attr_seen then (
            printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n"
              loc cname name;
            exit 1);
          attr_seen := SeenSet.add name !attr_seen)
        (List.rev attr_list);
      let meth_list = Hashtbl.find_all class_map_method cname in
      let meth_seen = ref SeenSet.empty in

      (* Check for redefined method *)
      List.iter
        (fun ((loc, mname), formal_list, _, _) ->
          if SeenSet.mem mname !meth_seen then (
            printf "ERROR: %s: Type-Check: class %s redefines method %s\n" loc
              cname mname;
            exit 1)
          else
            (* Check for duplicates in formal list *)
            let formal_seen = ref SeenSet.empty in
            List.iter
              (fun ((loc, pname), _) ->
                if SeenSet.mem pname !formal_seen then (
                  printf
                    "ERROR: %s: Type-Check: class %s has method %s with \
                     duplicate formal parameter named %s\n"
                    loc cname mname pname;
                  exit 1);
                formal_seen := SeenSet.add pname !formal_seen)
              formal_list;
            meth_seen := SeenSet.add mname !meth_seen)
        (List.rev meth_list))
    all_classes;

  (* BLOCK 1 END *)

  (* BLOCK 2 BEGIN *)
  (* Type Checking features *)
  let rec feature_check iname cname =
    let inherited_methods = Hashtbl.find_all class_map_method iname in
    let inherited_attributes = Hashtbl.find_all class_map_attr iname in
    let inherited_attributes_names =
      List.map (fun ((_, name), _, _) -> name) inherited_attributes
    in
    let methods = Hashtbl.find_all class_map_method cname in
    let attributes = Hashtbl.find_all class_map_attr cname in
    (* Type checking attributes *)
    List.iter
      (fun ((aloc, name), (tloc, tname), _) ->
        (* Checks for attributes called self *)
        if (not (List.mem tname all_classes)) && tname <> "SELF_TYPE" then (
          printf
            "ERROR: %s: Type-Check: class %s has attribute %s with unknown \
             type %s\n"
            tloc cname name tname;
          exit 1);
        if name = "self" then (
          printf "ERROR: %s: Type-Check: class %s has an attribute named self\n"
            aloc cname;
          exit 1);
        if List.mem name inherited_attributes_names then (
          printf "ERROR: %s: Type-Check: class %s redefines attribute %s\n" aloc
            cname name;
          exit 1))
      (List.rev attributes);
    List.iter
      (fun ((mloc, mname), formal_list, (typeloc, mtype), _) ->
        (* Checks each formal parameter to see if the type exists *)
        List.iter
          (fun ((floc, fname), (ftloc, ftype)) ->
            if not (List.mem ftype all_classes) then (
              printf
                "ERROR: %s: Type-Check: class %s has method %s with formal \
                 parameter of unknown type %s\n"
                ftloc cname mname ftype;
              exit 1);
            if fname = "self" then (
              printf
                "ERROR: %s: Type-Check: class %s has method %s with formal \
                 parameter named self\n"
                floc cname mname;
              exit 1))
          formal_list;

        (* Checks the return type to see if the type exists *)
        if (not (List.mem mtype all_classes)) && mtype <> "SELF_TYPE" then (
          printf
            "ERROR: %s: Type-Check: class %s has method %s with unknown return \
             type %s\n"
            typeloc cname mname mtype;
          exit 1);
        List.iter
          (fun ((imloc, imname), iformal_list, (itypeloc, imtype), _) ->
            (* if (mname = imname) then begin *)
            if is_subtype mname imname then (
              (* Checking for inherited method redefinitions*)
              (* Checking for return type change *)
              if mtype <> imtype then (
                printf
                  "ERROR: %s: Type-Check: class %s redefines method %s and \
                   changes return type (from %s to %s)\n"
                  typeloc cname mname imtype mtype;
                exit 1);
              (* Checking for # of formals change *)
              if List.length formal_list <> List.length iformal_list then (
                printf
                  "ERROR: %s: Type-Check: class %s redefines method %s and \
                   changes number of formals\n"
                  mloc cname mname;
                exit 1);
              (* Checking if the formal types were changed *)
              List.iter2
                (fun ((fploc, fname), (ftloc, ftype)) ((_, ifname), (_, iftype))
                   ->
                  if ftype <> iftype then (
                    printf
                      "ERROR: %s: Type-Check: class %s redefines method %s and \
                       changes type of formal %s\n"
                      ftloc cname mname fname;
                    exit 1))
                formal_list iformal_list))
          inherited_methods)
      methods;
    (* If no errors are found, then add inherited features to the class map*)
    List.iter (fun _ -> Hashtbl.remove class_map_method cname) methods;
    List.iter
      (fun meth -> Hashtbl.add class_map_method cname meth)
      inherited_methods;
    List.iter (fun meth -> Hashtbl.add class_map_method cname meth) methods;
    List.iter (fun _ -> Hashtbl.remove class_map_attr cname) attributes;
    List.iter
      (fun attr -> Hashtbl.add class_map_attr cname attr)
      (List.rev inherited_attributes);
    List.iter
      (fun attr -> Hashtbl.add class_map_attr cname attr)
      (List.rev attributes);
    let child_classes = Hashtbl.find_all inheritance cname in
    List.iter (fun cl -> feature_check cname cl) child_classes
  in
  List.iter
    (fun cl -> feature_check "Object" cl)
    (Hashtbl.find_all inheritance "Object");

  (* BLOCK 2 END *)
  (* Check for main() method in Main or inherited classes*)
  let main_methods = Hashtbl.find_all class_map_method "Main" in
  let method_names = List.map (fun ((_, name), _, _, _) -> name) main_methods in
  if not (List.mem "main" method_names) then (
    printf "ERROR: 0: Type-Check: class Main method main not found\n";
    exit 1);
  (* Checking to see if the main method has zero parameters *)
  List.iter
    (fun ((_, name), formals, _, _) ->
      if name = "main" && List.length formals <> 0 then (
        printf
          "ERROR: 0: Type-Check: class Main method main with 0 parameters not \
           found\n";
        exit 1))
    (Hashtbl.find_all class_map_method "Main");
  let all_classes = List.rev !visited in
  (* top sorted *)

  (* List.iter (fun (cname) -> 
        printf "%s " cname;
    ) all_classes;
    printf "\n"; *)
  (* List.iter (fun ((_, aname), _, _) ->
        printf "%s" aname;
        ) (Hashtbl.find_all class_map_attr "Weird"); *)

  (* Check for self and SELF_TYPE errors in classes/methods *)
  (* Hashtbl.iter (fun (aclass) (_,_,_) -> 
        printf "%s: " aclass;
        let attributes = Hashtbl.find_all class_map_attr aclass in 
        List.iter (fun ((_,aname),_,_) -> 
            printf "%s " aname    
        ) attributes;
        printf "\n";
    ) class_map_attr; *)
  (* Hashtbl.iter (fun (aclass) (_,_,_, _) -> 
        printf "%s: " aclass;
        let methods = Hashtbl.find_all class_map_method aclass in 
        List.iter (fun ((_,aname),_,_, _) -> 
            printf "%s " aname    
        ) methods;
        printf "\n";
    ) class_map_method; *)
  (* MORE *)

  (* Error checking complete *)

  (* Emit CL-TYPE File *)
  let cltname = Filename.chop_extension fname ^ ".cl-type" in
  let fout = open_out cltname in

  let rec output_exp e =
    fprintf fout "%s\n" e.loc;
    (match e.static_type with
    | None -> fprintf fout ""
    | Some (Class c) -> fprintf fout "%s\n" c
    | Some (SELF_TYPE c) -> fprintf fout "");
    match e.exp_kind with
    | Assign ((id_loc, exp_name), exp) ->
        fprintf fout "assign\n%s\n%s\n" id_loc exp_name;
        output_exp exp
    | Dynamic_Dispatch (exp, (meth_loc, meth_name), args) ->
        fprintf fout "dynamic_dispatch\n";
        output_exp exp;
        fprintf fout "%s\n%s\n%d\n" meth_loc meth_name (List.length args);
        List.iter (fun exp -> output_exp exp) args
    | Static_Dispatch (exp, (stcl_loc, stcl_name), (sdmt_loc, sdmt_name), args)
      ->
        fprintf fout "static_dispatch\n";
        output_exp exp;
        fprintf fout "%s\n%s\n" stcl_loc stcl_name;
        fprintf fout "%s\n%s\n%d\n" sdmt_loc sdmt_name (List.length args);
        List.iter (fun exp -> output_exp exp) args
    | Self_Dispatch ((sdloc, sdname), args) ->
        fprintf fout "self_dispatch\n%s\n%s\n%d\n" sdloc sdname
          (List.length args);
        List.iter (fun exp -> output_exp exp) args
    | If (pred, thenexp, elseexp) ->
        fprintf fout "if\n";
        output_exp pred;
        output_exp thenexp;
        output_exp elseexp
    | While (pred, bodyexp) ->
        fprintf fout "while\n";
        output_exp pred;
        output_exp bodyexp
    | Block body ->
        fprintf fout "block\n%d\n" (List.length body);
        List.iter (fun exp -> output_exp exp) body
    | New (loc, name) -> fprintf fout "new\n%s\n%s\n" loc name
    | Isvoid e ->
        fprintf fout "isvoid\n";
        output_exp e
    | Plus (x, y) ->
        fprintf fout "plus\n";
        output_exp x;
        output_exp y
    | Minus (x, y) ->
        fprintf fout "minus\n";
        output_exp x;
        output_exp y
    | Times (x, y) ->
        fprintf fout "times\n";
        output_exp x;
        output_exp y
    | Divide (x, y) ->
        fprintf fout "divide\n";
        output_exp x;
        output_exp y
    | Lt (x, y) ->
        fprintf fout "lt\n";
        output_exp x;
        output_exp y
    | Le (x, y) ->
        fprintf fout "le\n";
        output_exp x;
        output_exp y
    | Eq (x, y) ->
        fprintf fout "eq\n";
        output_exp x;
        output_exp y
    | Not x ->
        fprintf fout "not\n";
        output_exp x
    | Negate x ->
        fprintf fout "negate\n";
        output_exp x
    | Integer ival -> fprintf fout "integer\n%s\n" ival
    | String sval -> fprintf fout "string\n%s\n" sval
    | Identifier (loc, name) -> fprintf fout "identifier\n%s\n%s\n" loc name
    | Bool bval -> fprintf fout "%s\n" bval
    | Let (bindlist, body) ->
        fprintf fout "let\n%d\n" (List.length bindlist);
        List.iter
          (fun (b : binding) ->
            match b with
            | Binding ((bloc, bname), (tloc, tname), Some exp) ->
                fprintf fout "let_binding_init\n%s\n%s\n%s\n%s\n" bloc bname
                  tloc tname;
                output_exp exp
            | Binding ((bloc, bname), (tloc, tname), None) ->
                fprintf fout "let_binding_no_init\n%s\n%s\n%s\n%s\n" bloc bname
                  tloc tname)
          bindlist;
        output_exp body
    | Case (caseexp, elemlist) ->
        fprintf fout "case\n";
        output_exp caseexp;
        fprintf fout "%d\n" (List.length elemlist);
        List.iter
          (fun (Case_Elem ((cid, cname), (ctid, ctname), exp)) ->
            fprintf fout "%s\n%s\n%s\n%s\n" cid cname ctid ctname;
            output_exp exp)
          elemlist
    | StringMethod _ -> ()
    | IOMethod _ -> ()
    | ObjectMethod _ -> ()
  in

  let sorted_all_classes = List.sort compare all_classes in

  (* Function to print class map *)
  let print_class_map fout sorted_classes class_map_attr output_exp =
    fprintf fout "class_map\n%d\n" (List.length all_classes);

    List.iter
      (fun cname ->
        fprintf fout "%s\n" cname;
        let attributes = Hashtbl.find_all class_map_attr cname in
        fprintf fout "%d\n" (List.length attributes);
        List.iter
          (fun attr ->
            match attr with
            | (_, aname), (_, atype), None ->
                fprintf fout "no_initializer\n%s\n%s\n" aname atype
            | (_, aname), (_, atype), Some init ->
                fprintf fout "initializer\n%s\n%s\n" aname atype;
                output_exp init)
          (List.rev attributes)
        (* Attributes are stored in reverse order due to how insertion into hash tables work*))
      sorted_all_classes
  in

  (* Function to print implementation map *)
  let print_impl_map fout sorted_classes class_map_method output_exp =
    fprintf fout "implementation_map\n%d\n" (List.length all_classes);
    List.iter
      (fun cname ->
        fprintf fout "%s\n" cname;
        let methods = Hashtbl.find_all class_map_method cname in
        fprintf fout "%d\n" (List.length methods);
        List.iter
          (fun meth ->
            let (_, mname), mformals, _, _ = meth in
            fprintf fout "%s\n%d\n" mname (List.length mformals);
            List.iter
              (fun ((_, fmname), _) ->
                fprintf fout "%s\n" fmname;
                fprintf fout "%s\n" cname;
                (* TODO: If this method is inherited but NOT OVERIDDEN 
              Output the name of the highest parent class that defined 
              the method body expression, otherwise output current class *)
                let dummy_exp =
                  (* TODO: output actual method body expression *)
                  {
                    loc = "0";
                    exp_kind = Integer "0";
                    static_type = Some (Class "Int");
                  }
                in
                output_exp dummy_exp)
              mformals)
          (List.rev methods))
      sorted_all_classes
  in

  let print_parent_map sorted_classes inheritance =
    fprintf fout "parent_map\n%d\n" (Hashtbl.length inheritance)
  in

  (* No arguments provided *)
  let args_array = Array.to_list Sys.argv in
  (match args_array with
  | [ prog ] -> printf "Usage: %s program.cl-ast\n" prog
  | [ prog; coolprog ] ->
      (* TEMP SET to only print class map for PA2C2 *)
      print_class_map fout sorted_all_classes class_map_attr output_exp
  (* print_impl_map fout sorted_all_classes class_map_method output_exp;
      print_parent_map sorted_all_classes inheritance *)
  | [ prog; coolprog; arg ] when arg = "--class-map" ->
      print_class_map fout sorted_all_classes class_map_attr output_exp
  | [ prog; coolprog; arg ] when arg = "--parent-map" ->
      print_parent_map sorted_all_classes inheritance
  | [ prog; coolprog; arg ] when arg = "--imp-map" ->
      print_impl_map fout sorted_all_classes class_map_method output_exp
  | _ -> printf "something went very wrong\n");
  close_out fout
;;

main ()
