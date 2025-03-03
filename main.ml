(* Allen Cabrera, Avanish Kulkarni - PA2C2 *)

open Printf

type static_type =
  | Class of string (* ex "Int" or "Object" *)
  | SELF_TYPE of string

module SeenSet = Set.Make (String)

let type_to_str t =
  match t with Class x -> x | SELF_TYPE c -> "SELF_TYPE(" ^ c ^ ")"

type cool_program = cool_class list
and loc = int
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
  | Integer of int
  | String of string
  | Identifier of id
  | Bool of string (* bool *)
  | Let of binding list * exp
  | Case of exp * case_elem list
  | Internal of
      string
      * string
      * string (* return class, class its defined in, method name *)

and binding = Binding of id * cool_type * exp option
and case_elem = Case_Elem of id * cool_type * exp

let inheritance : (name, name) Hashtbl.t = Hashtbl.create 255

let class_map_attr : (name, id * cool_type * exp option) Hashtbl.t =
  Hashtbl.create 255

let class_map_method :
    (name, id * formal list * cool_type * exp * name) Hashtbl.t =
  Hashtbl.create 255

(*defined this way so we can make arbitrary new object_envs *)
type obj_env = (name, static_type) Hashtbl.t
type meth_env = (static_type * name, name list) Hashtbl.t

let global_obj_env : obj_env = Hashtbl.create 255
let global_meth_env : meth_env = Hashtbl.create 255

(* Is x a subtype of y *)
let is_subtype (x : static_type) (y : static_type) =
  let x = match x with Class c -> c | SELF_TYPE c -> c in
  let y = match y with Class c -> c | SELF_TYPE c -> c in

  match (x, y) with
  | x, y when x = y -> true (* same type *)
  | x, "Object" -> true (* subtype of object *)
  | x, y when x = "Void" && y = "Void" -> true
  | x, y when x = "SELF_TYPE" && y = "SELF_TYPE" -> true
  | x, y when x = "Void" && y = "SELF_TYPE" -> true
  | x, y when x = "SELF_TYPE" && y = "Void" -> true
  | x, y ->
      let rec dfs_helper vert =
        (* checking inheritance map *)
        if vert = x then true
        else
          let children = Hashtbl.find_all inheritance vert in
          List.exists dfs_helper children
      in
      dfs_helper y

let find_parent cname inheritance =
  Hashtbl.fold
    (fun parent v acc -> if v = cname then Some parent else acc)
    inheritance None

let least_upper_bound (x : static_type) (y : static_type) =
  match (x, y) with
  | Class "Object", _ -> Class "Object"
  | _, Class "Object" -> Class "Object"
  | _, _ ->
      let xname = type_to_str x in
      let yname = type_to_str y in
      let rec goto_root (node : string) =
        match find_parent node inheritance with
        | None -> [ node ]
        | Some parent -> node :: goto_root parent
      in

      let path_x = List.rev (goto_root xname) in
      let path_y = List.rev (goto_root yname) in

      (* start at root, go down until divergence *)
      let rec find_common px py =
        match (px, py) with
        | hx :: tx, hy :: ty when hx = hy -> hx :: find_common tx ty
        | _ -> []
      in
      let rec grab_last lst =
        match lst with
        | [] ->
            failwith
              (sprintf "Can't find lub for %s and %s. This should never happen."
                 xname yname)
        | [ x ] -> x
        | _ :: tl -> grab_last tl
      in
      let common = grab_last (find_common path_x path_y) in
      Class common

let main () =
  (* De-serialize CL-AST file *)
  let args_array = Array.to_list Sys.argv in
  if List.length args_array < 2 then (
    printf "COOL Semantic Checker (by Allen Cabrera, Avanish Kulkarni)\n\n";
    printf "Usage: %s source.cl-ast [options]\n\n" Sys.argv.(0);
    printf
      "  --class-map\tstop after type checking (produce source.cl-type class \
       map file)\n";
    printf
      "  --imp-map\tstop after type checking (produce source.cl-type imp map \
       file)\n";
    printf
      "  --parent-map\tstop after type checking (produce source.cl-type parent \
       map file)\n";

    exit 1);

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
    let loc = int_of_string (read ()) in
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
    let eloc = int_of_string (read ()) in
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
          Integer (int_of_string ival)
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

  (* BEGIN Check for class-related errors *)
  let illegal_inherit_classes = [ "Int"; "Bool"; "String" ] in
  let base_classes = [ "Int"; "Bool"; "String"; "IO"; "Object" ] in
  let user_classes = List.map (fun ((_, cname), _, _) -> cname) ast in
  let all_classes = base_classes @ user_classes in
  let all_classes = List.sort compare all_classes in
  let seen = ref SeenSet.empty in

  (* Add all built-in classes and their methods to Hashtbls *)
  Hashtbl.add inheritance "Object" "Int";
  Hashtbl.add inheritance "Object" "Bool";
  Hashtbl.add inheritance "Object" "String";
  Hashtbl.add inheritance "Object" "IO";

  Hashtbl.add class_map_method "Object"
    ( (0, "type_name"),
      [],
      (0, "String"),
      {
        loc = 0;
        exp_kind = Internal ("String", "Object", "type_name");
        static_type = None;
      },
      "Object" );
  Hashtbl.add class_map_method "Object"
    ( (0, "copy"),
      [],
      (0, "SELF_TYPE"),
      {
        loc = 0;
        exp_kind = Internal ("SELF_TYPE", "Object", "copy");
        static_type = None;
      },
      "Object" );
  Hashtbl.add class_map_method "Object"
    ( (0, "abort"),
      [],
      (0, "Object"),
      {
        loc = 0;
        exp_kind = Internal ("Object", "Object", "abort");
        static_type = None;
      },
      "Object" );

  Hashtbl.add class_map_method "IO"
    ( (0, "out_string"),
      [ ((0, "x"), (0, "String")) ],
      (0, "SELF_TYPE"),
      {
        loc = 0;
        exp_kind = Internal ("SELF_TYPE", "IO", "out_string");
        static_type = None;
      },
      "IO" );
  Hashtbl.add class_map_method "IO"
    ( (0, "out_int"),
      [ ((0, "x"), (0, "Int")) ],
      (0, "SELF_TYPE"),
      {
        loc = 0;
        exp_kind = Internal ("SELF_TYPE", "IO", "out_int");
        static_type = None;
      },
      "IO" );
  Hashtbl.add class_map_method "IO"
    ( (0, "in_string"),
      [],
      (0, "String"),
      {
        loc = 0;
        exp_kind = Internal ("String", "IO", "in_string");
        static_type = None;
      },
      "IO" );
  Hashtbl.add class_map_method "IO"
    ( (0, "in_int"),
      [],
      (0, "Int"),
      {
        loc = 0;
        exp_kind = Internal ("Int", "IO", "in_int");
        static_type = None;
      },
      "IO" );

  Hashtbl.add class_map_method "String"
    ( (0, "substr"),
      [ ((0, "i"), (0, "Int")); ((0, "l"), (0, "Int")) ],
      (0, "String"),
      {
        loc = 0;
        exp_kind = Internal ("String", "String", "substr");
        static_type = None;
      },
      "String" );
  Hashtbl.add class_map_method "String"
    ( (0, "length"),
      [],
      (0, "Int"),
      {
        loc = 0;
        exp_kind = Internal ("Int", "String", "length");
        static_type = None;
      },
      "String" );
  Hashtbl.add class_map_method "String"
    ( (0, "concat"),
      [ ((0, "s"), (0, "String")) ],
      (0, "String"),
      {
        loc = 0;
        exp_kind = Internal ("String", "String", "concat");
        static_type = None;
      },
      "String" );

  (* 
        look for inheritance from Int 
        look for inheritance from Undeclared Class
    *)
  List.iter
    (fun ((cloc, cname), inherits, features) ->
      (* Class named SELF_TYPE *)
      if cname = "SELF_TYPE" then (
        printf "ERROR: %d: Type-Check: class named SELF_TYPE\n" cloc;
        exit 1);
      (* Redefining base classes *)
      if List.mem cname user_classes && List.mem cname base_classes then (
        printf "ERROR: %d: Type-Check: class %s redefined\n" cloc cname;
        exit 1);

      (* Check for duplicate classes *)
      if SeenSet.mem cname !seen then (
        printf "ERROR: %d: Type-Check: class %s redefined\n" cloc cname;
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
                    (mid, formal_list, mtype, mexp, cname))
              (* method name, formal list, return type, method expression, and source class *)
            (List.rev lst));
      match inherits with
      | None -> Hashtbl.add inheritance "Object" cname
      (* inherits from Object by default *)
      | Some (iloc, iname) ->
          (* inherited type identifier *)
          if List.mem iname illegal_inherit_classes then (
            printf "ERROR: %d: Type-Check: class %s inherits from %s\n" iloc
              cname iname;
            exit 1);
          if not (List.mem iname all_classes) then (
            printf
              "ERROR: %d: Type-Check: class %s inherits from unknown class %s\n"
              iloc cname iname;
            exit 1);
          Hashtbl.add inheritance iname cname)
    ast;

  (* Check for missing main in Main *)
  if not (List.mem "Main" all_classes) then (
    printf "ERROR: 0: Type-Check: class Main not found\n";
    exit 1);

  (* Checking for inheritance cycle *)
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
            printf "ERROR: %d: Type-Check: class %s redefines attribute %s\n"
              loc cname name;
            exit 1);
          attr_seen := SeenSet.add name !attr_seen)
        attr_list;
      let meth_list = Hashtbl.find_all class_map_method cname in
      let meth_seen = ref SeenSet.empty in

      (* Check for redefined method *)
      List.iter
        (fun ((loc, mname), formal_list, _, _, _) ->
          if SeenSet.mem mname !meth_seen then (
            printf "ERROR: %d: Type-Check: class %s redefines method %s\n" loc
              cname mname;
            exit 1)
          else
            (* Check for duplicates in formal list *)
            let formal_seen = ref SeenSet.empty in
            List.iter
              (fun ((loc, pname), _) ->
                if SeenSet.mem pname !formal_seen then (
                  printf
                    "ERROR: %d: Type-Check: class %s has method %s with \
                     duplicate formal parameter named %s\n"
                    loc cname mname pname;
                  exit 1);
                formal_seen := SeenSet.add pname !formal_seen)
              formal_list;
            meth_seen := SeenSet.add mname !meth_seen)
        meth_list)
    all_classes;

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
            "ERROR: %d: Type-Check: class %s has attribute %s with unknown \
             type %s\n"
            tloc cname name tname;
          exit 1);
        if name = "self" then (
          printf "ERROR: %d: Type-Check: class %s has an attribute named self\n"
            aloc cname;
          exit 1);
        if List.mem name inherited_attributes_names then (
          printf "ERROR: %d: Type-Check: class %s redefines attribute %s\n" aloc
            cname name;
          exit 1))
      attributes;
    List.iter
      (fun ((mloc, mname), formal_list, (typeloc, mtype), _, _) ->
        (* Checks each formal parameter to see if the type exists *)
        List.iter
          (fun ((floc, fname), (ftloc, ftype)) ->
            if not (List.mem ftype all_classes) then (
              printf
                "ERROR: %d: Type-Check: class %s has method %s with formal \
                 parameter of unknown type %s\n"
                ftloc cname mname ftype;
              exit 1);
            if fname = "self" then (
              printf
                "ERROR: %d: Type-Check: class %s has method %s with formal \
                 parameter named self\n"
                floc cname mname;
              exit 1))
          formal_list;

        (* Checks the return type to see if the type exists *)
        if (not (List.mem mtype all_classes)) && mtype <> "SELF_TYPE" then (
          printf
            "ERROR: %d: Type-Check: class %s has method %s with unknown return \
             type %s\n"
            typeloc cname mname mtype;
          exit 1);
        List.iter
          (fun ((imloc, imname), iformal_list, (itypeloc, imtype), _, _) ->
            (* if (mname = imname) then begin *)
            if is_subtype (Class mname) (Class imname) then (
              (* Checking for inherited method redefinitions*)
              (* Checking for return type change *)
              if mtype <> imtype then (
                printf
                  "ERROR: %d: Type-Check: class %s redefines method %s and \
                   changes return type (from %s to %s)\n"
                  typeloc cname mname imtype mtype;
                exit 1);
              (* Checking for # of formals change *)
              if List.length formal_list <> List.length iformal_list then (
                printf
                  "ERROR: %d: Type-Check: class %s redefines method %s and \
                   changes number of formals\n"
                  mloc cname mname;
                exit 1);
              (* Checking if the formal types were changed *)
              List.iter2
                (fun ((fploc, fname), (ftloc, ftype)) ((_, ifname), (_, iftype))
                   ->
                  if ftype <> iftype then (
                    printf
                      "ERROR: %d: Type-Check: class %s redefines method %s and \
                       changes type of formal %s\n"
                      ftloc cname mname fname;
                    exit 1))
                formal_list iformal_list))
          inherited_methods)
      methods;
    (* If no errors are found, then add inherited features to the class map*)
    List.iter (fun _ -> Hashtbl.remove class_map_method cname) methods;
    (* Add methods that aren't redefined inherited methods *)
    List.iter
      (fun meth ->
        let (_, mname), _, _, _, _ = meth in
        if
          not
            (List.mem mname
               (List.map
                  (fun ((_, name), _, _, _, _) -> name)
                  inherited_methods))
        then Hashtbl.add class_map_method cname meth)
      (List.rev methods);
    (* Add methods that are inherited/redefined *)
    (*     List.iter
      (fun meth -> let (_, mname), _, _, _, _ = meth in
      if
          (List.mem mname
             (List.map (fun ((_, name), _, _, _, _) -> name) inherited_methods))
      then Hashtbl.add class_map_method cname meth)
      (List.rev methods); *)
    List.iter
      (fun meth ->
        let (mloc, mname), formals, mtype, mexp, _ = meth in
        if
          not
            (List.mem mname
               (List.map (fun ((_, name), _, _, _, _) -> name) methods))
        then Hashtbl.add class_map_method cname meth
        else
          Hashtbl.add class_map_method cname
            (List.find (fun ((_, iname), _, _, _, _) -> mname = iname) methods))
      (List.rev inherited_methods);
    List.iter (fun _ -> Hashtbl.remove class_map_attr cname) attributes;
    List.iter
      (fun attr -> Hashtbl.add class_map_attr cname attr)
      (List.rev attributes);
    List.iter
      (fun attr -> Hashtbl.add class_map_attr cname attr)
      (List.rev inherited_attributes);
    let child_classes = Hashtbl.find_all inheritance cname in
    List.iter (fun cl -> feature_check cname cl) child_classes
  in
  List.iter
    (fun cl -> feature_check "Object" cl)
    (Hashtbl.find_all inheritance "Object");

  (* BLOCK 2 END *)
  (* Check for main() method in Main or inherited classes*)
  let main_methods = Hashtbl.find_all class_map_method "Main" in
  let method_names =
    List.map (fun ((_, name), _, _, _, _) -> name) main_methods
  in
  if not (List.mem "main" method_names) then (
    printf "ERROR: 0: Type-Check: class Main method main not found\n";
    exit 1);
  (* Checking to see if the main method has zero parameters *)
  List.iter
    (fun ((_, name), formals, _, _, _) ->
      if name = "main" && List.length formals <> 0 then (
        printf
          "ERROR: 0: Type-Check: class Main method main with 0 parameters not \
           found\n";
        exit 1))
    (Hashtbl.find_all class_map_method "Main");
  let all_classes = List.rev !visited in
  (* top sorted *)

  (* Class Error checking complete *)

  (* Begin Expression type-checking *)
  let check_class_exists loc name =
    if not (List.mem name all_classes) then (
      printf "ERROR: %d: Type-Check: unknown type %s\n" loc name;
      exit 1)
  in
  let rec tc (cname : name) (o : obj_env) (m : meth_env) (exp : exp) :
      static_type =
    let static_type =
      match exp.exp_kind with
      | Assign (i, e1) ->
          (* [ASSIGN] *)
          let aloc, aname = i in
          let atype = Hashtbl.find o aname in
          let exp_type = tc cname o m e1 in
          if not (is_subtype exp_type atype) then (
            printf
              "ERROR: %d: Type-Check: %s does not conform to %s in initialized \
               attribute\n"
              aloc (type_to_str exp_type) (type_to_str atype);
            exit 1);
          Class (type_to_str exp_type)
      | Dynamic_Dispatch (e1, i, elist) ->
          let class_type = tc cname o m e1 in
          let mloc, mname = i in
          (* Checks if the method exists *)
          if not (Hashtbl.mem m (class_type, mname)) then (
            printf
              "ERROR: %d: Type-Check: unknown method %s in dispatch on %s\n"
              mloc mname (type_to_str class_type);
            exit 1);
          let meth = Hashtbl.find m (class_type, mname) in
          (* first n-1 elements are formal types, n is return type*)
          (* Checks if number of arguments matches with number of formals *)
          if List.length elist <> List.length meth - 1 then (
            printf
              "ERROR: %d: Type-Check: wrong number of actual arguments (%d vs. \
               %d)\n"
              mloc (List.length elist)
              (List.length meth - 1);
            exit 1);
          (* Type checks arguments to formals *)
          List.iteri
            (fun ind exp ->
              let exp_type = tc cname o m exp in
              let formal_type = Class (List.nth meth ind) in
              if not (is_subtype exp_type formal_type) then (
                printf
                  "ERROR: %d: Type-Check: argument #%d type %s does not \
                   conform to formal type %s\n"
                  mloc (ind + 1) (type_to_str exp_type)
                  (type_to_str formal_type);
                exit 1))
            elist;
          let rtype = List.hd (List.rev meth) in
          if rtype = "SELF_TYPE" then SELF_TYPE (type_to_str class_type)
          else Class rtype
      | Static_Dispatch (e1, (_, static_class), i2, elist) ->
          let calling_class = tc cname o m e1 in
          if not (is_subtype calling_class (Class static_class)) then (
            printf
              "ERROR: %d: Type-Check: %s does not conform to %s in static \
               dispatch\n"
              exp.loc
              (type_to_str calling_class)
              static_class;
            exit 1);
          let class_type = Class static_class in
          let mloc, mname = i2 in
          (* Checks if the method exists *)
          if not (Hashtbl.mem m (class_type, mname)) then (
            printf
              "ERROR: %d: Type-Check: unknown method %s in dispatch on %s\n"
              mloc mname (type_to_str class_type);
            exit 1);
          let meth = Hashtbl.find m (class_type, mname) in
          (* first n-1 elements are formal types, n is return type*)
          (* Checks if number of arguments matches with number of formals *)
          if List.length elist <> List.length meth - 1 then (
            printf
              "ERROR: %d: Type-Check: wrong number of actual arguments (%d vs. \
               %d)\n"
              mloc (List.length elist)
              (List.length meth - 1);
            exit 1);
          (* Type checks arguments to formals *)
          List.iteri
            (fun ind exp ->
              let exp_type = tc cname o m exp in
              let formal_type = Class (List.nth meth ind) in
              if not (is_subtype exp_type formal_type) then (
                printf
                  "ERROR: %d: Type-Check: argument #%d type %s does not \
                   conform to formal type %s\n"
                  mloc (ind + 1) (type_to_str exp_type)
                  (type_to_str formal_type);
                exit 1))
            elist;
          let rtype = List.hd (List.rev meth) in
          if rtype = "SELF_TYPE" then SELF_TYPE (type_to_str calling_class)
          else Class rtype
      | Self_Dispatch (i, elist) ->
          let mloc, mname = i in
          (* Checks if the method exists *)
          if not (Hashtbl.mem m (Class cname, mname)) then (
            printf
              "ERROR: %d: Type-Check: unknown method %s in dispatch on %s\n"
              mloc mname cname;
            exit 1);
          let meth = Hashtbl.find m (Class cname, mname) in
          (* first n-1 elements are formal types, n is return type*)
          (* Checks if number of arguments matches with number of formals *)
          if List.length elist <> List.length meth - 1 then (
            printf
              "ERROR: %d: Type-Check: wrong number of actual arguments (%d vs. \
               %d)\n"
              mloc (List.length elist)
              (List.length meth - 1);
            exit 1);
          (* Type checks arguments to formals *)
          List.iteri
            (fun ind exp ->
              let exp_type = tc cname o m exp in
              let formal_type = Class (List.nth meth ind) in
              if not (is_subtype exp_type formal_type) then (
                printf
                  "ERROR: %d: Type-Check: argument #%d type %s does not \
                   conform to formal type %s\n"
                  mloc (ind + 1) (type_to_str exp_type)
                  (type_to_str formal_type);
                exit 1))
            elist;
          let rtype = List.hd (List.rev meth) in
          if rtype = "SELF_TYPE" then SELF_TYPE cname else Class rtype
      | If (e1, e2, e3) ->
          (* [If] *)
          let predtype = tc cname o m e1 in
          if predtype <> Class "Bool" then (
            printf
              "ERROR: %d: Type-Check: conditional has type %s instead of Bool\n"
              e1.loc (type_to_str predtype);
            exit 1);
          let thentype = tc cname o m e2 in
          let elsetype = tc cname o m e3 in
          least_upper_bound thentype elsetype
      | While (e1, e2) ->
          (* [Loop] *)
          let predtype = tc cname o m e1 in
          if predtype <> Class "Bool" then (
            printf
              "ERROR: %d: Type-Check: predicate has type %s instead of Bool\n"
              exp.loc (type_to_str predtype);
            exit 1);
          (* Type-check the body, do nothing with it *)
          ignore (tc cname o m e2);
          Class "Object"
      | Block elist -> (
          (* [Sequence] *)
          let tn = ref None in
          List.iter
            (fun exp ->
              let t = tc cname o m exp in
              tn := Some t)
            elist;
          match !tn with
          | None ->
              printf "weird bug in block\n";
              exit 1
          | Some t -> t)
      | New i -> (
          (* [New] *)
          let iloc, itype = i in
          check_class_exists iloc itype;
          match itype with "SELF_TYPE" -> SELF_TYPE cname | _ -> Class itype)
      | Isvoid e ->
          (* [Isvoid] *)
          ignore (tc cname o m e);
          Class "Bool"
      | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Divide (e1, e2) ->
          (* [Arith] *)
          let t1 = tc cname o m e1 in
          let t2 = tc cname o m e2 in
          if t1 <> Class "Int" || t2 <> Class "Int" then (
            printf
              "ERROR: %d: Type-Check: arithmetic on %s %s instead of Ints\n"
              exp.loc (type_to_str t1) (type_to_str t2);
            exit 1);
          Class "Int"
      | Lt (e1, e2) | Le (e1, e2) ->
          (* [Compare] *)
          let t1 = tc cname o m e1 in
          let t2 = tc cname o m e2 in
          (match t1 with
          | Class "Int" | Class "String" | Class "Bool" ->
              if type_to_str t1 <> type_to_str t2 then (
                printf "ERROR: %d: Type-Check: comparison between %s and %s\n"
                  exp.loc (type_to_str t1) (type_to_str t2);
                exit 1)
          | _ ->
              (* Do nothing, since non default objects can be equated freely *)
              ());
          Class "Bool"
      | Eq (e1, e2) ->
          (* [Equal] *)
          let t1 = tc cname o m e1 in
          let t2 = tc cname o m e2 in
          (match t1 with
          | Class "Int" | Class "String" | Class "Bool" ->
              if type_to_str t1 <> type_to_str t2 then (
                printf "ERROR: %d: Type-Check: comparison between %s and %s\n"
                  exp.loc (type_to_str t1) (type_to_str t2);
                exit 1)
          | _ ->
              (* Do nothing, since non default objects can be equated freely *)
              ());
          Class "Bool"
      | Not e1 ->
          (* [Not] *)
          let t1 = tc cname o m e1 in
          if t1 <> Class "Bool" then (
            printf
              "ERROR: %d: Type-Check: not applied to type %s instead of Bool\n"
              exp.loc (type_to_str t1);
            exit 1);
          Class "Bool"
      | Negate e1 ->
          (* [Neg] *)
          let t1 = tc cname o m e1 in
          if t1 <> Class "Int" then (
            printf
              "ERROR: %d: Type-Check: negate applied to type %s instead of Int\n"
              exp.loc (type_to_str t1);
            exit 1);
          Class "Int"
      | Integer c -> Class "Int"
      | String c -> Class "String"
      | Identifier (iloc, iname) ->
          (* [Var] *)
          if Hashtbl.mem o iname then Hashtbl.find o iname
          else (
            printf "ERROR: %d: Type-Check: unbound identifier %s\n" iloc iname;
            exit 1)
      | Bool c -> Class "Bool"
      | Let (bindlist, let_body) ->
          (* Let rules *)
          (* Add all variables in the letexpr to the scope *)
          List.iter
            (* typename is the T_0 in the type rule. T_1 (binit) must be <= T_0 *)
            (fun (Binding ((vloc, vname), (typeloc, typename), binit)) ->
              match binit with
              (* [Let-Init] *)
              | Some binit ->
                  let binit_type = tc cname o m binit in
                  if not (is_subtype binit_type (Class typename)) then (
                    printf
                      "ERROR: %d: Type-Check: initializer type %s does not \
                       conform to type %s\n"
                      exp.loc (type_to_str binit_type) typename;
                    exit 1)
                  else
                    (* Add to global obj env *)
                    (* printf "%s: %s %s\n" vname (type_to_str binit_type) typename; *)
                    Hashtbl.add o vname (Class typename)
              (* [Let-No-Init] *)
              | None -> Hashtbl.add o vname (Class typename))
            bindlist;
          (* Typecheck let_body with newly bound variables *)
          let body_type = tc cname o m let_body in

          (* Remove all variables in the letexpr from the scope *)
          List.iter
            (fun (Binding ((vloc, vname), (typeloc, typenam), binit)) ->
              Hashtbl.remove o vname)
            bindlist;
          body_type
      | Case (e0, caselist) ->
          ignore (tc cname o m e0);
          let seenTypes = ref SeenSet.empty in
          let branchTypes =
            List.fold_left
              (fun acc (Case_Elem ((loc, name), (tloc, tname), exp)) ->
                if SeenSet.mem tname !seenTypes then (
                  printf
                    "ERROR: %d: Type-Check: case branch type %s is bound twice\n"
                    tloc tname;
                  exit 1)
                else (
                  Hashtbl.add o name (Class tname);
                  if tname = "SELF_TYPE" then (
                    printf
                      "ERROR: %d: Type-Check: using %s as a case branch type \
                       is not allowed\n"
                      tloc tname;
                    exit 1);
                  if name = "self" then (
                    printf
                      "ERROR: %d: Type-Check: binding %s in a case expression \
                       is not allowed\n"
                      loc name;
                    exit 1);
                  let branchType = tc cname o m exp in
                  Hashtbl.remove o name;
                  seenTypes := SeenSet.add tname !seenTypes;
                  branchType :: acc))
              [] caselist
          in
          let lub_list branchTypes =
            match branchTypes with
            | [] -> failwith "case error bug!!!! somehow no case"
            | [ x ] -> x
            | hd :: tl ->
                List.fold_left (fun acc ce -> least_upper_bound acc ce) hd tl
          in
          lub_list branchTypes
      | Internal (c, d, n) -> Class c
    in
    (* write to type field *)
    exp.static_type <- Some static_type;
    static_type
  in

  (* Expression type-checking completed *)

  (* Emit CL-TYPE File *)
  let cltname = Filename.chop_extension fname ^ ".cl-type" in
  let fout = open_out cltname in

  let rec output_exp e =
    fprintf fout "%d\n" e.loc;
    (* Output type annotation *)
    (match e.static_type with
    | None -> fprintf fout "\n"
    | Some (Class c) -> fprintf fout "%s\n" c
    | Some (SELF_TYPE c) -> fprintf fout "SELF_TYPE\n");
    match e.exp_kind with
    | Assign ((id_loc, exp_name), exp) ->
        fprintf fout "assign\n%d\n%s\n" id_loc exp_name;
        output_exp exp
    | Dynamic_Dispatch (exp, (meth_loc, meth_name), args) ->
        fprintf fout "dynamic_dispatch\n";
        output_exp exp;
        fprintf fout "%d\n%s\n%d\n" meth_loc meth_name (List.length args);
        List.iter (fun exp -> output_exp exp) args
    | Static_Dispatch (exp, (stcl_loc, stcl_name), (sdmt_loc, sdmt_name), args)
      ->
        fprintf fout "static_dispatch\n";
        output_exp exp;
        fprintf fout "%d\n%s\n" stcl_loc stcl_name;
        fprintf fout "%d\n%s\n%d\n" sdmt_loc sdmt_name (List.length args);
        List.iter (fun exp -> output_exp exp) args
    | Self_Dispatch ((sdloc, sdname), args) ->
        fprintf fout "self_dispatch\n%d\n%s\n%d\n" sdloc sdname
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
    | New (loc, name) -> fprintf fout "new\n%d\n%s\n" loc name
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
    | Integer ival -> fprintf fout "integer\n%d\n" ival
    | String sval -> fprintf fout "string\n%s\n" sval
    | Identifier (loc, name) -> fprintf fout "identifier\n%d\n%s\n" loc name
    | Bool bval -> fprintf fout "%s\n" bval
    | Let (bindlist, body) ->
        fprintf fout "let\n%d\n" (List.length bindlist);
        List.iter
          (fun (b : binding) ->
            match b with
            | Binding ((bloc, bname), (tloc, tname), Some exp) ->
                fprintf fout "let_binding_init\n%d\n%s\n%d\n%s\n" bloc bname
                  tloc tname;
                output_exp exp
            | Binding ((bloc, bname), (tloc, tname), None) ->
                fprintf fout "let_binding_no_init\n%d\n%s\n%d\n%s\n" bloc bname
                  tloc tname)
          bindlist;
        output_exp body
    | Case (caseexp, elemlist) ->
        fprintf fout "case\n";
        output_exp caseexp;
        fprintf fout "%d\n" (List.length elemlist);
        List.iter
          (fun (Case_Elem ((cid, cname), (ctid, ctname), exp)) ->
            fprintf fout "%d\n%s\n%d\n%s\n" cid cname ctid ctname;
            output_exp exp)
          elemlist
    | Internal (m, c, name) -> fprintf fout "internal\n%s.%s\n" c name
  in

  let sorted_all_classes = List.sort compare all_classes in
  let set_envs cname =
    Hashtbl.clear global_obj_env;
    List.iter
      (fun ((_, aname), (_, atype), _) ->
        Hashtbl.add global_obj_env aname (Class atype))
      (Hashtbl.find_all class_map_attr cname);
    Hashtbl.add global_obj_env "self" (SELF_TYPE cname);
    List.iter
      (fun ((_, mname), formals, (_, rtype), _, _) ->
        let newFormals = List.map (fun (_, (_, ftype)) -> ftype) formals in
        let newFormals = newFormals @ [ rtype ] in
        if not (Hashtbl.mem global_meth_env (Class cname, mname)) then
          Hashtbl.add global_meth_env (Class cname, mname) newFormals)
      (Hashtbl.find_all class_map_method cname)
  in
  (* Function to print class map *)
  let print_class_map fout sorted_classes class_map_attr output_exp =
    fprintf fout "class_map\n%d\n" (List.length all_classes);

    List.iter
      (fun cname ->
        fprintf fout "%s\n" cname;
        let attributes = Hashtbl.find_all class_map_attr cname in
        fprintf fout "%d\n" (List.length attributes);
        set_envs cname;
        List.iter
          (fun attr ->
            match attr with
            | (aloc, aname), (_, atype), None ->
                fprintf fout "no_initializer\n%s\n%s\n" aname atype
            | (aloc, aname), (_, atype), Some init ->
                fprintf fout "initializer\n%s\n%s\n" aname atype;
                let atype =
                  if atype = "SELF_TYPE" then SELF_TYPE cname else Class atype
                in
                let init_type = tc cname global_obj_env global_meth_env init in
                if not (is_subtype init_type atype) then (
                  printf
                    "ERROR: %d: Type-Check: %s does not conform to %s in \
                     initialized attribute\n"
                    aloc (type_to_str init_type) (type_to_str atype);
                  exit 1);
                output_exp init)
          attributes
        (* Attributes are stored in reverse order due to how insertion into hash tables work*))
      sorted_classes
  in

  (* Function to print implementation map *)
  let print_impl_map fout (sorted_classes : name list) class_map_method
      output_exp =
    fprintf fout "implementation_map\n%d\n" (List.length all_classes);
    List.iter
      (fun cname ->
        fprintf fout "%s\n" cname;
        (* printf "Class: %s\n Methods: " cname; *)
        let methods = Hashtbl.find_all class_map_method cname in
        fprintf fout "%d\n" (List.length methods);
        set_envs cname;
        List.iter
          (fun meth ->
            let ( (mloc, mname),
                  mformals,
                  (returnloc, returntype),
                  mbody,
                  src_class ) =
              meth
            in
            (* Check body return type matches method type *)
            (* printf "%s " mname; *)
            List.iter
              (fun ((_, fmname), (_, fmtype)) ->
                Hashtbl.add global_obj_env fmname (Class fmtype))
              mformals;
            let o = global_obj_env in
            let m = global_meth_env in
            let body_type = tc cname o m mbody in
            (* TODO: This is erroring on comparing return types and body types of builtin methods *)
            if
              (not (is_subtype body_type (Class returntype)))
              && not
                   (returntype = "SELF_TYPE"
                   && is_subtype body_type (Class cname))
            then (
              printf
                "ERROR: %d: Type-Check: %s does not conform to %s in method %s\n"
                mloc (type_to_str body_type) returntype mname;
              exit 1);
            (* Print formals *)
            fprintf fout "%s\n%d\n" mname (List.length mformals);
            List.iter
              (fun ((_, fmname), _) ->
                fprintf fout "%s\n" fmname;
                Hashtbl.remove global_obj_env fmname)
              mformals;
            (* If this method is inherited but NOT OVERIDDEN 
              Output the name of the highest parent class that defined 
              the method body expression, otherwise output current class *)
            let _, _, _, _, most_recent_def = meth in
            fprintf fout "%s\n" most_recent_def;
            output_exp mbody)
          methods
        (* printf "\n"; *))
      sorted_classes
  in

  let print_parent_map fout (sorted_classes : name list) inheritance =
    fprintf fout "parent_map\n%d\n" (Hashtbl.length inheritance);
    List.iter
      (fun cname ->
        if cname <> "Object" then
          let parent = find_parent cname inheritance in
          fprintf fout "%s\n%s\n" cname
            (match parent with
            | Some parent -> parent
            | None -> "BUG FOUND - find_parent cannot find parent!!!!"))
      sorted_classes
  in
  let print_annoated_ast fout ast output_exp =
    fprintf fout "%d\n" (List.length ast);
    List.iter
      (fun ((cloc, cname), inherits, features) ->
        fprintf fout "%d\n%s\n" cloc cname;
        (match inherits with
        | None -> fprintf fout "no_inherits\n"
        (* inherits from Object by default *)
        | Some (iloc, iname) -> fprintf fout "inherits\n%d\n%s\n" iloc iname);
        fprintf fout "%d\n" (List.length features);
        match features with
        | [] -> fprintf fout ""
        | lst ->
            List.iter
              (fun feat ->
                match feat with
                | Attribute ((aloc, aname), (atloc, atype), Some aexp) ->
                    fprintf fout "attribute_init\n%d\n%s\n%d\n%s\n" aloc aname
                      atloc atype;
                    output_exp aexp
                | Attribute ((aloc, aname), (atloc, atype), None) ->
                    fprintf fout "attribute_no_init\n%d\n%s\n%d\n%s\n" aloc
                      aname atloc atype
                | Method ((mloc, mname), formal_list, (mtloc, mtype), mexp) ->
                    fprintf fout "method\n%d\n%s\n%d\n" mloc mname
                      (List.length formal_list);
                    List.iter
                      (fun ((floc, fname), (ftloc, ftname)) ->
                        fprintf fout "%d\n%s\n%d\n%s\n" floc fname ftloc ftname)
                      formal_list;
                    fprintf fout "%d\n%s\n" mtloc mtype;
                    output_exp mexp)
                (* method name, formal list, return type, method expression, and source class *)
              lst)
      ast
  in
  (match args_array with
  | [ prog; coolprog ] ->
      (* TEMP SET to only print class map for PA2C2 *)
      print_class_map fout sorted_all_classes class_map_attr output_exp;
      print_impl_map fout sorted_all_classes class_map_method output_exp;
      print_parent_map fout sorted_all_classes inheritance;
      print_annoated_ast fout ast output_exp
  | [ prog; coolprog; arg ] when arg = "--class-map" ->
      print_class_map fout sorted_all_classes class_map_attr output_exp
  | [ prog; coolprog; arg ] when arg = "--parent-map" ->
      print_parent_map fout sorted_all_classes inheritance
  | [ prog; coolprog; arg ] when arg = "--imp-map" ->
      print_impl_map fout sorted_all_classes class_map_method output_exp
  | _ -> printf "something went very wrong\n");

  close_out fout
;;

main ()
