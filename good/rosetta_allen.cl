-- COOL: Reverse-sort the lines from standard input. 

Class Main -- Main is where the program starts
  inherits IO { -- inheriting from IO allows us to read and write strings

    prereqs : MapList <- new MapNil;
    visited : List <- new Nil;
    cycle : List <- new Nil;
    dfs(task: String) : Bool {
        {
        out_string(task);
        out_string("\n");
        --cycle.print_list();
        if cycle.contains(task) then
            {
                out_string("In a cycle\n");
                false;
            }
        else
            if visited.contains(task) then
                true
            else
                {
                    cycle <- cycle.insert(task);
                    if prereqs.containsKey(task) then
                        let currentTasks : List <- prereqs.returnVal(task) in
                        {
                            while not currentTasks.is_empty() loop {
                                if not dfs(currentTasks.get()) then
                                    {
                                        false;
                                    }
                                else
                                    currentTasks <- currentTasks.next()
                                fi ;
                            } pool ;
                        }
                    else
                    {
                        new Object;
                    }
                    fi;
                    visited <- visited.cons(task);
                    cycle <- cycle.remove(task);
                    true;
                }
            fi
        fi;
        }
    } ;
	main() : Object { -- this method is invoked when the program starts
        let 
            done : Bool <- false,
            tasks : List <- new Nil
        in {
        while not done loop {
            let a : String <- in_string () in 
            let b : String <- in_string () in
            if b = "" then (* if we are done reading lines
                            * then s will be "" *) 
            done <- true 
            else 
                {
                if prereqs.containsKey(a) then
                    prereqs <- prereqs.insertVal(a, b) -- insertion sort it into our list
                else
                    {
                        prereqs <- prereqs.insertKey(a);
                        prereqs <- prereqs.insertVal(a, b);
                    }
                fi;
                    if not tasks.contains(a) then
                        tasks <- tasks.insert(a)
                    else
                        new Object
                    fi;

                    if not tasks.contains(b) then
                        tasks <- tasks.insert(b)
                    else
                        new Object
                    fi;
                }
    
            fi ;
        } pool ; -- loop/pool deliniates a while loop body
        -- out_string(tasks.get());
        --tasks.print_list();
        -- prereqs.print_list();
        while not tasks.is_empty() loop {
            if not dfs(tasks.get()) then
            {
                out_string("cycle\n");
                abort();
            }   
            else
                new Object
            fi ;
            tasks <- tasks.next();
        } pool ;
        visited.print_reverse();
        }
	};
};

(* The List type is not built in to Cool, so we'll have to define it 
 * ourselves. Cool classes can appear in any order, so we can define
 * List here _after_ our reference to it in Main. *) 
Class List inherits IO { 
        (* We only need three methods: cons, insert and print_list. *) 
           
        (* cons appends returns a new list with 'hd' as the first
         * element and this list (self) as the rest of the list *) 
	cons(hd : String) : Cons { 
	  let new_cell : Cons <- new Cons in
		new_cell.init(hd,self)
	};

        (* You can think of List as an abstract interface that both
         * Cons and Nil (below) adhere to. Thus you're never supposed
         * to call insert() or print_list() on a List itself -- you're
         * supposed to build a Cons or Nil and do your work there. *) 
	insert(i : String) : List { self };
    
    contains(s : String) : Bool { false };
    get() : String { "" };
    next() : List { self };
    is_empty() : Bool { false } ;
    remove(s : String) : List { self };
	print_list() : Object { abort() };
    print_reverse() : Object { abort() };
} ;

Class Cons inherits List { -- a Cons cell is a non-empty list 
	xcar : String;          -- xcar is the contents of the list head 
	xcdr : List;            -- xcdr is the rest of the list

	init(hd : String, tl : List) : Cons {
	  {
	    xcar <- hd;
	    xcdr <- tl;
	    self;
	  }
	};
	  
        (* insert() does insertion sort *) 
	insert(s : String) : List {
		if (s < xcar) then          
			(new Cons).init(s,self)
		else
			(new Cons).init(xcar,xcdr.insert(s))
		fi
	};

    contains(s : String) : Bool {
        if xcar = s then
            true
        else
            xcdr.contains(s)
        fi
    };

	print_list() : Object {
		{
		     out_string(xcar);
		     out_string("\n");
		     xcdr.print_list();
		}
	};

    is_empty() : Bool { false } ;
    
    remove(s : String) : List {
        if xcar = s then {
            xcar = "";
            xcdr;
        }
        else
            xcdr.remove(s)
        fi
    } ;
    get() : String {
        xcar
    };
    next() : List {
        xcdr
    };
    
    print_reverse() : Object {
        {
            xcdr.print_reverse();
            out_string(xcar);
            out_string("\n");
        }
    } ;
} ;

Class Nil inherits List { -- Nil is an empty list 

	insert(s : String) : List { (new Cons).init(s,self) }; 

	print_list() : Object { true }; -- do nothing 

    print_reverse() : Object { true };

    contains(s : String) : Bool { false };

    is_empty() : Bool { true } ;
    remove(s : String) : List { new Nil};
    get() : String { "" };
    next() : List { self };
} ;

Class MapList inherits IO {
    cons(hd : String, tl : List) : MapCons { 
        let new_cell : MapCons <- new MapCons in
        new_cell.init(hd, tl, self)
    };
    
    insertKey(s : String) : MapList { self };
    insertVal(i : String, j : String) : MapList { self };
    
    containsKey(s: String) : Bool { false } ;
    is_empty() : Bool { false } ;
    returnVal(s: String) : List { new Nil };

    print_list() : Object { abort() };
} ;

Class MapCons inherits MapList { -- a Cons cell is a non-empty list 
	xkey : String;          -- xkey is the key/node
    xval : List;            -- xval is the values/list of edges
	xcdr : MapList;            -- xcdr is the rest of the map

	init(hd : String, tl : List, md : MapList) : MapCons {
	  {
	    xkey <- hd;
	    xval <- tl;
        xcdr <- md;
	    self;
	  }
	};
	containsKey(s: String) : Bool { 
        if xkey = s then
            true
        else
            xcdr.containsKey(s)
        fi
     } ;
    insertKey(s : String) : MapList {
		if (s < xkey) then          
			(new MapCons).init(s, new Nil, self)
		else
			(new MapCons).init(xkey, xval, xcdr.insertKey(s))
		fi
	};

	insertVal(key : String, val : String) : MapList {
        {
            (* out_string("Inserting key: ");
            out_string(key);
            out_string(" and value: ");
            out_string(val);
            out_string("\n"); *)
        
		if key = xkey then
            (new MapCons).init(key, xval.insert(val), self)
        else
            (new MapCons).init(xkey, xval, xcdr.insertVal(key, val))
        fi;
        }
	};
    returnVal(key : String) : List {
        if key = xkey then
            xval
        else
            xcdr.returnVal(key)
        fi
    };

	print_list() : Object {
		{
            out_string(xkey);
            out_string(": \n");
            xval.print_list();
            out_string("\n");
            xcdr.print_list();
		}
	};

    is_empty() : Bool { false } ;
    
} ;

Class MapNil inherits MapList { -- Nil is an empty list 

    insertKey(s : String) : MapList { (new MapCons).init(s, new Nil, self) };
	insertVal(s : String, val : String) : MapList { (new MapCons).init(s, (new Cons).init(val, new Cons), self) }; 
    containsKey(s: String) : Bool { false } ;
	print_list() : Object { true }; -- do nothing 
    returnVal(s: String) : List { new Nil };
    is_empty() : Bool { true } ;
    
} ;