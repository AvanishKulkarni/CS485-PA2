Class List inherits IO { 
	isNil() : Bool { { abort(); true; } };

	cons(hd : String) : Cons {
		(let new_cell : Cons <- new Cons in
			new_cell.init(hd, self)
		)
	};

	car() : String { { abort(); new String; } };

	cdr() : List { { abort(); new List; } };

	insert(i : String) : List { cdr() };

	sort() : List { cdr() };

	rev() : List { cdr() };

	rcons(i : String) : List { cdr() };

	print_list() : Object { abort() };

	is_member(i : String) : Bool { { abort(); false; } };

	is_empty() : Bool { false };

	len_helper(list: List, l: Int) : Int { { abort(); new Int; } };

	len() : Int { { abort(); new Int; } };

	at_helper(list: List, i: Int, counter: Int) : List { { abort(); new List; } }; 

	at(i: Int) : List { { abort(); new List; } }; 
};

Class Cons inherits List {
	xcar : String;  -- We keep the car in cdr in attributes.
	xcdr : List; 

	isNil() : Bool { false };

	init(hd : String, tl : List) : Cons {
		{
			xcar <- hd;
			xcdr <- tl;
			self;
		}
	};

	car() : String { xcar };

	cdr() : List { xcdr };

	insert(i : String) : List {
		(new Cons).init(xcar,xcdr.insert(i))
	};

	sort() : List { (xcdr.sort()).insert(xcar) };

	rev() : List { (xcdr.rev()).rcons(xcar) };

	rcons(i : String) : List { (new Cons).init(xcar, xcdr.rcons(i)) };

	print_list() : Object {
		{
		    out_string(xcar);
		    out_string("\n");
		    xcdr.print_list();
		}
	};
	is_member(i : String) : Bool {
		{
			if i = xcar then
				true
			else
				xcdr.is_member(i)
			fi;
		}
	};

	at_helper(list: List, i: Int, counter: Int) : List { 
		if list.isNil() then 
			new Nil
		else
			if counter = i then
				(new Cons).init(list.car(), new Nil)
			else
				at_helper(list.cdr(), i, counter + 1)
			fi
		fi
	}; 

	at(i: Int) : List { 
		{ 
			at_helper(self, i, 0);
		} 
	}; 

	len_helper(list: List, l: Int) : Int {
		{
			if list.isNil() then
				l
			else
				len_helper(list.cdr(), l + 1)
			fi;
		}
	};

	len() : Int {
		{
			let l: Int <- 0 in 
				len_helper(self, l);
		}
	};

	is_empty() : Bool { false };
};

Class Nil inherits List {
	isNil() : Bool { true };

	insert(i : String) : List { rcons(i) };

	sort() : List { self };

	rev() : List { self };

	rcons(i : String) : List { (new Cons).init(i,self) };

	print_list() : Object { true };

	is_member(i : String) : Bool { false };

	is_empty() : Bool { true };

	len_helper(list: List, l: Int) : Int { { 0; } };

	len() : Int { { 0; } };

	at_helper(list: List, i: Int, counter: Int) : List { { abort(); new Nil; } }; 

	at(i: Int) : List { { abort(); new Nil; } }; 

};

Class SSList inherits IO { 
	isNil() : Bool { { abort(); true; } };

	cons(task : String, prereq: String) : SSCons {
        (let new_cell : SSCons <- new SSCons in
            new_cell.init(task, prereq, self)
        )
	};

	car() : String { { abort(); new String; } };

	cbr() : String { { abort(); new String; } };

	set_cbr(s: String) : Object {
		abort()
	};

	cdr() : SSList { { abort(); new SSList; } };

	rev() : SSList { cdr() };

	sort() : SSList { cdr() };

	insert(a : String, b : String) : SSList { cdr() };

	rcons(a : String, b : String) : SSList { cdr() };

	len_helper(list: SSList, l: Int) : Int { { abort(); new Int; } };

	len() : Int { { abort(); new Int; } };

	at_helper(list: SSList, i: Int, counter: Int) : SSList { { abort(); new SSList; } }; 

	at(i: Int) : SSList { { abort(); new SSList; } }; 

	get_helper(list: SSList, s: String) : String { { abort(); new String; } };

	get(s: String) : String { { abort(); new String; } };

	set_helper(list: SSList, s: String, v: String) : Object { abort() };

	set(s: String, v: String) : Object { abort() };

	dfs(current: String, edges: SSList, visited: List, stack: List) : List { { new List; } };

	top_sort(edges: SSList, vertices: List, progress: SSList) : List { new Nil };
};

Class SSNil inherits SSList {
	isNil() : Bool { true };

    rev() : SSList { self };

	sort() : SSList { self };

	insert(a : String, b : String) : SSList { rcons(a, b) };

	rcons(a : String, b: String) : SSList { (new SSCons).init(a, b, self) };

	len_helper(list: SSList, l: Int) : Int { { 0; } };

	len() : Int { { 0; } };

	dfs(current: String, edges: SSList, visited: List, stack: List) : List { { new Nil; } };

	at_helper(list: SSList, i: Int, counter: Int) : SSList { { abort(); new SSNil; } }; 

	at(i: Int) : SSList { { abort(); new SSNil; } }; 

	get_helper(list: SSList, s: String) : String { { new String; } };

	get(s: String) : String { { new String; } };

	set_helper(list: SSList, s: String, v: String) : Object { abort() };

	set(s: String, v: String) : Object { abort() };

	top_sort(edges: SSList, vertices: List, progress: SSList) : List { new Nil };
};

Class SSCons inherits SSList {
	xcar : String;  -- We keep the car in cdr in attributes.
    xcbr : String;
	xcdr : SSList; 

	isNil() : Bool { false };

	init(task : String, prereq: String, tl : SSList) : SSCons {
	  {
	    xcar <- task;
        xcbr <- prereq;
	    xcdr <- tl;
	    self;
	  }
	};

	car() : String { xcar };

    cbr() : String { xcbr };

	set_cbr(s: String) : Object {
		xcbr <- s
	};

	cdr() : SSList { xcdr };

	sort() : SSList { (xcdr.sort()).insert(xcar, xcbr) };

	insert(a : String, b : String) : SSList {
		if a < xcar then
			(new SSCons).init(a, b, self)
		else
			(new SSCons).init(xcar, xcbr, xcdr.insert(a, b))
		fi
	};

	rcons(a : String, b: String) : SSList { (new SSCons).init(xcar, xcbr, xcdr.rcons(a, b)) };

	len_helper(list: SSList, l: Int) : Int {
		{
			if list.isNil() then
				l
			else
				len_helper(list.cdr(), l + 1)
			fi;
		}
	};

	len() : Int {
		{
			let l: Int <- 0 in 
				len_helper(self, l);
		}
	};

	at_helper(list: SSList, i: Int, counter: Int) : SSList { 
		if list.isNil() then 
			new SSNil
		else
			if counter = i then
				(new SSCons).init(list.car(), list.cbr(), new SSNil)
			else
				at_helper(list.cdr(), i, counter + 1)
			fi
		fi
	}; 

	at(i: Int) : SSList { 
		{ 
			at_helper(self, i, 0);
		} 
	}; 

	get_helper(list: SSList, s: String) : String { 
		{ 
			if list.isNil() then
				new String
			else 
				if list.car() = s then
					list.cbr()
				else 
					get_helper(list.cdr(), s)
				fi
			fi;
		} 
	};

	get(s: String) : String { 
		{ 
			get_helper(self, s);
		} 
	};

	set_helper(list: SSList, s: String, v: String) : Object { 
		{ 
			if list.isNil() then
				new Object
			else 
				if list.car() = s then
					list.set_cbr(v)
				else 
					set_helper(list.cdr(), s, v)
				fi
			fi;
		} 
	};

	set(s: String, v: String) : Object { 
		{ 
			set_helper(self, s, v);
		} 
	};

	
	path: List <- new Nil;
	cycleDetected: Bool <- false;

	is_cyclic(n: String, progress: SSList, edges: SSList) : Bool {
		{
			if cycleDetected then {
				path <- (new Cons).init("cycle", new Nil);
				true;
			} else {
				if progress.get(n) = "inprogress" then {
					-- cycle detected
					-- out_string("Cycle detected - iscyclic\n");
					cycleDetected <- true;
					true;
				} else {
					if progress.get(n) = "done" then {
						false;
					} else {

						progress.set(n, "inprogress");

						let iter: Int <- edges.len() - 1 in {
							edges <- edges.sort();
							while 0 <= iter loop {
								if edges.at(iter).car() = n then {
									if is_cyclic(edges.at(iter).cbr(), progress, edges) then {
										-- out_string("Cycle detected - iscyclic deep\n");
										cycleDetected <- true;
										iter <- (0 - 1);
										true;
									} else
										false	
									fi;
								} else {
									path;
								} fi;
								iter <- iter - 1;
							}
							pool;
						};

						-- set progress to "done"
						progress.set(n, "done");

						-- append to L
						path <- (new Cons).init(n, path);
						-- out_string(n);
						-- out_string("\n");

						-- return false
						false;
					}
					fi;
				} fi;
			} fi;
		}
	};

	top_sort(edges: SSList, vertices: List, progress: SSList) : List {
		{
			-- for node in vertices, call isCyclic. if cyclic return "cycle"
			-- otherwise return result

			if cycleDetected then {
				path <- (new Cons).init("cycle", new Nil);
				path;
			} else {
				edges <- edges.sort();
				vertices <- vertices.sort();

				let result: List <- new Nil in {
					let iter: Int <- vertices.len() - 1 in {
						while 0 <= iter loop {
							if is_cyclic(vertices.at(iter).car(), progress, edges) then {
								-- out_string("Cycle detected - topsort\n");
								cycleDetected <- true;
								iter <- (0 - 1);
							}	
							else 
								result
							fi;
							iter <- iter - 1;
						} 
						pool;
					};
					result;
				};

				path;
			} fi;
		}
	};
};

Class Main inherits IO {

	edges : SSList <- new SSNil;
	vertices : List <- new Nil;
	progress : SSList <- new SSNil;
	stack : List <- new Nil;


	main() : Object {{
        let reading : Bool <- true in
        while reading loop
            let task : String <- in_string() in
            let prereq : String <- in_string() in
            if prereq = "" then
                reading <- false
            else {
                edges <- (new SSCons).init(task, prereq, edges);
				
				if vertices.is_member(task) then 
					vertices
				else {
					progress <- (new SSCons).init(task, "unvisited", progress);
					vertices <- (new Cons).init(task, vertices);
				}
				fi;

				if vertices.is_member(prereq) then 
					vertices
				else {
					progress <- (new SSCons).init(prereq, "unvisited", progress);
					vertices <- (new Cons).init(prereq, vertices);
				}
				fi;
			}
            fi
        pool;

        edges <- edges.sort();
		vertices <- vertices.sort();

		let path : List <- edges.top_sort(edges, vertices, progress) in 
		{	
			path.rev().print_list();
		};
	}};
};			    
