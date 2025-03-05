class Main inherits IO {
    a : Int <- (1 + 2) -4 + (5* (2- 3));
    b : Bool <- Isvoid(1);
    c : Bool <- 1 = 2;
    d : Bool <- 1 < 2;
    e : Bool <- (new A) = (new B);
    f : Bool <- not false;
    g : Int <- ~10;
    h : Object <- let test : Int <- 1 in
        while test < 3 loop
            test <- test + 1
        pool;
    i : Int <- if true then 2 else 4 fi;
    j: Int;
    k : String <- "Hello";
    l : Int <- 1;
    m : Bool <- false;
    n : SELF_TYPE;

    o: Int <- case 1 of 
            one: Int => 2;
            two: String => 3;
        esac;

    print() : Object {
        out_string(self.type_name().substr((new Int), 1).concat("\n"))
    };

    main() : Object {{
        -- dynamic dispatch 
        out_int((new A).print(5, (new Int)));
        out_string("\n");
        
        out_int((new B).print(1, (new Int)));
        out_string("\n");

        -- static dispatch
        out_int(((new B)@A.print(1, (new Int))));
        out_string("\n");

        -- self dispatch
        print();

        -- inherited methods
        (new C).print(0, 0);
    }};

};

class A inherits IO {
    x: Int <- 5;
    print(x: Int, y: Int) : Int {
        x+y
    };
};

class B inherits A {
    print(x: Int, y: Int) : Int {
        x+(y+1)*2
    };
};

class C inherits B {
    y: Int <- 5;

    print(x: Int, y: Int) : Int {{
        out_string("super earth... our home\n");
        1;

        if 0 < y then {
            print(0, 0);
            y <- y - 1;
        } else 
            (new Int)
        fi;
    }};
};