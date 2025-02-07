class A inherits C {
    init() : SELF_TYPE {
        self
    };
};

class B inherits A {
    init() : SELF_TYPE {
        self
    };
};

class C inherits B {
    init() : SELF_TYPE {
        self
    };
};

class Main inherits IO {

    x : A <- new C;

    main(): Object {
        let output: String in {
            case x of
                x:A => {output <- "A";};
                x:B => {output <- "B";};
                x:C => {output <- "C";};
            esac;
            out_string(output);
            out_string("\n");
        }
            
    };
};