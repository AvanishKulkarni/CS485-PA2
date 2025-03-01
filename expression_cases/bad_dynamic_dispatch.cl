class A {
    out_string(): String {
        "Class A\n"
    };
};

class B {
    out_string(): String {
        "Class B\n"
    };
};

class C inherits B {
    out_string(): String {
        "Class C\n"
    };
};


class Main inherits IO {
    a: A <- new A;
    b: B <- new C;
    main(): Object {
        {
            out_string("main\n");
            out_string(b.out_string());
        }    
    };
};