class ACLASS {
    alpha() : Int {
        0
    };
    redef_in_B() : Int {
        2121
    };
    redef_in_C() : Int {
        5657
    };

};

class BCLASS inherits ACLASS {
    redef_in_B() : Int {
        2121
    };
    new_in_B() : Int {
        1234
    };
};

class CCLASS inherits BCLASS {
    redef_in_C() : Int {
        5657
    };
    new_in_C() : Int {
        7567
    };
};

class Main {
    main() : Object {
        2
    };
};