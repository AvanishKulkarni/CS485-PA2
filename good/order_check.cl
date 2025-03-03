class ACLASS {
    alpha() : Int {
        0
    };
        redef_in_C() : Int {
        0
    };
    redef_in_B() : Int {
        0
    };

};

class BCLASS inherits ACLASS {
    redef_in_B() : Int {
        0
    };
    new_in_B() : Int {
        0
    };
};

class CCLASS inherits BCLASS {
    redef_in_C() : Int {
        0
    };
    new_in_C() : Int {
        0
    };
};

class Main {
    main() : Object {
        2
    };
};