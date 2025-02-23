class Main inherits B {
    y: Int;
    print(y: String, x:String) : Object {
        x
    };

    main() : Object {
        2
    };
};
class B inherits A {
    x : SELF_TYPE <- self;
    print(y: String, x: String) : Object {
        x
    };
};

class A inherits IO {
    z : String;
    a : Int;
};
