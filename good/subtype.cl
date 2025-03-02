class A {
    print() : Int {
        1
    };
};

class B inherits A {
    print() : Int {
        2
    };
    testB() : String {
        "Hello"
    };
};

class Main inherits IO {

    x: A <- (new A);
    y: A <- (new B);

    main(): Object {
        {
            1;
        }
    };
};