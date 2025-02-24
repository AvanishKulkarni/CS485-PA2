class A {
    print() : Int {
        1
    };
};

class B inherits A {
    print() : Int {
        2
    };
};

class Main inherits IO {

    x: A <- (new A);
    y: B <- (new A);

    main(): Object {
        1
    };
};