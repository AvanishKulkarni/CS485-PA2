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

    x: B <- (new B);
    y: Int <- x@A.print();

    main(): Object {
        1
    };
};