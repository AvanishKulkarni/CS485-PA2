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

    x: Int <- (new A).print();
    y: Int <- (new B).print();

    main(): Object {
        1
    };
};