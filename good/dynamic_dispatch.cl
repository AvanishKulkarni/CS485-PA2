class A {
    print(x: Int, y: Int) : Int {
        x+1
    };
};

class B inherits A {
    print(x: Int, y: Int) : Int {
        x+2
    };
};

class Main inherits IO {

    x: Int <- (new A).print(1, 2);
    y: Int <- (new B).print(3, 3);

    main(): Object {
        1
    };
};