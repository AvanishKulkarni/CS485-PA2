class A inherits IO {
    print(): Object {
        out_string("A\n")
    };
};

class A inherits IO {
    print(): Object {
        out_string("A redef\n")
    };
};

class Main inherits IO {
    main(): Object {
        {
            out_string("Main\n");
        }
    };
};