class Main inherits IO {
    main() :Object {
        out_int(add(1, "2", false))
    };
    add(x: Int, y: Int, z: Int) : Int {
        x+y+z
    };
};