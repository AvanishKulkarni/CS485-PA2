class Main inherits IO {
    main(): Int {
        0
    };

    random(x: Int): Int { x };
};

class Silly inherits Main {
    random(x: Int, y: Int): Int {
        5
    };
};