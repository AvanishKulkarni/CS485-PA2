-- self-type LUB issues 
class A inherits Main {};

class Main inherits IO {
    main(): SELF_TYPE {{
        if true then {
            (new A);
        } else {
            (new Main);
        } fi;
    }};
};
