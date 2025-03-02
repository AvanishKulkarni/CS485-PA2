class A {};
class B inherits A {};
class C inherits A {};
class Main inherits IO {
    x : A <- if true then
            (new B)
        else
            (new C)
        fi;
    main(): Object {
        1
    };
};