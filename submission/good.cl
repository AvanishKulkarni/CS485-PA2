class Main inherits IO {
    a : Int <- (1 + 2) -4 + (5* (2- 3));
    b : Bool <- Isvoid(1);
    c : Bool <- 1 = 2;
    d : Bool <- 1 < 2;
    e : Bool <- (new A) = (new B);
    f : Bool <- not false;
    g : Int <- ~10;
    h : Object <- let test : Int <- 1 in
        while test < 3 loop
            test <- test + 1
        pool;
    i : Int <- if true then 2 else 4 fi;
    j: Int;
    k : String <- "Hello";
    l : Int <- 1;
    m : Bool <- false;
    n : SELF_TYPE <- (new Main);

    x: Int <- case 1 of 
        one: Int => 2;
        two: String => 3;
    esac;


    main() : Object {
        1
    };

    
};

class A inherits Main {
    alpha : String;
    beta : Int <- 2+5;
};

class B inherits Main {
    charlie : Int <- a-5;
    delta : String <- "Hello";
};

class D inherits A {
    abra : Int;
    charles: String;
};