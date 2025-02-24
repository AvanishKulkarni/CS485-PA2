class Main inherits IO {
    a : Int <- (1 + 2)
    -4 +
    (5*
    (2-
    3));
    b : Int <- 1 - 2;
    c : Int <- 1 * 2;
    d : Int <- 4 / 2;
    e : Bool <- Isvoid(1);
    f : Bool <- 1 = 2;
    g : Bool <- 1 < 2;
    h : Bool <- 1 <= 2;
    i : Bool <- not false;
    j : Int <- ~10;
    k : Object <-
    let test : Int <- 1 in
    while test < 3 loop
        test <- test + 1
    pool;
    l : Int <-
    if true then
        2
    else
        4
    fi;
    aa: Int;
    m : String <- "Hello";
    n : Int <- 1;
    o : Bool <- false;
    p : SELF_TYPE;


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