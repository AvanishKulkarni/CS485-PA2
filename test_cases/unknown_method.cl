class Main inherits IO {
    y:List <- (new List).init(2);
    main(): Object {
        y.print()
    };
};

class List {
    x: Int;
    init(y: Int) : List{ 
        {
            x<- y;
            self;
        }
    };
};