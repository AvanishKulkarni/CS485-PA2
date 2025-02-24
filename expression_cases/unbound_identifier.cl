class Main inherits IO {
    main(): Object {
        {
            let x: Int <- 1 in {
                out_string("1\n");
            };
            out_int(x+1);
        }
    };

};