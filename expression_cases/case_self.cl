class Main inherits IO {
    main(): Int {
        0
    };

    case_self(): Int { 
        {
            case 1 of
                self:SELF_TYPE => {0;};
            esac;
        }
    };
};