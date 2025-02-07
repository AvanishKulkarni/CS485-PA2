class Main inherits IO {
    main(): Int {
        0
    };

    case_self(): Int { 
        {
            case self of
                self:SELF_TYPE => {0;};
            esac;
        }
    };
};