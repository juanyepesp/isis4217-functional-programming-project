

fun {Tree List Op Refs}
    case Op of nil
    then
    else
        case List of
            [H | T] then if {List.memeber H ["+", "-", "*", "/"]} then H else end 
    end 
    end
end
