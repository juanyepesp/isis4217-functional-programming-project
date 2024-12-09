declare EvalFunctionCall

fun {ApplyFunction FunctionList}
    {Show 'Applying Function....'#FunctionList}
    % FunctionList
    "4"
end

fun {EvalFunctionCall CallList CurrentList}
    {Show CallList#CurrentList} 
    case CallList of H|T then
        if {String.is H} then
            {EvalFunctionCall T {Append CurrentList (H | nil)}}
        else
            local Res in 
                Res = {EvalFunctionCall H nil}
                {Show Res}
                {EvalFunctionCall T {Append CurrentList (Res | nil)}}
            end
        end
    [] nil then {ApplyFunction CurrentList}
    end
end

local 
    % CallList = ["sum_n" "1" ["sum_n" "1" "1" "1" "2"] "3"] 
    CallList = ["sqr" [ "sqr" [ "sqr" "2" ] ]]
in
    {Browse {EvalFunctionCall CallList nil}}
end