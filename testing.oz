% Initialize:
% stack = []
% current_list = []

% Process tokens one by one:

% Token "sqr":
% Add "sqr" to current_list: current_list = ["sqr"].
% stack = []

% Token "(":
% Push current_list to stack: stack = [["sqr"]].
% Create a new sublist: current_list = [].

% Token "sqr":
% Add "sqr" to current_list: current_list = ["sqr"].
% stack = [["sqr"]].

% Token "(":
% Push current_list to stack: stack = [["sqr"], ["sqr"]].
% Create a new sublist: current_list = [].

% Token "sqr":
% Add "sqr" to current_list: current_list = ["sqr"].
% stack = [["sqr"], ["sqr"]].

% Token "2":
% Add "2" to current_list: current_list = ["sqr", "2"].
% stack = [["sqr"], ["sqr"]].

% Token ")":
% Pop the top list from stack: parent_list = ["sqr"].
% Append current_list to parent_list: parent_list = ["sqr", ["sqr", "2"]].
% Set current_list = parent_list: current_list = ["sqr", ["sqr", "2"]].

% Token ")":
% Pop the top list from stack: parent_list = ["sqr"].
% Append current_list to parent_list: parent_list = ["sqr", ["sqr", ["sqr", "2"]]].
% Set current_list = parent_list: current_list = ["sqr", ["sqr", ["sqr", "2"]]].

% Final Output:
% ["sqr", ["sqr", ["sqr", "2"]]]



declare ParseFunctionCallHelper 
fun {ParseFunctionCallHelper Li Stack CurrentList}
    % {Show Stack#CurrentList}
    case Li 
    of H | T then 
        {Show '-------------------------'}
        {Show 'This is H:'#H}
        {Show 'And this is T:'#T}
        {Show 'And this is Stack:'#Stack}
        {Show 'And this is CurrentLi:'#CurrentList}
        case H 
        of "(" then 
            {ParseFunctionCallHelper T CurrentList | Stack nil}
        [] ")" then 
            local Rest ParentList AppRest in
                ParentList = {Nth {List.take Stack 1} 1} % pops first element
                {Show 'popTop'#ParentList}
                Rest = {List.drop Stack 1} % rest of stack
                {Show 'restStack'#Rest}
                AppRest = {Append ParentList (CurrentList | nil)}
                {Show 'appRest'#AppRest}
                {ParseFunctionCallHelper T Rest AppRest }
            end 
        [] _ then 
    
            local NewCurrent in 
                {Show 'CurrentList'#CurrentList}

                if CurrentList == nil then 
                    NewCurrent = [H]
                else 
                    NewCurrent = {Append CurrentList [H]}
                end
                {Show 'NewCurrent'#NewCurrent}
                {ParseFunctionCallHelper T Stack NewCurrent}
            end
        end
    else 
       CurrentList
    end
end

local 
    % Lst = ["sqr" "(" "sqr" "(" "sqr" "2" ")" ")"]
    % Lst = ["sum_n" "1" "(" "sum_n" "1" "1" "1" "2" ")" "3" "2"]
    Lst = ["sum_n" "1"]
in 
    {Browse {ParseFunctionCallHelper Lst nil nil}}
end

