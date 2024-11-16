        % return something
declare Env Str2Lst Parse ParseFun Infix2Prefix ParseFunction FindFunction FunctionName FunctionBody BuildTree BuildTreeHelper ParseFunctionBody ParseFunctionName

fun {Str2Lst Data}
    {String.tokens Data & }
end

%% Data is a list of the form ["(", "X", "+", "Y", ")"] en returns id prefix form ["+" "X" "Y"]
fun {Infix2Prefix Data}
    local Reverse Infix2Postfix in
        fun {Reverse Data Ans}
            case Data of H|T then
                case H of "(" then
                    {Reverse T ")"|Ans}
                []  ")" then
                    {Reverse T "("|Ans}
                else
                    {Reverse T H|Ans}
                end
            else
                Ans
            end
        end
        fun {Infix2Postfix Data Stack Res}
            local PopWhile in
                fun {PopWhile Stack Res Cond}
                    case Stack of H|T then
                        if {Cond H} then
                            {PopWhile T H|Res Cond}
                        else
                            [Res Stack]
                        end
                    else
                        [Res Stack]
                    end
                end
                case Data of H|T then
                    case H of "(" then
                        {Infix2Postfix T H|Stack Res}
                    [] ")" then
                        local H2 T2 T3 in
                            H2|T2|nil = {PopWhile Stack Res fun {$ X} {Not X=="("} end}
                            _|T3 = T2
                            {Infix2Postfix T T3 H2}
                        end
                    [] "+" then
                        local H2 T2 in
                            H2|T2|nil = {PopWhile Stack Res fun {$ X} {List.member X ["*" "/"]} end}
                            {Infix2Postfix T H|T2 H2}
                        end
                    [] "-" then
                        local H2 T2 in
                            H2|T2|nil = {PopWhile Stack Res fun {$ X} {List.member X ["*" "/"]} end}
                            {Infix2Postfix T H|T2 H2}
                        end
                    [] "*" then
                        local H2 T2 in
                            H2|T2|nil = {PopWhile Stack Res fun {$ X} {List.member X nil} end}
                            {Infix2Postfix T H|T2 H2}
                        end
                    [] "/" then
                        local H2 T2 in
                            H2|T2|nil = {PopWhile Stack Res fun {$ X} {List.member X nil} end}
                            {Infix2Postfix T H|T2 H2}
                        end
                    else
                        {Infix2Postfix T Stack H|Res}
                    end
                else
                    Res
                end
            end
        end
        {Infix2Postfix "("|{Reverse "("|Data nil} nil nil}
    end
end

fun {FindFunctionName ProgramLi}
    local X in
        {List.takeWhile ProgramLi fun {$ X} X \= "=" end  X }
        X
    end
end

fun {FindFunctionBody ProgramLi}
    local X in
        {List.dropWhile ProgramLi fun {$ X} X \= "=" end  X }
        {List.drop X 1}
    end
end



fun {FindFunctionIn ProgramLi}
    local X in
        {List.takeWhile ProgramLi fun {$ X} X \= "in" end  X }
        X
    end
end

fun {BuildTree FunBody}
    {BuildTreeHelper {Infix2Prefix FunBody}}
end

fun {BuildTreeHelper InfixBody}
    case InfixBody of H|T then
        if {List.member H ["+" "-" "*" "/"]} then
            local Left Right in
                Left = tree(left: H right: {BuildTreeHelper T})


                % Right = {BuildTreeHelper T}
                tree(left: Left right: Left.right)
            end
        else
            H
        end
    end
end


fun {FunctionName ProgramStr}
    {FindFunctionName {Str2Lst ProgramStr}}
end

fun {FunctionBody ProgramStr}
    {FindFunctionBody {Str2Lst ProgramStr}}
end

fun {ParseFunctionBody Body}
    case Body of H|T then
        case H of "var" then 
            local VarDef FunDef in
                VarDef = {FindFunctionIn T} 
                FunDef = {Reverse {FindFunctionIn {Reverse T}}}
                % [record(name: "@y" value: {FindFunctionBody VarDef}) FunDef]
                {BuildTree FunDef}
                {BuildTree {FindFunctionBody VarDef}}
                % TODO: put the var (@y) in the tree
            end 
        else {BuildTree Body}
        end
    end
end

fun {ParseFunctionName NameLi}
    [{Nth NameLi 2} {List.drop NameLi 2}]
end
    
fun {ParseFunction ProgramStr}
    local Name Body BTree in
        Name = {ParseFunctionName {FunctionName ProgramStr}}
        Body = {ParseFunctionBody {FunctionBody ProgramStr}}
        [record(name: {Nth Name 1} param: {Nth Name 2} body: {Nth Body 2}) {Nth Body 1}]

        % return something
    end
end


% {Browse {ParseFunction "fun fourtimes x y z w d f g h = var y = x * x in y + y"}}
{Browse {ParseFunction "fun fourtimes x = x * x + x * x"}}