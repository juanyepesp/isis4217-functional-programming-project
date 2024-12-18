declare Env Str2Lst Parse ParseFun Infix2Prefix ParseFunction FindFunction ParseFunctionBody ParseFunctionName GetFunDef GetFunCall Reference Node Tree ListToDict ListToDictHelper SetReferencesOnTreeForCall ParseFunctionCall ParseFunctionCallHelper


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AUX FROM NICOLAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {Str2Lst Data}
    {String.tokens Data & }
end

% Data is a list of the form ["(", "X", "+", "Y", ")"] en returns id prefix form ["+" "X" "Y"]
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CLASSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class Reference
    attr varName value
    meth init(VarName Value)
        varName := VarName
        value := Value
    end

    meth getVarName(Res)
        Res := @varName
    end

    meth setVarName(Val)
        varName := Val
    end

    meth getValue(Res)
        Res := @value
    end

    meth setValue(Val)
        value := Val
    end
end

class Node
    attr value left right ref
    meth init(Value Left Right Ref)
        value := Value
        left := Left
        right := Right
        ref := Ref
    end

    meth getValue(Res)
        Res := @value
    end

    meth setValue(Val)
        value := Val
    end

    meth getLeft(Res)
        Res := @left
    end

    meth setLeft(Val)
        left := Val
    end

    meth getRight(Res)
        Res := @right
    end

    meth setRight(Val)
        right := Val
    end

    meth getRef(Res)
        Res := @ref
    end

    meth setRef(Val)
        ref := Val
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARSING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Isolates all thats on the LHS of the =
fun {FindFunctionName ProgramLi}
    local X in
        {List.takeWhile ProgramLi fun {$ X} X \= "=" end  X }
        X
    end
end

% Returns the function body (things after the equal)
fun {FindFunctionBody ProgramLi}
    local X in
        {List.dropWhile ProgramLi fun {$ X} X \= "=" end  X }
        {List.drop X 1}
    end
end

% Returns the function DEFINITION as a list
%["fun" "sum" "x" "y" "z" "=" "(" "x" "+" "y" ")" "*" "z"]
fun {GetFunDef ProgramLi }
    local X in
        {List.takeWhile ProgramLi fun {$ X} X \= "\n" end  X }
        X
    end
end

% Returns the function CALL as a list
%["sum" "4" "5" "6"]
fun {GetFunCall ProgramLi }
    local X in
        {List.dropWhile ProgramLi fun {$ X} X \= "\n" end  X }
        {List.drop X 1}

    end
end

fun {FindFunctionIn ProgramLi}
    local X in
        {List.takeWhile ProgramLi fun {$ X} X \= "in" end  X }
        X
    end
end

% Returns the function name and parameters ["sum" ["x" "y" "z"]]
fun {ParseFunctionName NameLi}
    [{Nth NameLi 2} {List.drop NameLi 2}]
end

% Returns a list of records with name - value association
fun {RecursiveParsingWithParentheses CallList ReferencesList}
    % CallList - Function application as list e.g. ["fourtimes" "4"]
    % ReferenceList - Function's def references e.g. ["x"]
  if {Not {Or {List.member "(" CallList} {List.member ")" CallList}}} then
      {List.zip ReferencesList {List.drop CallList 1} fun{$ Ref Arg}
      local S in
        S = {StringToFloat Arg}
        item(name: Ref value: S)
      end
    end}

  else
      local Li FinalLi WithoutParen in
        {List.dropWhile CallList fun {$ X} X \= "(" end  Li }
        {List.dropWhile {Reverse Li} fun {$ X} X \= ")" end  FinalLi }
        WithoutParen =  {List.drop {Reverse FinalLi} 1 }
        {RecursiveParsingWithParentheses {List.take WithoutParen {List.length  WithoutParen}-1} ReferencesList}
      end
  end
end

fun {ParseFunctionCall OriginalCallLi}
    {ParseFunctionCallHelper OriginalCallLi nil nil}
end

fun {ParseFunctionCallHelper Li Stack CurrentList}
    % {Show Stack#CurrentList}
    case Li
    of H | T then
        % {Show '-------------------------'}
        % {Show 'This is H:'#H}
        % {Show 'And this is T:'#T}
        % {Show 'And this is Stack:'#Stack}
        % {Show 'And this is CurrentLi:'#CurrentList}
        case H
        of "(" then
            {ParseFunctionCallHelper T CurrentList | Stack nil}
        [] ")" then
            local Rest ParentList AppRest in
                ParentList = {Nth {List.take Stack 1} 1} % pops first element
                % {Show 'popTop'#ParentList}
                Rest = {List.drop Stack 1} % rest of stack
                % {Show 'restStack'#Rest}
                AppRest = {Append ParentList (CurrentList | nil)}
                % {Show 'appRest'#AppRest}
                {ParseFunctionCallHelper T Rest AppRest }
            end
        [] _ then

            local NewCurrent in
                % {Show 'CurrentList'#CurrentList}

                if CurrentList == nil then
                    NewCurrent = [H]
                else
                    NewCurrent = {Append CurrentList [H]}
                end
                % {Show 'NewCurrent'#NewCurrent}
                {ParseFunctionCallHelper T Stack NewCurrent}
            end
        end
    else
       CurrentList
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BUILDING TREE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% LITERALLY makes a tree
fun {Tree Tokens Op}
    if Op == nil then
        case Tokens of H|T then
            if {Member H ["+" "-" "*" "/"]} then
                local UnusedRemainingTokens LeftRecord LeftTree LeftUsedTokens LeftRefs RightRecord RightTree RightUsedTokens RightRefs CurrentNode AppendedUsedStringsList AppendedRefsList in
                    LeftRecord = {Tree T H}
                    LeftTree = LeftRecord.1
                    LeftUsedTokens = LeftRecord.2
                    LeftRefs = LeftRecord.3

                    UnusedRemainingTokens = {FoldL LeftUsedTokens fun {$ Acc Elem}
                        case Acc of H|T then
                            if H == Elem then
                                T
                            else
                                Acc
                            end
                        end
                    end Tokens}

                    RightRecord = {Tree UnusedRemainingTokens nil}
                    RightTree = RightRecord.1
                    RightUsedTokens = RightRecord.2
                    RightRefs = RightRecord.3

                    CurrentNode = {New Node init("@" LeftTree RightTree nil)}
                    AppendedUsedStringsList = {Append LeftUsedTokens RightUsedTokens}
                    AppendedRefsList = {Append LeftRefs RightRefs}

                    record(CurrentNode AppendedUsedStringsList AppendedRefsList)
                end
            else
                local CurrentNode Ref UsedTokens CurrentRefs ReplaceValue in
                    if {Or {String.isInt H} {String.isFloat H}} then
                        ReplaceValue = {StringToFloat H}
                    else
                        ReplaceValue = nil
                    end

                    Ref = {New Reference init(H ReplaceValue)}
                    CurrentNode = {New Node init(H nil nil Ref)}
                    UsedTokens = [H]

                    if ReplaceValue == nil then
                        CurrentRefs = [Ref]
                    else
                        CurrentRefs = nil
                    end

                    record(CurrentNode UsedTokens CurrentRefs)
                end
            end
        else
            Tokens
        end
    else
        local Rec RightTree UsedStrings Refs LeftTree CurrentNode CurrentUsedStrings in
            Rec = {Tree Tokens nil}
            RightTree = Rec.1
            UsedStrings = Rec.2
            Refs = Rec.3

            LeftTree = {New Node init(Op nil nil nil)}
            CurrentNode = {New Node init("@" LeftTree RightTree nil)}
            CurrentUsedStrings =  Op | UsedStrings

            record(CurrentNode CurrentUsedStrings Refs)
        end
    end
end

% Returns tree for all types of functions (with var or not)
fun {ParseFunctionBody Body}
    case Body of H|T then
        case H of "var" then
            local VarDef FunDef VarName VarBody VarTree FunTree in
                VarDef = {FindFunctionIn T}
                VarName = {Nth {List.take VarDef 1} 1} % ["y"]
                VarBody = {List.drop VarDef 2}
                FunDef = {Reverse {FindFunctionIn {Reverse T}}}
                VarTree = {Tree {Infix2Prefix VarBody} nil} % y = x * x -> x * x
                FunTree = {Tree {Infix2Prefix FunDef} nil}
                {ReplaceReferencesInTree FunTree VarName VarTree.1}
                record(FunTree.1 FunTree.2 VarTree.3) % Replace references with those of inner variables (ie. param)

            end
        else {Tree {Infix2Prefix Body} nil}
        end
    else nil
    end
end


% Replaces Var References or FunctionCall References
proc {ReplaceReferencesInTreeHelper NodeElem ThingToReplace TreeToInsert}
    local NodeValue = {NewCell nil} LeftNode = {NewCell nil} RightNode = {NewCell nil} LeftValue = {NewCell nil} RightValue = {NewCell nil} in
        {NodeElem getValue(NodeValue)}
        {NodeElem getLeft(LeftNode)}
        {NodeElem getRight(RightNode)}
        if @NodeValue == "@" then
            {@LeftNode getValue(LeftValue)}
            {@RightNode getValue(RightValue)}

            if @LeftValue == ThingToReplace then
                {NodeElem setLeft(TreeToInsert)}
            else
                {ReplaceReferencesInTreeHelper @LeftNode ThingToReplace TreeToInsert}
            end

            if @RightValue == ThingToReplace then
                {NodeElem setRight(TreeToInsert)}
            else
                {ReplaceReferencesInTreeHelper @RightNode ThingToReplace TreeToInsert}
            end
        elseif @LeftNode \= nil andthen @RightNode \= nil then
            {ReplaceReferencesInTreeHelper @LeftNode ThingToReplace TreeToInsert}
            {ReplaceReferencesInTreeHelper @RightNode ThingToReplace TreeToInsert}
        end
    end
end

proc {ReplaceReferencesInTree FuncTree ThingToReplace TreeToInsert}
    {ReplaceReferencesInTreeHelper FuncTree.1 ThingToReplace TreeToInsert}
end

% Update tree reference list with actual call values
fun {SetReferencesOnTreeForCall TreeReferences FunctionCallParameters}
    % TreeReferences = [REFOBJECT(varname: "x", value: nil), REFOBJECT(varname: "y", value: nil)]
    % FunctionCallParameters = [item(name: "x" value: 4), item(name: "y" value: 5)] -> but a dictionary
    % Expected output: [REFOBJECT(varname: "x", value: 4), REFOBJECT(varname: "y", value: 5)]
    case TreeReferences of H|T then
        case H of nil then
            TreeReferences
        else
            local VarName VarNameAtom VarValue in
                VarName = {NewCell 0}
                {H getVarName(VarName)}
                VarNameAtom = {StringToAtom @VarName}
                VarValue = {Dictionary.get FunctionCallParameters VarNameAtom}
                {H setValue(VarValue)}
                {SetReferencesOnTreeForCall T FunctionCallParameters}
            end
        end
    else
        TreeReferences
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following two functions turn an array of records into a dictionary.
% NOTE: keys are atoms, proper conversion is needed
fun {ListToDictHelper ListOfRecords Dict}
    case ListOfRecords of H|T then
        case H of nil then Dict
        else
            {Dictionary.put Dict {StringToAtom H.name} H.value}
            {ListToDictHelper T Dict }
        end
    else Dict
    end
end

fun {ListToDict ListOfRecords}
    local Dict in
        {Dictionary.new Dict}
        {ListToDictHelper ListOfRecords Dict}
    end
end

proc {PrintTree Tree}
    local NodeElem = {NewCell nil} Ans = {NewCell nil} in
        {Tree getValue(Ans)}
        {System.showInfo @Ans}

        {Tree getLeft(NodeElem)}
        if @NodeElem == nil then
            {Show 'Left is nil'}
        else
            {PrintTree @NodeElem}
        end

        {Tree getRight(NodeElem)}
        if @NodeElem == nil then
            {Show 'Right is nil'}
        else
            {PrintTree @NodeElem}
        end
        {Show ''}
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TREE EVAULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {EvalTree NodeElem}
    local NodeValue = {NewCell nil} Ref = {NewCell nil} RefValue = {NewCell nil} LeftRecord RightRecord LeftNode = {NewCell nil} RightNode = {NewCell nil} LeftValue LeftOp RightValue in
        {NodeElem getValue(NodeValue)}
        if @NodeValue == "@" then
            {NodeElem getLeft(LeftNode)}
            LeftRecord = {EvalTree @LeftNode}
            LeftValue = LeftRecord.1
            LeftOp = LeftRecord.2

            {NodeElem getRight(RightNode)}
            RightRecord = {EvalTree @RightNode}
            RightValue = RightRecord.1

            if LeftValue == nil then
                record(RightValue LeftOp)
            else
                record({EvalOp LeftValue RightValue LeftOp} nil)
            end

        elseif {Member @NodeValue ["+" "-" "*" "/"]} then
            record(nil @NodeValue)
        else
            {NodeElem getRef(Ref)}
            {@Ref getValue(RefValue)}
            record(@RefValue nil)
        end
    end
end

fun {EvalOp LeftValue RightValue Op}
    case Op of
        "+" then
            LeftValue + RightValue
        [] "-" then
            LeftValue - RightValue
        [] "*" then
            LeftValue * RightValue
        [] "/" then
            LeftValue / RightValue
    end
end

fun {ApplyFunction FunctionList FuncDefTree RefList}
    % {Show 'Applying Function....'#FunctionList}
    local FunctionParamCall FunctionParamCallDict Ans in
        FunctionParamCall = {RecursiveParsingWithParentheses FunctionList RefList}

        % {Show 'ParamsAssignment'#FunctionParamCall}
        FunctionParamCallDict = {ListToDict FunctionParamCall}
        _ = {SetReferencesOnTreeForCall FuncDefTree.3 FunctionParamCallDict}
        % {Show 'TreeWithReferences'#Ret}
        Ans = {EvalTree FuncDefTree.1}
        Ans.1
    end
end

fun {EvalFunctionCall CallList CurrentList FuncDefTree RefList}
    % {Show CallList#CurrentList}
    case CallList of H|T then
        if {String.is H} then
            {EvalFunctionCall T {Append CurrentList (H | nil)} FuncDefTree RefList}
        else
            local Res in
                Res = {EvalFunctionCall H nil FuncDefTree RefList}
                % {Show Res}
                {EvalFunctionCall T {Append CurrentList (Res | nil)} FuncDefTree RefList}
            end
        end
    [] nil then {ApplyFunction CurrentList FuncDefTree RefList}
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN CALLER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {Run ProgramStr}
    local ProgramLi FunDefiniton FunCall Name FuncDefTree RefList ParsedFunCall in
        ProgramLi = {Str2Lst ProgramStr}

        FunDefiniton = {GetFunDef ProgramLi}
        FuncDefTree = {ParseFunctionBody {FindFunctionBody FunDefiniton}}
        % {PrintTree FuncDefTree.1}

        FunCall = {GetFunCall ProgramLi}
        ParsedFunCall = {ParseFunctionCall FunCall}

        Name = {ParseFunctionName {FindFunctionName FunDefiniton}}
        RefList = {Nth Name 2}

        {EvalFunctionCall ParsedFunCall nil FuncDefTree RefList}
    end
end

{Browse '---------------------- Examples provided in class ----------------------'}

{Browse {Run "fun fourtimes x z = var y = x * z in y + y \n fourtimes 4 3"}} % :)
{Browse {Run "fun sum x y z = ( x + y ) * z \n sum 4 5 6"}} % :)
{Browse {Run "fun sqr x = x * x \n sqr ( sqr ( sqr 2 ) )"}} % :)
{Browse {Run "fun sum_n x y z n = ( x + y + z ) * n \n sum_n 1 ( sum_n 1 1 1 2 ) 3 2"}}
{Browse {Run "fun square x = x * x \n square 3"}}
{Browse {Run "fun square x = x * x \n square ( square 3 )"}}
{Browse {Run "fun arithmetic x y = ( ( x + y ) / ( x - y ) ) * 2 \n arithmetic ( arithmetic 5 6 ) ( arithmetic 2 11 )"}}
{Browse {Run "fun sqr x = ( x + 1 ) * ( x - 1 ) \n sqr 4"}}
{Browse {Run "fun fourtimes x = var y = x + x in y + y \n fourtimes ( fourtimes 16 )"}}
