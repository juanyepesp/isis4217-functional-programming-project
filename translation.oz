
declare
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


    fun {Tree List Op Refs} % tri tuple (Node, List[Strings], List[Reference])
        {Show 'Recurring'}
        {Show record(node: List op: Op refs: Refs)}
        case Op of nil then
            {Show 'Operator is nil'}
            % {Show List}
            % {System.showInfo List}
            case List of H|T then
                {Show 'entro a la list'}
                {Show {Member H ["+" "-" "*" "/"]}}
                {Show H}
                {System.showInfo H}
                if {Member H ["+" "-" "*" "/"]} then
                    local ListWithoutUsed LeftRecord LeftTree LeftUsedStrings LeftRefs RightRecord RightTree RightUsedStrings RightRefs in
                        LeftRecord = {Tree T H nil}

                        LeftTree = {Nth LeftRecord 1}
                        LeftUsedStrings = {Nth LeftRecord 2}
                        LeftRefs = {Nth LeftRecord 3}

                        ListWithoutUsed = {FoldL LeftUsedStrings fun {$ Acc Elem}
                            case Acc of [H|T] then
                                if {List.head H} = Elem then
                                    T
                                else
                                    Acc
                                end
                            end
                        end List}

                        RightRecord = {Tree ListWithoutUsed nil nil}

                        RightTree = {Nth RightRecord 1}
                        RightUsedStrings = {Nth RightRecord 2}
                        RightRefs = {Nth RightRecord 3}

                        [{New Node init("@" LeftTree RightTree nil)} | [{List.append(LeftUsedStrings RightUsedStrings)}] | [{List.append(LeftRefs RightRefs)}]]
                    end
                else
                    if H \= nil then
                        {Show 'H is not nil'}
                        local ThisNode Ref UsedStringsList CurrentRefs in

                            {Show 'assign ref'}
                            Ref = {New Reference init(H nil)}
                            {Show 'assigned node'}
                            ThisNode = {New Node init(H nil nil Ref)}
                            {Show 'assigned used strings'}
                            % UsedStringsList = H | nil
                            UsedStringsList = [H]
                            {Show 'assigned refss'}
                            % CurrentRefs = Ref | Refs
                            if Refs == nil then
                                CurrentRefs = [Ref]
                            else
                                CurrentRefs = {List.append Ref Refs}
                            end


                            {Show 'hola'}
                            {Show 'sldkfj'}
                            {Show UsedStringsList}
                            {Show CurrentRefs}
                            {Show 'next'}
                            {Show 'end'}
                            ThisNode | UsedStringsList | CurrentRefs


                        end
                    else
                        {Show 'H is nil'}
                        [{New Node init("" nil nil nil)} | nil | nil]
                    end
                end
            else
                {Show 'lskdjflskdjfls'}
                List
            end
        else
            {Show 'Operator is not nil'}
            local Rec RightTree UsedStrings ActualRefs in
                Rec = {Tree List nil Refs}
                RightTree = {Nth Rec 1}
                UsedStrings = {Nth Rec 2}
                ActualRefs = {Nth Rec 3}

                [{New Node init("@" {New Node init(Op nil nil nil)} RightTree)} | [Op | UsedStrings] | [{List.append(Refs ActualRefs)}]]
            end
        end
    end

local NodeList in
    NodeList = {Tree ["-" "*" "x" "y" "+" "z" "w"] nil nil}
    {Show NodeList}
end
