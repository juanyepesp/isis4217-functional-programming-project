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


    fun {Tree Tokens Op}
        {Show 'Recurring'}
        case Op of nil then
            case Tokens of H|T then
                if {Member H ["+" "-" "*" "/"]} then
                    local UnusedRemainingTokens LeftRecord LeftTree LeftUsedTokens LeftRefs RightRecord RightTree RightUsedTokens RightRefs CurrentNode AppendedUsedStringsList AppendedRefsList in
                        LeftRecord = {Tree T H}
                        {Show LeftRecord}
                        % {Show {Length LeftRecord}}
                        {Show 'Left Record done'}
                        LeftTree = LeftRecord.1
                        {Show 'Left Tree done'}
                        LeftUsedTokens = LeftRecord.2
                        {Show 'Left Used Tokens done'}
                        LeftRefs = LeftRecord.3


                        {Show 'Folding'}
                        UnusedRemainingTokens = {FoldL LeftUsedTokens fun {$ Acc Elem}
                            case Acc of H|T then
                                if H == Elem then
                                    T
                                else
                                    Acc
                                end
                            end
                        end Tokens}

                        {Show 'Right Record'}
                        RightRecord = {Tree UnusedRemainingTokens nil}
                        RightTree = RightRecord.1
                        RightUsedTokens = RightRecord.2
                        RightRefs = RightRecord.3

                        CurrentNode = {New Node init(H LeftTree RightTree nil)}
                        AppendedUsedStringsList = {Append LeftUsedTokens RightUsedTokens}
                        AppendedRefsList = {Append LeftRefs RightRefs}

                        record(CurrentNode AppendedUsedStringsList AppendedRefsList)
                    end
                else
                    {Show 'Found leaf'}
                    {Show H}
                    local CurrentNode Ref UsedTokens CurrentRefs in
                        Ref = {New Reference init(H nil)}
                        CurrentNode = {New Node init(H nil nil Ref)}
                        UsedTokens = [H]
                        CurrentRefs = [Ref]

                        record(CurrentNode UsedTokens CurrentRefs)
                    end
                end
            else
                Tokens
            end
        else
            {Show 'Operation found'}
            {Show Op}
            local Rec RightTree UsedStrings Refs LeftTree CurrentNode CurrentUsedStrings in
                Rec = {Tree Tokens nil}
                RightTree = Rec.1
                UsedStrings = Rec.2
                Refs = Rec.3
                {Show Refs}

                {Show 'Operator left tree done'}
                LeftTree = {New Node init(Op nil nil nil)}
                CurrentNode = {New Node init("@" LeftTree RightTree nil)}
                CurrentUsedStrings =  Op | UsedStrings
                {Show 'Done'}

                record(CurrentNode CurrentUsedStrings Refs)
            end
        end
    end

local NodeList in
    NodeList = {Tree ["-" "*" "x" "y" "+" "z" "w"] nil}
    {Show NodeList}
end
