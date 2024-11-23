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

local Rec NodeElem Refs Ref1 Ref2 Ref3 Ref4 Res in
    Rec = {Tree ["-" "*" "x" "y" "+" "z" "w"] nil}
    {Show Rec}
    NodeElem = Rec.1
    Refs = Rec.3

    Ref1 = {Nth Refs 1}
    Ref2 = {Nth Refs 2}
    Ref3 = {Nth Refs 3}
    Ref4 = {Nth Refs 4}

    {Ref1 setValue(5)}
    {Ref2 setValue(5)}
    {Ref3 setValue(5)}
    {Ref4 setValue(5)}

    Res = {EvalTree NodeElem}
    {Show Res.1}



end
