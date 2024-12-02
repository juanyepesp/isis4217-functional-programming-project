declare RecursiveCallingWithParentheses Reference Node

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



fun {RecursiveCallingWithParentheses CallList ReferencesList}
  if {Not {Or {List.member "(" CallList} {List.member ")" CallList}}} then
      {List.zip ReferencesList {List.drop CallList 1} fun{$ Ref Arg}
      local S in
        S = {StringToInt Arg}
        item(name: Ref value: S)
      end
    end}
    % ! THIS RETURN ASSOC WITH PARAMS AND ARGS

    % sqr 3 -> recordEsteba -> iterar sobre su lista de References y actualizarlos con ese return
    % ahi ya podemos llamar lo de esteban con una funcion simple.

      
  else
      local Li FinalLi WithoutParen in
        {List.dropWhile CallList fun {$ X} X \= "(" end  Li }
        {List.dropWhile {Reverse Li} fun {$ X} X \= ")" end  FinalLi }
        WithoutParen =  {List.drop {Reverse FinalLi} 1 }
        {RecursiveCallingWithParentheses {List.take WithoutParen {List.length  WithoutParen}-1} ReferencesList}
      end
  end
end

local ReferencesList in
  ReferencesList = ["x"]
  {Browse {RecursiveCallingWithParentheses ["sqr" "(" "sqr" "(" "sqr"  "2" ")" ")"] ReferencesList}}
end
