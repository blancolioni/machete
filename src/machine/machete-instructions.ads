package Machete.Instructions is

   type Machete_Instruction is
     (
      --  Execution
      Break, Stop, No_Operation,

      --  Control instructions
      Allocate, Deallocate, Call, Execute, Proceed,

      --  Put instructions
      Put_Variable, Put_Value, Put_Unsafe_Value, Put_Constant, Put_Integer,
      Put_Nil, Put_Structure, Put_List,

      --  Get instructions
      Get_Variable, Get_Value, Get_Constant, Get_Integer, Get_Nil,
      Get_Structure, Get_List,

      --  Set instructions
      Set_Variable, Set_Value, Set_Constant, Set_Integer,

      --  Unify instructions
      Unify_Void, Unify_Variable, Unify_Value, Unify_Local_Value,
      Unify_Constant, Unify_Integer,

      --  Indexing instructions
      Try_Me_Else, Retry_Me_Else, Trust_Me,
      Try, Re_Try, Trust,
      Switch_On_Term, Switch_On_Constant, Switch_On_Structure,

      --  Cut instructions
      Neck_Cut, Get_Level, Cut,

      --  Miscellaneous
      Fail, Trail
     );

end Machete.Instructions;
