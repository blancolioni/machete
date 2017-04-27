with Ada.Characters.Handling;
with Ada.Text_IO;

with Machete.Instructions;               use Machete.Instructions;

with Machete.Terms.Images;

with Machete.Cells;
with Machete.Images;
with Machete.Logging;

with Machete.Address_Stacks;

with Machete.Compiler;

package body Machete.Machine is

   Log_Execution : constant Boolean := False;

   type Machine_Predicate_Argument is
     new Machete.Functors.Predicate_Argument_Interface with
      record
         Machine : access Machete_Machine'Class;
         Address : Machete_Address;
      end record;

   overriding function Is_Bound
     (Argument : Machine_Predicate_Argument)
      return Boolean;

   overriding function Value
     (Argument : Machine_Predicate_Argument)
      return Machete.Terms.Machete_Term;

   overriding procedure Bind_Value
     (Argument : in out Machine_Predicate_Argument;
      Value    : Machete.Terms.Machete_Term);

   function Scan_Cell
     (Machine : in out Machete_Machine'Class)
      return Machete.Cells.Cell_Type
     with Unreferenced;

   procedure Log_Instruction
     (Address     : Machete_Address;
      Instruction : Machete.Instructions.Machete_Instruction;
      Operand_1   : String := "";
      Operand_2   : String := "";
      Operand_3   : String := "");

   function Log
     (Instruction : Machete.Instructions.Machete_Instruction)
      return String
   is (Ada.Characters.Handling.To_Lower
       (Machete.Instructions.Machete_Instruction'Image (Instruction)));

   function Log
     (Address : Machete_Address)
      return String
   is (Machete.Images.Address_Image (Address));

   function Log
     (Value : Integer)
      return String
   is (Integer'Image (Value));

   function Log
     (Octet : Word_8)
      return String
   is (Machete.Images.Image (Word_32 (Octet), 2));

   function Log
     (Functor : Machete_Functor)
      return String
   is (Machete.Functors.Image (Functor));

   function Log
     (Register : Machete_Register)
      return String
   is (Machete.Code.Register_Image (Register));

   type Handler is access
     procedure (Machine : in out Machete_Machine'Class);

   procedure Bad_Instruction
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Allocate
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Deallocate
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Call
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Get_Constant
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Get_Integer
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Get_Structure
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Get_Value
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Get_Variable
     (Machine : in out Machete_Machine'Class);

   procedure Eval_No_Operation
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Proceed
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Stop
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Unify_Constant
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Unify_Integer
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Unify_Value
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Unify_Variable
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Put_Constant
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Put_Integer
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Put_Structure
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Put_Value
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Put_Variable
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Set_Constant
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Set_Integer
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Set_Value
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Set_Variable
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Try_Me_Else
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Retry_Me_Else
     (Machine : in out Machete_Machine'Class);

   procedure Eval_Trust_Me
     (Machine : in out Machete_Machine'Class);

   procedure Meta_Assertz
     (Machine : in out Machete_Machine'Class);

   procedure Meta_Call
     (Machine : in out Machete_Machine'Class);

   type Array_Of_Instruction_Handlers is
     array (Machete.Instructions.Machete_Instruction) of Handler;

   Instruction_Handlers : constant Array_Of_Instruction_Handlers :=
                            (Allocate       => Eval_Allocate'Access,
                             Deallocate     => Eval_Deallocate'Access,
                             Call           => Eval_Call'Access,
                             Proceed        => Eval_Proceed'Access,
                             No_Operation   => Eval_No_Operation'Access,
                             Stop           => Eval_Stop'Access,
                             Try_Me_Else    => Eval_Try_Me_Else'Access,
                             Retry_Me_Else  => Eval_Retry_Me_Else'Access,
                             Trust_Me       => Eval_Trust_Me'Access,
                             Get_Constant   => Eval_Get_Constant'Access,
                             Get_Integer    => Eval_Get_Integer'Access,
                             Get_Structure  => Eval_Get_Structure'Access,
                             Get_Value      => Eval_Get_Value'Access,
                             Get_Variable   => Eval_Get_Variable'Access,
                             Unify_Constant => Eval_Unify_Constant'Access,
                             Unify_Integer  => Eval_Unify_Integer'Access,
                             Unify_Value    => Eval_Unify_Value'Access,
                             Unify_Variable => Eval_Unify_Variable'Access,
                             Put_Constant   => Eval_Put_Constant'Access,
                             Put_Integer    => Eval_Put_Integer'Access,
                             Put_Structure  => Eval_Put_Structure'Access,
                             Put_Value      => Eval_Put_Value'Access,
                             Put_Variable   => Eval_Put_Variable'Access,
                             Set_Constant   => Eval_Set_Constant'Access,
                             Set_Integer    => Eval_Set_Integer'Access,
                             Set_Value      => Eval_Set_Value'Access,
                             Set_Variable   => Eval_Set_Variable'Access,
                             others         => Bad_Instruction'Access);

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Machine     : in out Machete_Machine;
      Value       : Word_8)
   is
   begin
      Machine.Memory.Set
        (Machine.Code_Top, Value);
      Machine.Code_Top := Machine.Code_Top + 1;
   end Append;

   ---------------
   -- Backtrack --
   ---------------

   procedure Backtrack (Machine : in out Machete_Machine'Class) is
   begin
      if Machine.Rs (B) = Stack_Address'First then
         if Log_Execution then
            Machete.Logging.Log ("fail");
         end if;
         Machine.Fail := True;
      else
         if Log_Execution then
            Machete.Logging.Log
              (Machine.Rs (P), "backtrack");
         end if;

         Machine.Rs (P) :=
           Machine.Memory.Get_Address
             (Machine.B, Machine.Memory.Get_Address (Machine.B) + 4);
      end if;
   end Backtrack;

   ---------------------
   -- Bad_Instruction --
   ---------------------

   procedure Bad_Instruction
     (Machine : in out Machete_Machine'Class)
   is
   begin
      Machine.Stop := True;
   end Bad_Instruction;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Machine     : in out Machete_Machine'Class;
      Left, Right : Machete_Address)
   is
      use Machete.Cells;
      Left_Cell  : constant Machete.Cells.Cell_Type :=
                     Machine.Memory.Get_Cell (Left);
      Right_Cell : constant Machete.Cells.Cell_Type :=
                     Machine.Memory.Get_Cell (Right);
   begin
      if Is_Reference (Left_Cell)
        and then (not Is_Reference (Right_Cell)
                  or else Right < Left)
      then
         Machine.Memory.Set_Cell (Left, Right_Cell);
         Machine.Trail (Left);
      else
         Machine.Memory.Set_Cell (Right, Left_Cell);
         Machine.Trail (Right);
      end if;
   end Bind;

   ----------------
   -- Bind_Value --
   ----------------

   overriding procedure Bind_Value
     (Argument : in out Machine_Predicate_Argument;
      Value    : Machete.Terms.Machete_Term)
   is
      Address : constant Machete_Address :=
                  Argument.Machine.Rs (H);
   begin
      Argument.Machine.Memory.Write_Term
        (Argument.Machine.Rs (H), Value);
      Argument.Machine.Memory.Set_Cell
        (Argument.Address,
         Machete.Cells.To_Reference_Cell (Address));
   end Bind_Value;

   ------------
   -- Define --
   ------------

   procedure Define
     (Machine  : in out Machete_Machine'Class;
      Functor  : Machete_Functor)
   is
   begin
      Machine.Definitions.Insert
        (Functor,
         (Meta => No_Meta,
          Primitive => null,
          Address   => Machine.Code_Top));
   end Define;

   ------------
   -- Define --
   ------------

   procedure Define
     (Machine  : in out Machete_Machine'Class;
      Functor  : Machete_Functor;
      Handler  : Machete.Functors.Predicate_Handler)
   is
   begin
      Machine.Definitions.Insert
        (Functor,
         (Meta => No_Meta,
          Primitive => Handler,
          Address   => 0));
   end Define;

   -------------------
   -- Eval_Allocate --
   -------------------

   procedure Eval_Allocate
     (Machine : in out Machete_Machine'Class)
   is
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      R_E       : Machete_Address renames Machine.Rs (E);
      R_P   : Machete_Address renames Machine.Rs (P);
      New_E : constant Machete_Address := Machine.New_Frame;
      N     : constant Word_8 := Machine.Memory.Get (Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Allocate, Log (N), Log (R_E), Log (New_E));
      end if;

      R_P := R_P + 1;
      Machine.Memory.Set_Address (New_E, 0, R_E);
      Machine.Memory.Set_Address (New_E, 1, Machine.Rs (CP));
      Machine.Memory.Set_Address (New_E, 2, Machete_Address (N));
      R_E := New_E;
   end Eval_Allocate;

   ---------------
   -- Eval_Call --
   ---------------

   procedure Eval_Call
     (Machine : in out Machete_Machine'Class)
   is
      use Machete.Functors;
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Functor   : constant Machete_Functor :=
                    Machete.Code.Scan_Functor (Machine.Memory, Machine.Rs (P));
      Position  : constant Functor_Maps.Cursor :=
                    Machine.Definitions.Find (Functor);
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Call, Log (Functor));
      end if;

      if Functor_Maps.Has_Element (Position) then
         declare
            Definition : constant Functor_Record :=
                           Functor_Maps.Element (Position);
         begin
            case Definition.Meta is
               when Meta_Asserta =>
                  null;
               when Meta_Assertz =>
                  Machine.Meta_Assertz;
               when Meta_Atom =>
                  declare
                     Value : constant Machete.Cells.Cell_Type :=
                               Machine.Memory.Get_Cell
                                 (Machine.Get_Register_Address (1));
                  begin
                     if (not Machete.Cells.Is_Functor (Value)
                         or else Machete.Functors.Arity
                           (Machete.Cells.Get_Functor (Value)) /= 0)
                       and then not Machete.Cells.Is_Integer (Value)
                     then
                        Machine.Backtrack;
                     end if;
                  end;
               when Meta_Var =>
                  declare
                     Value : constant Machete.Cells.Cell_Type :=
                               Machine.Memory.Get_Cell
                                 (Machine.Memory.Dereference
                                    (Machine.Get_Register_Address (1)));
                  begin
                     if not Machete.Cells.Is_Reference (Value) then
                        Machine.Backtrack;
                     end if;
                  end;
               when Meta_Call =>
                  Machine.Meta_Call;

               when No_Meta =>
                  if Definition.Primitive /= null then
                     declare
                        Count        : constant Natural := Arity (Functor);
                        Args         : Predicate_Arguments (1 .. Count);
                        Machine_Args : array (1 .. Count)
                          of aliased Machine_Predicate_Argument;
                        Success      : Boolean := False;
                     begin
                        for I in Machine_Args'Range loop
                           Machine_Args (I) :=
                             (Machine'Unchecked_Access,
                              Machine.Memory.Dereference
                                (Machine.Get_Register_Address
                                     (Machete_Register (I))));
                           Args (I) := Machine_Args (I)'Unchecked_Access;
                        end loop;

                        Definition.Primitive (Args, Success);

                        if not Success then
                           Machine.Backtrack;
                        end if;
                     end;
                  elsif Definition.Address /= 0 then
                     Machine.Rs (ARGC) :=
                       Machete_Address (Machete.Functors.Arity (Functor));
                     Machine.Rs (CP) := Machine.Rs (P);
                     Machine.Rs (P) := Definition.Address;
                  else
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "warning: " & Machete.Functors.Image (Functor)
                        & " not defined");

                     Machine.Backtrack;
                  end if;
            end case;
         end;
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "warning: " & Machete.Functors.Image (Functor)
            & " not defined");

         Machine.Backtrack;
      end if;
   end Eval_Call;

   ---------------------
   -- Eval_Deallocate --
   ---------------------

   procedure Eval_Deallocate
     (Machine : in out Machete_Machine'Class)
   is
      PC    : constant Machete_Address := Machine.Rs (P) - 1;
      Old_E : constant Machete_Address := Machine.Rs (E);
   begin
      Machine.Rs (CP) := Machine.Memory.Get_Address (Machine.Rs (E), 1);
      Machine.Rs (E) := Machine.Memory.Get_Address (Machine.Rs (E));

      if Log_Execution then
         Log_Instruction
           (PC, Deallocate,
            Log (Machine.Rs (CP)), Log (Old_E), Log (Machine.Rs (E)));
      end if;

   end Eval_Deallocate;

   -----------------------
   -- Eval_Get_Constant --
   -----------------------

   procedure Eval_Get_Constant
     (Machine : in out Machete_Machine'Class)
   is
      use Machete.Cells;
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Con       : constant Machete_Functor :=
                    Machete.Code.Scan_Functor
                      (Machine.Memory, Machine.Rs (P));
      Address   : constant Machete_Address :=
                    Machine.Memory.Dereference
                      (Machine.Get_Register_Address (Register));
      Cell      : constant Machete.Cells.Cell_Type :=
                    Machine.Memory.Get_Cell (Address);
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Get_Constant, Log (Register), Log (Con));
      end if;

      if Machete.Cells.Is_Reference (Cell) then
         Machine.Memory.Set_Cell (Address, To_Functor_Cell (Con));
         Machine.Trail (Address);
      elsif Machete.Cells.Is_Functor (Cell) then
         if Get_Functor (Cell) /= Con then
            Machine.Backtrack;
         end if;
      elsif Machete.Cells.Is_Structure (Cell) then
         declare
            Functor_Address : constant Machete_Address :=
                                Machete.Cells.Get_Address (Cell);
            Functor_Cell    : constant Machete.Cells.Cell_Type :=
                                Machine.Memory.Get_Cell (Functor_Address);
            Functor         : constant Machete_Functor :=
                                Machete.Cells.Get_Functor (Functor_Cell);
         begin
            if Functor /= Con then
               Machine.Backtrack;
            end if;
         end;
      else
         Machine.Backtrack;
      end if;
   end Eval_Get_Constant;

   ----------------------
   -- Eval_Get_Integer --
   ----------------------

   procedure Eval_Get_Integer
     (Machine : in out Machete_Machine'Class)
   is
      use Machete.Cells;
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Value     : constant Integer :=
                    Machete.Code.Scan_Integer
                      (Machine.Memory, Machine.Rs (P));
      Address   : constant Machete_Address :=
                    Machine.Memory.Dereference
                      (Machine.Get_Register_Address (Register));
      Cell      : constant Machete.Cells.Cell_Type :=
                    Machine.Memory.Get_Cell (Address);
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Get_Integer, Log (Register), Log (Value));
      end if;

      if Machete.Cells.Is_Reference (Cell) then
         Machine.Memory.Set_Cell (Address, To_Integer_Cell (Value));
         Machine.Trail (Address);
      elsif Machete.Cells.Is_Integer (Cell) then
         if Get_Integer (Cell) /= Value then
            Machine.Backtrack;
         end if;
      else
         Machine.Backtrack;
      end if;
   end Eval_Get_Integer;

   ------------------------
   -- Eval_Get_Structure --
   ------------------------

   procedure Eval_Get_Structure
     (Machine : in out Machete_Machine'Class)
   is
      use Machete.Cells;
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Functor   : constant Machete_Functor :=
                    Machete.Code.Scan_Functor
                      (Machine.Memory, Machine.Rs (P));
      Address   : constant Machete_Address :=
                    Machine.Memory.Dereference
                      (Machine.Get_Register_Address (Register));
      Cell      : constant Machete.Cells.Cell_Type :=
                    Machine.Memory.Get_Cell (Address);
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Get_Structure, Log (Register), Log (Functor));
      end if;

      if Machete.Cells.Is_Reference (Cell) then
         Machine.Memory.Set_Cell (Machine.Rs (H) + Word_Size,
                                  Machete.Cells.To_Functor_Cell (Functor));
         Machine.Memory.Set_Cell (Machine.Rs (H),
                                  Machete.Cells.To_Structure_Cell
                                    (Machine.Rs (H) + Word_Size));
         Machine.Bind (Address, Machine.Rs (H));
         Machine.Rs (H) := Machine.Rs (H) + 2 * Word_Size;
         Machine.Mode := Write;
      elsif Machete.Cells.Is_Structure (Cell) then
         declare
            S_Addr : constant Machete_Address :=
                       Machete.Cells.Get_Address (Cell);
         begin
            if Machine.Memory.Get_Cell (S_Addr)
              = To_Functor_Cell (Functor)
            then
               Machine.Rs (S) := S_Addr + Word_Size;
               Machine.Mode := Read;
            else
               Machine.Backtrack;
            end if;
         end;
      else
         Machine.Backtrack;
      end if;
   end Eval_Get_Structure;

   --------------------
   -- Eval_Get_Value --
   --------------------

   procedure Eval_Get_Value
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Argument : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Fail     : Boolean;
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Get_Value, Log (Register), Log (Argument));
      end if;

      Machine.Unify
        (Machine.Get_Register_Address (Register),
         Machine.Get_Register_Address (Argument),
         Fail);
      if Fail then
         Machine.Backtrack;
      end if;

   end Eval_Get_Value;

   -----------------------
   -- Eval_Get_Variable --
   -----------------------

   procedure Eval_Get_Variable
     (Machine : in out Machete_Machine'Class)
   is
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Argument  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Get_Variable, Log (Register), Log (Argument));
      end if;

      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Register),
         Machine.Memory.Get_Cell (Machine.Get_Register_Address (Argument)));
   end Eval_Get_Variable;

   procedure Eval_No_Operation
     (Machine : in out Machete_Machine'Class)
   is null;

   ------------------
   -- Eval_Proceed --
   ------------------

   procedure Eval_Proceed
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
   begin
      if Log_Execution then
         Log_Instruction (PC, Proceed, Log (Machine.Rs (CP)));
      end if;

      Machine.Rs (P) := Machine.Rs (CP);
   end Eval_Proceed;

   -----------------------
   -- Eval_Put_Constant --
   -----------------------

   procedure Eval_Put_Constant
     (Machine : in out Machete_Machine'Class)
   is
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Functor   : constant Machete_Functor :=
                    Machete.Code.Scan_Functor
                      (Machine.Memory, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Put_Constant, Log (Register), Log (Functor));
      end if;

      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Register),
         Machete.Cells.To_Functor_Cell (Functor));
   end Eval_Put_Constant;

   ----------------------
   -- Eval_Put_Integer --
   ----------------------

   procedure Eval_Put_Integer
     (Machine : in out Machete_Machine'Class)
   is
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Value     : constant Integer :=
                    Machete.Code.Scan_Integer
                      (Machine.Memory, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Put_Constant, Log (Register), Log (Value));
      end if;

      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Register),
         Machete.Cells.To_Integer_Cell (Value));
   end Eval_Put_Integer;

   ------------------------
   -- Eval_Put_Structure --
   ------------------------

   procedure Eval_Put_Structure
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Functor  : constant Machete_Functor :=
                   Machete.Code.Scan_Functor (Machine.Memory, Machine.Rs (P));
      Structure : constant Machete.Cells.Cell_Type :=
                    Machete.Cells.To_Structure_Cell
                      (Machine.Rs (H) + Word_Size);
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Put_Structure, Log (Register), Log (Functor));
      end if;

      Machine.Memory.Set_Cell (Machine.Rs (H) + Word_Size,
                               Machete.Cells.To_Functor_Cell (Functor));
      Machine.Memory.Set_Cell (Machine.Rs (H), Structure);
      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Register), Structure);
      Machine.Rs (H) := Machine.Rs (H) + 2 * Word_Size;
   end Eval_Put_Structure;

   --------------------
   -- Eval_Put_Value --
   --------------------

   procedure Eval_Put_Value
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Argument : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Value    : constant Machete.Cells.Cell_Type :=
                   Machine.Memory.Get_Cell
                     (Machine.Get_Register_Address (Register));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Put_Value, Log (Register), Log (Argument));
      end if;

      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Argument), Value);
   end Eval_Put_Value;

   -----------------------
   -- Eval_Put_Variable --
   -----------------------

   procedure Eval_Put_Variable
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Argument  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Reference : constant Machete.Cells.Cell_Type :=
                    Machete.Cells.To_Reference_Cell (Machine.Rs (H));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Put_Variable, Log (Register), Log (Argument));
      end if;

      Machine.Memory.Set_Cell (Machine.Rs (H), Reference);
      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Register), Reference);
      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Argument), Reference);
      Machine.Rs (H) := Machine.Rs (H) + Word_Size;
   end Eval_Put_Variable;

   ------------------------
   -- Eval_Retry_Me_Else --
   ------------------------

   procedure Eval_Retry_Me_Else
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      function B return Machete_Address is (Machine.B);
      M : Machete.Memory.Machete_Memory renames Machine.Memory;
      N : constant Machete_Address := M.Get_Address (Machine.B);
      L     : constant Machete_Address :=
                Machete.Code.Scan_Address (M, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction (PC, Retry_Me_Else, Log (L));
      end if;

      for I in 1 .. N loop
         declare
            R : constant Machete_Register := Machete_Register (I);
         begin
            M.Set_Address (Machine.Get_Register_Address (R),
                           M.Get_Address (B, I));
         end;
      end loop;

      Machine.Rs (E) := M.Get_Address (B, N + 1);
      Machine.Rs (CP) := M.Get_Address (B, N + 2);
      M.Set_Address (B, N + 4, L);
      Machine.Unwind_Trail (M.Get_Address (B, N + 5));
      Machine.Rs (TR) := M.Get_Address (B, N + 5);
      Machine.Rs (H) := M.Get_Address (B, N + 6);
      Machine.Rs (HB) := Machine.Rs (H);
   end Eval_Retry_Me_Else;

   -----------------------
   -- Eval_Set_Constant --
   -----------------------

   procedure Eval_Set_Constant
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Value    : constant Machete_Functor :=
                   Machete.Code.Scan_Functor (Machine.Memory, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Set_Constant, Log (Value));
      end if;

      Machine.Memory.Set_Cell (Machine.Rs (H),
                               Machete.Cells.To_Functor_Cell (Value));
      Machine.Rs (H) := Machine.Rs (H) + Word_Size;
   end Eval_Set_Constant;

   ----------------------
   -- Eval_Set_Integer --
   ----------------------

   procedure Eval_Set_Integer
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Value    : constant Integer :=
                   Machete.Code.Scan_Integer (Machine.Memory, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Set_Integer, Log (Value));
      end if;

      Machine.Memory.Set_Cell (Machine.Rs (H),
                               Machete.Cells.To_Integer_Cell (Value));
      Machine.Rs (H) := Machine.Rs (H) + Word_Size;
   end Eval_Set_Integer;

   --------------------
   -- Eval_Set_Value --
   --------------------

   procedure Eval_Set_Value
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Value    : constant Machete.Cells.Cell_Type :=
                   Machine.Memory.Get_Cell
                     (Machine.Get_Register_Address (Register));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Set_Value, Log (Register));
      end if;

      Machine.Memory.Set_Cell (Machine.Rs (H), Value);
      Machine.Rs (H) := Machine.Rs (H) + Word_Size;
   end Eval_Set_Value;

   -----------------------
   -- Eval_Set_Variable --
   -----------------------

   procedure Eval_Set_Variable
     (Machine : in out Machete_Machine'Class)
   is
      PC        : constant Machete_Address := Machine.Rs (P) - 1;
      Register  : constant Machete_Register :=
                    Machete.Code.Scan_Register
                      (Machine.Memory, Machine.Rs (P));
      Reference : constant Machete.Cells.Cell_Type :=
                    Machete.Cells.To_Reference_Cell (Machine.Rs (H));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Set_Variable, Log (Register));
      end if;

      Machine.Memory.Set_Cell (Machine.Rs (H), Reference);
      Machine.Memory.Set_Cell
        (Machine.Get_Register_Address (Register), Reference);
      Machine.Rs (H) := Machine.Rs (H) + Word_Size;
   end Eval_Set_Variable;

   ---------------
   -- Eval_Stop --
   ---------------

   procedure Eval_Stop
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
   begin
      if Log_Execution then
         Log_Instruction (PC, Stop);
      end if;

      Machine.Stop := True;
   end Eval_Stop;

   -------------------
   -- Eval_Trust_Me --
   -------------------

   procedure Eval_Trust_Me
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      function B return Machete_Address is (Machine.B);
      M     : Machete.Memory.Machete_Memory renames Machine.Memory;
      N        : constant Machete_Address := M.Get_Address (B);
      L        : constant Machete_Address :=
                   Machete.Code.Scan_Address (M, Machine.Rs (P));
   begin
      pragma Assert (L = 0);
      if Log_Execution then
         Log_Instruction (PC, Trust_Me, Log (N));
      end if;

      for I in 1 .. N loop
         declare
            R : constant Machete_Register := Machete_Register (I);
         begin
            M.Set_Address (Machine.Get_Register_Address (R),
                           M.Get_Address (B, I));
         end;
      end loop;

      Machine.Rs (E) := M.Get_Address (B, N + 1);
      Machine.Rs (CP) := M.Get_Address (B, N + 2);
      Machine.Unwind_Trail (M.Get_Address (B, N + 5));
      Machine.Rs (TR) := M.Get_Address (B, N + 5);
      Machine.Rs (H) := M.Get_Address (B, N + 6);
      Machine.Rs (B) := M.Get_Address (B, N + 3);
      Machine.Rs (HB) := M.Get_Address (Machine.Rs (B), N + 6);
   end Eval_Trust_Me;

   ----------------------
   -- Eval_Try_Me_Else --
   ----------------------

   procedure Eval_Try_Me_Else
     (Machine : in out Machete_Machine'Class)
   is
      PC    : constant Machete_Address := Machine.Rs (P) - 1;
      M     : Machete.Memory.Machete_Memory renames Machine.Memory;
      New_B : constant Machete_Address := Machine.New_Frame;
      N     : constant Machete_Address := Machine.Rs (ARGC);
      L     : constant Machete_Address :=
                Machete.Code.Scan_Address (M, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction (PC, Try_Me_Else, Log (L) & "; args = " & Log (N));
      end if;

      M.Set_Address (New_B, 0, N);
      for I in 1 .. N loop
         declare
            R : constant Machete_Register := Machete_Register (I);
            X : constant Word_32 :=
                  M.Get (Machine.Get_Register_Address (R));
         begin
            M.Set (New_B, I, X);
         end;
      end loop;

      M.Set_Address (New_B, N + 1, Machine.Rs (E));
      M.Set_Address (New_B, N + 2, Machine.Rs (CP));
      M.Set_Address (New_B, N + 3, Machine.Rs (B));
      M.Set_Address (New_B, N + 4, L);
      M.Set_Address (New_B, N + 5, Machine.Rs (TR));
      M.Set_Address (New_B, N + 6, Machine.Rs (H));

      Machine.Rs (B) := New_B;
      Machine.Rs (HB) := Machine.Rs (H);

   end Eval_Try_Me_Else;

   -------------------------
   -- Eval_Unify_Constant --
   -------------------------

   procedure Eval_Unify_Constant
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Value    : constant Machete_Functor :=
                   Machete.Code.Scan_Functor (Machine.Memory,
                                            Machine.Rs (P));
      Fail     : Boolean;
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Unify_Constant, Log (Value));
      end if;

      case Machine.Mode is
         when Read =>
            declare
               use Machete.Cells;
               Address  : constant Machete_Address :=
                            Machine.Memory.Dereference (Machine.Rs (S));
               Cell     : constant Cell_Type :=
                            Machine.Memory.Get_Cell (Address);
            begin
               if Is_Reference (Cell) then
                  Machine.Memory.Set_Cell (Address, To_Functor_Cell (Value));
                  Machine.Trail (Address);
               elsif Is_Functor (Cell) then
                  Fail := Get_Functor (Cell) /= Value;
               elsif Is_Structure (Cell) then
                  declare
                     Functor_Address : constant Machete_Address :=
                                         Get_Address (Cell);
                     Functor_Cell    : constant Cell_Type :=
                                         Machine.Memory.Get_Cell
                                           (Functor_Address);
                     Functor         : constant Machete_Functor :=
                                         Get_Functor (Functor_Cell);
                  begin
                     if Functor /= Value then
                        Machine.Backtrack;
                     end if;
                  end;

               else
                  Fail := True;
               end if;
            end;

            if Fail then
               Machine.Backtrack;
            end if;

         when Write =>
            Machine.Memory.Set_Cell
              (Machine.Rs (H), Machete.Cells.To_Functor_Cell (Value));
            Machine.Rs (H) := Machine.Rs (H) + Word_Size;
      end case;
   end Eval_Unify_Constant;

   ------------------------
   -- Eval_Unify_Integer --
   ------------------------

   procedure Eval_Unify_Integer
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Value    : constant Integer :=
                   Machete.Code.Scan_Integer (Machine.Memory,
                                            Machine.Rs (P));
      Fail     : Boolean;
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Unify_Integer, Log (Value));
      end if;

      case Machine.Mode is
         when Read =>
            declare
               use Machete.Cells;
               Address  : constant Machete_Address :=
                            Machine.Memory.Dereference (Machine.Rs (S));
               Cell     : constant Cell_Type :=
                            Machine.Memory.Get_Cell (Address);
            begin
               if Is_Reference (Cell) then
                  Machine.Memory.Set_Cell (Address, To_Integer_Cell (Value));
                  Machine.Trail (Address);
               elsif Is_Integer (Cell) then
                  Fail := Get_Integer (Cell) /= Value;
               else
                  Fail := True;
               end if;
            end;

            if Fail then
               Machine.Backtrack;
            end if;

         when Write =>
            Machine.Memory.Set_Cell
              (Machine.Rs (H), Machete.Cells.To_Integer_Cell (Value));
            Machine.Rs (H) := Machine.Rs (H) + Word_Size;
      end case;
   end Eval_Unify_Integer;

   ----------------------
   -- Eval_Unify_Value --
   ----------------------

   procedure Eval_Unify_Value
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
      Fail     : Boolean;
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Unify_Value, Log (Register));
      end if;

      case Machine.Mode is
         when Read =>
            Machine.Unify
              (Machine.Get_Register_Address (Register),
               Machine.Rs (S),
               Fail);
            if Fail then
               Machine.Backtrack;
            end if;
         when Write =>
            Machine.Memory.Set_Cell
              (Machine.Rs (H),
               Machine.Memory.Get_Cell
                 (Machine.Get_Register_Address (Register)));
            Machine.Rs (H) := Machine.Rs (H) + Word_Size;
      end case;
      Machine.Rs (S) := Machine.Rs (S) + Word_Size;
   end Eval_Unify_Value;

   -------------------------
   -- Eval_Unify_Variable --
   -------------------------

   procedure Eval_Unify_Variable
     (Machine : in out Machete_Machine'Class)
   is
      PC       : constant Machete_Address := Machine.Rs (P) - 1;
      Register : constant Machete_Register :=
                   Machete.Code.Scan_Register (Machine.Memory, Machine.Rs (P));
   begin
      if Log_Execution then
         Log_Instruction
           (PC, Unify_Variable, Log (Register));
      end if;

      case Machine.Mode is
         when Read =>
            Machine.Memory.Set_Cell
              (Machine.Get_Register_Address (Register),
               Machine.Memory.Get_Cell (Machine.Rs (S)));
         when Write =>
            Machine.Memory.Set_Cell
              (Machine.Rs (H),
               Machete.Cells.To_Reference_Cell (Machine.Rs (H)));
            Machine.Memory.Set_Cell
              (Machine.Get_Register_Address (Register),
               Machete.Cells.To_Reference_Cell (Machine.Rs (H)));
            Machine.Rs (H) := Machine.Rs (H) + Word_Size;
      end case;
      Machine.Rs (S) := Machine.Rs (S) + Word_Size;
   end Eval_Unify_Variable;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Machine  : in out Machete_Machine'Class;
      Start    : Machete_Address;
      Continue : not null access function return Boolean;
      Success  : out Boolean)
   is
   begin
      Machine.Rs := (P      => Start,
                     H      => Heap_Address'First,
                     E      => Stack_Address'First,
                     B      => Stack_Address'First,
                     TR     => Trail_Address'First,
                     others => 0);
      Machine.Fail := False;
      Machine.Stop := False;

      Machine.Memory.Set_Address (Machine.Rs (E), 0);
      Machine.Memory.Set_Address (Machine.Rs (E) + Word_Size, 0);
      Machine.Memory.Set_Address (Machine.Rs (E) + 2 * Word_Size, 0);

      while not Machine.Stop and then not Machine.Fail loop
         declare
            Code : constant Word_8 := Machine.Memory.Get (Machine.Rs (P));
         begin
            Machine.Rs (P) := Machine.Rs (P) + 1;
            exit when Code = 0;

            declare
               Instruction : constant Machete_Instruction :=
                               Machete_Instruction'Val (Code);
            begin
               Instruction_Handlers (Instruction) (Machine);
            end;
         end;
         if Machine.Stop
           and then not Machine.Fail
           and then Continue.all
         then
            Machine.Stop := False;
            Machine.Backtrack;
         end if;

      end loop;

      Success := not Machine.Fail;

   end Execute;

   -----------------------
   -- Get_From_Register --
   -----------------------

   function Get_From_Register
     (Machine  : Machete_Machine'Class;
      Register : Machete_Register)
      return Machete_Address
   is
   begin
      return Machete.Cells.Get_Address
        (Machine.Memory.Get_Cell
           (Machine.Get_Register_Address (Register)));
   end Get_From_Register;

   --------------------------
   -- Get_Register_Address --
   --------------------------

   function Get_Register_Address
     (Machine  : Machete_Machine'Class;
      Register : Machete_Register)
      return Machete_Address
   is
   begin
      if Register in General_Purpose_Register then
         return Machete_Address (Register) * Word_Size;
      else
         declare
            Address : constant Machete_Address :=
                        Machine.Rs (E)
                        + 3 * Word_Size
                        + Machete_Address
                          (Register - Permanent_Register'First) * Word_Size;
         begin
            if False then
               Machete.Logging.Log
                 (Machete.Code.Register_Image (Register)
                  & " = "
                  & Machete.Images.Address_Image (Address)
                  & " ["
                  & Machete.Images.Address_Image
                    (Machine.Memory.Get_Address (Address))
                  & "]");
            end if;
            return Address;
         end;
      end if;
   end Get_Register_Address;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Machine : in out Machete_Machine)
   is
   begin
      Machine.Definitions.Insert
        (Machete.Functors.Get_Functor ("atom", 1),
         (Meta => Meta_Atom, others => <>));
      Machine.Definitions.Insert
        (Machete.Functors.Get_Functor ("asserta", 1),
         (Meta => Meta_Asserta, others => <>));
      Machine.Definitions.Insert
        (Machete.Functors.Get_Functor ("assertz", 1),
         (Meta => Meta_Assertz, others => <>));
      Machine.Definitions.Insert
        (Machete.Functors.Get_Functor ("assert", 1),
         (Meta => Meta_Assertz, others => <>));
      Machine.Definitions.Insert
        (Machete.Functors.Get_Functor ("call", 1),
         (Meta => Meta_Call, others => <>));
      Machine.Definitions.Insert
        (Machete.Functors.Get_Functor ("var", 1),
         (Meta => Meta_Var, others => <>));
   end Initialize;

   --------------
   -- Is_Bound --
   --------------

   overriding function Is_Bound
     (Argument : Machine_Predicate_Argument)
      return Boolean
   is
      use Machete.Cells;
      Cell : constant Cell_Type :=
               Argument.Machine.Memory.Get_Cell (Argument.Address);
   begin
      return not Is_Reference (Cell)
        or else Get_Address (Cell) /= Argument.Address;
   end Is_Bound;

   --------------
   -- Log_Code --
   --------------

   overriding procedure Log_Code
     (Machine : Machete_Machine;
      Message : String)
   is
   begin
      Machete.Logging.Log
        (Machete.Images.Image (Machete_Word (Machine.Code_Top), 4)
         & ": "
         & Message);
   end Log_Code;

   ---------------------
   -- Log_Instruction --
   ---------------------

   procedure Log_Instruction
     (Address     : Machete_Address;
      Instruction : Machete.Instructions.Machete_Instruction;
      Operand_1   : String := "";
      Operand_2   : String := "";
      Operand_3   : String := "")
   is
      use Ada.Characters.Handling;
   begin
      Machete.Logging.Log
        (Machete.Images.Image (Machete_Word (Address), 4)
         & ": "
         & Log (Instruction)
         & (if Operand_1 = "" then ""
           else " " & Operand_1
           & (if Operand_2 = "" then ""
             else "," & Operand_2
             & (if Operand_3 = "" then ""
               else "," & Operand_3))));
   end Log_Instruction;

   ------------
   -- Memory --
   ------------

   function Memory
     (Machine : in out Machete_Machine'Class)
      return access Machete.Memory.Machete_Memory'Class
   is
   begin
      return Machine.Memory'Unchecked_Access;
   end Memory;

   ------------------
   -- Meta_Assertz --
   ------------------

   procedure Meta_Assertz
     (Machine : in out Machete_Machine'Class)
   is
      Address : constant Machete_Address :=
                  Machine.Memory.Dereference
                    (Machine.Get_Register_Address (1));
      Cell    : constant Machete.Cells.Cell_Type :=
                  Machine.Memory.Get_Cell (Address);
   begin
      if Machete.Cells.Is_Reference (Cell) then
         Machine.Backtrack;
      else
         declare
            Term     : constant Machete.Terms.Machete_Term :=
                         Machine.Memory.Read_Term (Address);
            Functor  : constant Machete_Functor :=
                         Machete.Terms.Functor (Term);
            Position  : constant Functor_Maps.Cursor :=
                          Machine.Definitions.Find (Functor);
         begin
            if Log_Execution then
               Machete.Logging.Log
                 ("assertz: " & Machete.Terms.Images.Image (Term));
            end if;

            if Functor_Maps.Has_Element (Position) then
               declare
                  use type Machete.Functors.Predicate_Handler;
                  Definition : constant Functor_Record :=
                                 Functor_Maps.Element (Position);
               begin
                  if Definition.Meta /= No_Meta then
                     raise Constraint_Error with
                       "redefinition of built-in not allowed";
                  elsif Definition.Primitive /= null then
                     raise Constraint_Error with
                       "redefinition of foreign function not allowed";
                  end if;

                  if Log_Execution then
                     Machete.Logging.Log
                       ("added rule to "
                        & Machete.Functors.Image (Functor));
                  end if;

                  declare
                     Address    : Machete_Address := Definition.Address;
                     Op         : Word_8;
                     Instruction : Machete_Instruction;
                  begin
                     loop
                        Op          := Machine.Memory.Get (Address);
                        Instruction := Machete_Instruction'Val (Op);
                        Log_Instruction (Address, Instruction);

                        exit when Instruction /= Try_Me_Else
                          and then Instruction /= Retry_Me_Else;

                        Address := Machine.Memory.Get_Address (Address + 1);

                     end loop;

                     if Instruction = Trust_Me then
                        if Log_Execution then
                           Machete.Logging.Log
                             ("updating to retry_me_else");
                        end if;
                        Machine.Memory.Set
                          (Address,
                           Word_8'(Machete_Instruction'Pos (Retry_Me_Else)));
                     elsif Instruction = No_Operation then
                        if Log_Execution then
                           Machete.Logging.Log
                             ("updating to try_me_else");
                        end if;
                        Machine.Memory.Set
                          (Address,
                           Word_8'(Machete_Instruction'Pos (Try_Me_Else)));
                     else
                        raise Program_Error with
                          "unexpected instruction: " & Op'Img;
                     end if;

                     Machine.Trust_Me (Address + 1);
                     Machete.Compiler.Compile_Patch_Term
                       (Term, Machine);
                     Machine.Proceed;

                  end;

               end;
            else
               declare
                  Context : Machete.Compiler.Compile_Context;
               begin
                  Machete.Compiler.New_Context (Context);
                  Machete.Compiler.Add_Term (Context, Term, Machine);
                  Machete.Compiler.End_Context (Context, Machine);
               end;
            end if;
         end;
      end if;
   end Meta_Assertz;

   ---------------
   -- Meta_Call --
   ---------------

   procedure Meta_Call
     (Machine : in out Machete_Machine'Class)
   is
      Address : constant Machete_Address :=
                  Machine.Memory.Dereference
                    (Machine.Get_Register_Address (1));
      Cell    : constant Machete.Cells.Cell_Type :=
                  Machine.Memory.Get_Cell (Address);
   begin
      if Machete.Cells.Is_Reference (Cell) then
         if Log_Execution then
            Machete.Logging.Log
              ("call: cannot call " & Machete.Cells.Image (Cell));
         end if;
         Machine.Backtrack;
      else
         declare
            Term     : constant Machete.Terms.Machete_Term :=
                         Machine.Memory.Read_Term (Address);
            Bindings : Machete.Compiler.Query_Binding;
            Address  : constant Machete_Address :=
                         Machine.Code_Top;
         begin
            if Log_Execution then
               Machete.Logging.Log
                 ("calling: " & Machete.Terms.Images.Image (Term));
            end if;

            Machete.Compiler.Compile_Query
              (Term, Machine, Bindings);
            Machine.Deallocate;
            Machine.Rs (ARGC) :=
              Machete_Address (Machete.Terms.Arity (Term));
            Machine.Rs (CP) := Machine.Rs (P);
            Machine.Rs (P) := Address;
         end;

      end if;
   end Meta_Call;

   ---------------
   -- Scan_Cell --
   ---------------

   function Scan_Cell
     (Machine : in out Machete_Machine'Class)
      return Machete.Cells.Cell_Type
   is
   begin
      return Cell : constant Machete.Cells.Cell_Type :=
        Machine.Memory.Get_Cell (Machine.Rs (P))
      do
         Machine.Rs (P) := Machine.Rs (P) + Word_Size;
      end return;
   end Scan_Cell;

   -----------
   -- Trail --
   -----------

   procedure Trail
     (Machine : in out Machete_Machine'Class;
      Address : Machete_Address)
   is
   begin
      if Address < Machine.Rs (HB) or else
        (Machine.Rs (H) < Address and then Address < Machine.Rs (B))
      then
         Machine.Memory.Set_Address (Machine.Rs (TR), Address);
         Machine.Rs (TR) := Machine.Rs (TR) + Word_Size;
      end if;
   end Trail;

   -----------
   -- Unify --
   -----------

   procedure Unify
     (Machine     : in out Machete_Machine'Class;
      Left, Right : Machete_Address;
      Fail        : out Boolean)
   is
      Memory : Machete.Memory.Machete_Memory renames Machine.Memory;
      Stack : Machete.Address_Stacks.Address_Stack;
   begin
      Stack.Push (Left);
      Stack.Push (Right);
      Fail := False;

      while not (Stack.Is_Empty or else Fail) loop
         declare
            A1 : constant Machete_Address := Stack.Pop;
            A2 : constant Machete_Address := Stack.Pop;
            D1 : constant Machete_Address :=
                   Memory.Dereference (A1);
            D2 : constant Machete_Address :=
                   Memory.Dereference (A2);
         begin
            if D1 /= D2 then
               declare
                  use Machete.Cells;
                  Cell_1 : constant Cell_Type :=
                             Memory.Get_Cell (D1);
                  Cell_2 : constant Cell_Type :=
                             Memory.Get_Cell (D2);
                  pragma Assert (Is_Reference (Cell_1)
                                 or else Is_Structure (Cell_1)
                                 or else Is_Integer (Cell_1)
                                 or else (Is_Functor (Cell_1)
                                   and then Machete.Functors.Arity
                                     (Get_Functor (Cell_1))
                                   = 0));
                  pragma Assert (Is_Reference (Cell_2)
                                 or else Is_Structure (Cell_2)
                                 or else Is_Integer (Cell_2)
                                 or else (Is_Functor (Cell_2)
                                   and then Machete.Functors.Arity
                                     (Get_Functor (Cell_2))
                                   = 0));
               begin
                  if Is_Reference (Cell_1) then
                     Machine.Bind (D1, D2);
                  else
                     if Is_Reference (Cell_2) then
                        Machine.Bind (D1, D2);
                     elsif Is_Functor (Cell_2) then
                        Fail := Cell_1 /= Cell_2;
                     elsif Is_Integer (Cell_2) then
                        Fail := Cell_1 /= Cell_2;
                     elsif Is_Structure (Cell_2) then
                        if not Is_Structure (Cell_1) then
                           Fail := True;
                        else
                           declare
                              V_1    : constant Machete_Address :=
                                         Get_Address (Cell_1);
                              V_2    : constant Machete_Address :=
                                         Get_Address (Cell_2);
                              F_1    : constant Machete_Functor :=
                                      Get_Functor
                                        (Memory.Get_Cell (V_1));
                              F_2 : constant Machete_Functor :=
                                      Get_Functor
                                        (Memory.Get_Cell (V_2));
                           begin
                              if F_1 = F_2 then
                                 for I in 1 .. Functors.Arity (F_1) loop
                                    Stack.Push
                                      (V_1 + Machete_Address (I * Word_Size));
                                    Stack.Push
                                      (V_2 + Machete_Address (I * Word_Size));
                                 end loop;
                              else
                                 Fail := True;
                              end if;
                           end;
                        end if;
                     else
                        raise Constraint_Error with
                          "unhandled cell: " & Image (Cell_2);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Unify;

   ------------------
   -- Unwind_Trail --
   ------------------

   procedure Unwind_Trail (Machine  : in out Machete_Machine'Class;
                           Saved_TR : Machete_Address)
   is
      Trail : Machete_Address := Saved_TR;
   begin
      if Log_Execution then
         Machete.Logging.Log
           ("unwind-trail: "
            &  Machete.Images.Address_Image (Saved_TR)
            & " .. "
            & Machete.Images.Address_Image (Machine.Rs (TR)));
      end if;

      while Trail < Machine.Rs (TR) loop
         declare
            Address : constant Machete_Address :=
                        Machine.Memory.Get_Address (Trail);
         begin
            Machine.Memory.Set_Cell
              (Address,
               Machete.Cells.To_Reference_Cell (Address));
            Trail := Trail + Word_Size;
         end;
      end loop;
   end Unwind_Trail;

   --------------------
   -- Update_Pointer --
   --------------------

   overriding procedure Update_Pointer
     (Machine : in out Machete_Machine;
      Address : Machete_Address;
      Value   : Machete_Address)
   is
   begin
      Machine.Memory.Set_Address (Address, Value);
   end Update_Pointer;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Argument : Machine_Predicate_Argument)
      return Machete.Terms.Machete_Term
   is
   begin
      return Argument.Machine.Memory.Read_Term (Argument.Address);
   end Value;

end Machete.Machine;
