with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Machete.Cells;
with Machete.Functors;
with Machete.Images;

package body Machete.Code is

   function Register_Image
     (Register  : Machete_Register;
      Base_Name : Character := 'X')
      return String
   is ((if Register in Permanent_Register
        then "Y" & Ada.Strings.Fixed.Trim
          (Machete_Register'Image
             (Register - Permanent_Register'First + 1),
           Ada.Strings.Left)
        else Base_Name & Ada.Strings.Fixed.Trim
          (Machete_Register'Image (Register), Ada.Strings.Left)));

   type Operand_Type is (No_Operand,
                         Integer_Operand, Functor_Operand, Register_Operand,
                         Address_Operand, Cell_Operand, Word_8_Operand);

   type Operand (Op : Operand_Type := No_Operand) is
      record
         case Op is
            when No_Operand =>
               null;
            when Integer_Operand =>
               Integer_Value : Integer;
            when Functor_Operand =>
               Functor_Value : Machete_Functor;
            when Register_Operand =>
               Register_Value : Machete_Register;
            when Address_Operand =>
               Address        : Machete_Address;
            when Cell_Operand =>
               Cell        : Machete.Cells.Cell_Type;
            when Word_8_Operand =>
               Word_8_Value   : Word_8;
         end case;
      end record;

   function Image (Op : Operand) return String;

   function Op (Value : Integer) return Operand
   is (Integer_Operand, Value);

   function Op (Value : Machete_Functor) return Operand
   is (Functor_Operand, Value);

   function Op (Value : Machete_Register) return Operand
   is (Register_Operand, Value);

   function Op (Value : Machete_Address) return Operand
   is (Address_Operand, Value)
   with Unreferenced;

   function Op (Value : Machete.Cells.Cell_Type) return Operand
   is (Cell_Operand, Value)
   with Unreferenced;

   function Op (Value : Word_8) return Operand
   is (Word_8_Operand, Value);

   procedure Instruction
     (Code        : in out Code_Interface'Class;
      Instruction : Machete.Instructions.Machete_Instruction;
      Op_1        : Operand;
      Op_2, Op_3  : Operand := (Op => No_Operand));

   procedure Append
     (Code : in out Code_Interface'Class;
      Op   : Operand);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Code  : in out Code_Interface'Class;
      Size  : Natural)
   is
   begin
      Code.Instruction (Machete.Instructions.Allocate,
                        Op (Word_8 (Size)));
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append
     (Code : in out Code_Interface'Class;
      Op   : Operand)
   is
   begin
      case Op.Op is
         when No_Operand =>
            null;
         when Integer_Operand =>
            if Op.Integer_Value in 0 .. 253 then
               Code.Append (Word_8 (Op.Integer_Value));
            else
               declare
                  It : Natural := abs Op.Integer_Value;
               begin
                  if Op.Integer_Value < 0 then
                     Code.Append (254);
                  else
                     Code.Append (255);
                  end if;
                  for I in 1 .. Word_Size loop
                     Code.Append (Word_8 (It mod 256));
                     It := It / 256;
                  end loop;
               end;
            end if;

         when Functor_Operand =>
            Code.Append (Word_8 (Op.Functor_Value mod 256));
            Code.Append (Word_8 (Op.Functor_Value / 256));

         when Register_Operand =>
            Code.Append (Word_8 (Op.Register_Value));

         when Address_Operand =>
            declare
               It : Machete_Address := Op.Address;
            begin
               for I in 1 .. Word_Size loop
                  Code.Append (Word_8 (It mod 256));
                  It := It / 256;
               end loop;
            end;

         when Cell_Operand =>
            declare
               It : Machete_Word := Machete.Cells.To_Word (Op.Cell);
            begin
               for I in 1 .. Word_Size loop
                  Code.Append (Word_8 (It mod 256));
                  It := It / 256;
               end loop;
            end;

         when Word_8_Operand =>
            Code.Append (Op.Word_8_Value);

      end case;
   end Append;

   ----------
   -- Call --
   ----------

   procedure Call
     (Code     : in out Code_Interface'Class;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Call, Functor);
   end Call;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Code     : in out Code_Interface'Class)
   is
   begin
      Code.Instruction (Machete.Instructions.Deallocate);
   end Deallocate;

   ------------------
   -- Get_Constant --
   ------------------

   procedure Get_Constant
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Get_Constant, Register, Functor);
   end Get_Constant;

   -----------------
   -- Get_Integer --
   -----------------

   procedure Get_Integer
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Value    : Integer)
   is
   begin
      Code.Instruction (Machete.Instructions.Get_Integer,
                        Op (Register),
                        Op (Value));
   end Get_Integer;

   -------------------
   -- Get_Structure --
   -------------------

   procedure Get_Structure
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Get_Structure,
                        Register, Functor);
   end Get_Structure;

   ---------------
   -- Get_Value --
   ---------------

   procedure Get_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Get_Value, Register, Argument);
   end Get_Value;

   ------------------
   -- Get_Variable --
   ------------------

   procedure Get_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Get_Variable, Register, Argument);
   end Get_Variable;

   -----------
   -- Image --
   -----------

   function Image (Op : Operand) return String is
   begin
      case Op.Op is
         when No_Operand =>
            return "";
         when Integer_Operand =>
            return Ada.Strings.Fixed.Trim
              (Integer'Image (Op.Integer_Value), Ada.Strings.Left);
         when Functor_Operand =>
            return Machete.Functors.Image (Op.Functor_Value);
         when Register_Operand =>
            return Register_Image (Op.Register_Value);
         when Address_Operand =>
            return Machete.Images.Address_Image (Op.Address);
         when Cell_Operand =>
            return Machete.Cells.Image (Op.Cell);
         when Word_8_Operand =>
            return Ada.Strings.Fixed.Trim
              (Word_8'Image (Op.Word_8_Value), Ada.Strings.Left);
      end case;
   end Image;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Code        : in out Code_Interface'Class;
      Instruction : Machete.Instructions.Machete_Instruction;
      Op_1        : Operand;
      Op_2, Op_3  : Operand := (Op => No_Operand))
   is
      Message : constant String :=
                  Ada.Characters.Handling.To_Lower
                    (Machete.Instructions.Machete_Instruction'Image
                       (Instruction))
                  & (if Op_1.Op /= No_Operand
                     then " " & Image (Op_1)
                     & (if Op_2.Op /= No_Operand
                       then "," & Image (Op_2)
                       & (if Op_3.Op /= No_Operand
                         then "," & Image (Op_3)
                         else "")
                       else "")
                     else "");

   begin
      Code.Log_Code (Message);
      Code.Append
        (Word_8'(Machete.Instructions.Machete_Instruction'Pos (Instruction)));
      Code.Append (Op_1);
      Code.Append (Op_2);
      Code.Append (Op_3);
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Code  : in out Code_Interface'Class;
      Instr : Machete.Instructions.Machete_Instruction)
   is
   begin
      Code.Instruction (Instr, (Op => No_Operand));
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Code             : in out Code_Interface'Class;
      Instruction      : Machete.Instructions.Machete_Instruction;
      Functor          : Machete_Functor)
   is
   begin
      Code.Instruction (Instruction, Op (Functor));
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Code             : in out Code_Interface'Class;
      Instruction      : Machete.Instructions.Machete_Instruction;
      Register         : Machete_Register)
   is
   begin
      Code.Instruction (Instruction, Op (Register));
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Code        : in out Code_Interface'Class;
      Instruction : Machete.Instructions.Machete_Instruction;
      Register    : Machete_Register;
      Functor     : Machete_Functor)
   is
   begin
      Code.Instruction (Instruction, Op (Register), Op (Functor));
   end Instruction;

   -----------------
   -- Instruction --
   -----------------

   procedure Instruction
     (Code        : in out Code_Interface'Class;
      Instruction : Machete.Instructions.Machete_Instruction;
      Register_1  : Machete_Register;
      Register_2  : Machete_Register)
   is
   begin
      Code.Instruction (Instruction, Op (Register_1), Op (Register_2));
   end Instruction;

   ------------------
   -- No_Operation --
   ------------------

   procedure No_Operation
     (Code     : in out Code_Interface'Class)
   is
   begin
      Code.Instruction (Machete.Instructions.No_Operation);
   end No_Operation;

   -------------
   -- Proceed --
   -------------

   procedure Proceed
     (Code     : in out Code_Interface'Class)
   is
   begin
      Code.Instruction (Machete.Instructions.Proceed);
   end Proceed;

   ------------------
   -- Put_Constant --
   ------------------

   procedure Put_Constant
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Put_Constant, Register, Functor);
   end Put_Constant;

   -----------------
   -- Put_Integer --
   -----------------

   procedure Put_Integer
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Value    : Integer)
   is
   begin
      Code.Instruction (Machete.Instructions.Put_Integer,
                        Op (Register), Op (Value));
   end Put_Integer;

   -------------------
   -- Put_Structure --
   -------------------

   procedure Put_Structure
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Put_Structure,
                        Register, Functor);
   end Put_Structure;

   ---------------
   -- Put_Value --
   ---------------

   procedure Put_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Put_Value,
                        Register, Argument);
   end Put_Value;

   ------------------
   -- Put_Variable --
   ------------------

   procedure Put_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Put_Variable,
                        Register, Argument);
   end Put_Variable;

   -------------------
   -- Retry_Me_Else --
   -------------------

   procedure Retry_Me_Else
     (Code     : in out Code_Interface'Class;
      Label    : in out Machete_Address)
   is
   begin
      Code.Update_Pointer (Label, Code.Current_Code_Address);
      Code.Instruction (Machete.Instructions.Retry_Me_Else);
      Label := Code.Current_Code_Address;
      for I in 1 .. Word_Size loop
         Code.Append (0);
      end loop;
   end Retry_Me_Else;

   ------------------
   -- Scan_Address --
   ------------------

   function Scan_Address
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Machete_Address
   is
      Address : constant Machete_Address := Memory.Get_Address (PC);
   begin
      PC := PC + Word_Size;
      return Address;
   end Scan_Address;

   ------------------
   -- Scan_Functor --
   ------------------

   function Scan_Functor
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Machete_Functor
   is
      X : constant Word_16 := Memory.Get (PC);
   begin
      PC := PC + 2;
      return Machete_Functor (X);
   end Scan_Functor;

   ------------------
   -- Scan_Integer --
   ------------------

   function Scan_Integer
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Integer
   is
      X : constant Word_8 := Memory.Get (PC);
   begin
      PC := PC + 1;
      if X < 254 then
         return Integer (X);
      else
         declare
            It : Word_32 := 0;
            Y  : Word_8;
         begin
            for I in 1 .. 4 loop
               Y := Memory.Get (PC);
               PC := PC + 1;
               It := It * 256 + Word_32 (Y);
            end loop;
            if X = 254 then
               return Integer (It);
            else
               return -Integer (It);
            end if;
         end;
      end if;
   end Scan_Integer;

   -------------------
   -- Scan_Register --
   -------------------

   function Scan_Register
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Machete_Register
   is
      X : constant Word_8 := Memory.Get (PC);
   begin
      PC := PC + 1;
      return Machete_Register (X);
   end Scan_Register;

   ------------------
   -- Set_Constant --
   ------------------

   procedure Set_Constant
     (Code     : in out Code_Interface'Class;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Set_Constant, Functor);
   end Set_Constant;

   -----------------
   -- Set_Integer --
   -----------------

   procedure Set_Integer
     (Code     : in out Code_Interface'Class;
      Value    : Integer)
   is
   begin
      Code.Instruction (Machete.Instructions.Set_Integer,
                        Op (Value));
   end Set_Integer;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Set_Value, Register);
   end Set_Value;

   ------------------
   -- Set_Variable --
   ------------------

   procedure Set_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Set_Variable, Register);
   end Set_Variable;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Code     : in out Code_Interface'Class)
   is
   begin
      Code.Instruction (Machete.Instructions.Stop);
   end Stop;

   --------------
   -- Trust_Me --
   --------------

   procedure Trust_Me
     (Code     : in out Code_Interface'Class;
      Label    : Machete_Address)
   is
   begin
      Code.Update_Pointer (Label, Code.Current_Code_Address);
      Code.Instruction (Machete.Instructions.Trust_Me);
      for I in 1 .. Word_Size loop
         Code.Append (0);
      end loop;
   end Trust_Me;

   -----------------
   -- Try_Me_Else --
   -----------------

   procedure Try_Me_Else
     (Code     : in out Code_Interface'Class;
      Label    :    out Machete_Address)
   is
   begin
      Code.Instruction (Machete.Instructions.Try_Me_Else);
      Label := Code.Current_Code_Address;
      for I in 1 .. Word_Size loop
         Code.Append (0);
      end loop;
   end Try_Me_Else;

   --------------------
   -- Unify_Constant --
   --------------------

   procedure Unify_Constant
     (Code     : in out Code_Interface'Class;
      Functor  : Machete_Functor)
   is
   begin
      Code.Instruction (Machete.Instructions.Unify_Constant, Functor);
   end Unify_Constant;

   -------------------
   -- Unify_Integer --
   -------------------

   procedure Unify_Integer
     (Code  : in out Code_Interface'Class;
      Value : Integer)
   is
   begin
      Code.Instruction (Machete.Instructions.Unify_Integer,
                        Op (Value));
   end Unify_Integer;

   -----------------
   -- Unify_Value --
   -----------------

   procedure Unify_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Unify_Value, Register);
   end Unify_Value;

   --------------------
   -- Unify_Variable --
   --------------------

   procedure Unify_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register)
   is
   begin
      Code.Instruction (Machete.Instructions.Unify_Variable, Register);
   end Unify_Variable;

end Machete.Code;
