with Machete.Instructions;
with Machete.Memory;

package Machete.Code is

   type Code_Interface is limited interface;

   procedure Instruction
     (Code  : in out Code_Interface'Class;
      Instr : Machete.Instructions.Machete_Instruction);

   procedure Instruction
     (Code             : in out Code_Interface'Class;
      Instruction      : Machete.Instructions.Machete_Instruction;
      Functor          : Machete_Functor);

   procedure Instruction
     (Code             : in out Code_Interface'Class;
      Instruction      : Machete.Instructions.Machete_Instruction;
      Register         : Machete_Register);

   procedure Instruction
     (Code        : in out Code_Interface'Class;
      Instruction : Machete.Instructions.Machete_Instruction;
      Register    : Machete_Register;
      Functor     : Machete_Functor);

   procedure Instruction
     (Code        : in out Code_Interface'Class;
      Instruction : Machete.Instructions.Machete_Instruction;
      Register_1  : Machete_Register;
      Register_2  : Machete_Register);

   function Current_Code_Address
     (Code : Code_Interface)
      return Machete_Address
      is abstract;

   procedure Log_Code
     (Code    : Code_Interface;
      Message : String)
   is abstract;

   procedure Append
     (Code  : in out Code_Interface;
      Value : Word_8)
   is abstract;

--     procedure Append_Cell
--       (Code        : in out Code_Interface;
--        Cell        : Machete.Cells.Cell_Type)
--     is abstract;
--
--     procedure Append_Functor
--       (Code        : in out Code_Interface;
--        Functor     : Machete_Functor)
--     is abstract;
--
--     procedure Append_Register
--       (Code        : in out Code_Interface;
--        Register    : Machete_Register)
--     is abstract;

   procedure Update_Pointer
     (Code    : in out Code_Interface;
      Address : Machete_Address;
      Value   : Machete_Address)
   is abstract;

   procedure Allocate
     (Code  : in out Code_Interface'Class;
      Size  : Natural);

   procedure Deallocate
     (Code     : in out Code_Interface'Class);

   procedure Call
     (Code     : in out Code_Interface'Class;
      Functor  : Machete_Functor);

   procedure Proceed
     (Code     : in out Code_Interface'Class);

   procedure No_Operation
     (Code     : in out Code_Interface'Class);

   procedure Stop
     (Code     : in out Code_Interface'Class);

   procedure Try_Me_Else
     (Code     : in out Code_Interface'Class;
      Label    :    out Machete_Address);

   procedure Retry_Me_Else
     (Code     : in out Code_Interface'Class;
      Label    : in out Machete_Address);

   procedure Trust_Me
     (Code     : in out Code_Interface'Class;
      Label    : Machete_Address);

   procedure Get_Structure
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor);

   procedure Get_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register);

   procedure Get_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register);

   procedure Unify_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register);

   procedure Unify_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register);

   procedure Put_Structure
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor);

   procedure Put_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register);

   procedure Put_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Argument : Machete_Register);

   procedure Set_Value
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register);

   procedure Set_Variable
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register);

   procedure Get_Constant
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor);

   procedure Get_Integer
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Value    : Integer);

   procedure Put_Integer
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Value    : Integer);

   procedure Put_Constant
     (Code     : in out Code_Interface'Class;
      Register : Machete_Register;
      Functor  : Machete_Functor);

   procedure Set_Integer
     (Code     : in out Code_Interface'Class;
      Value    : Integer);

   procedure Set_Constant
     (Code     : in out Code_Interface'Class;
      Functor  : Machete_Functor);

   procedure Unify_Constant
     (Code     : in out Code_Interface'Class;
      Functor  : Machete_Functor);

   procedure Unify_Integer
     (Code  : in out Code_Interface'Class;
      Value : Integer);

   function Register_Image
     (Register  : Machete_Register;
      Base_Name : Character := 'X')
      return String;

   function Scan_Address
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Machete_Address;

   function Scan_Register
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Machete_Register;

   function Scan_Functor
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Machete_Functor;

   function Scan_Integer
     (Memory   : in out Machete.Memory.Machete_Memory'Class;
      PC       : in out Machete_Address)
      return Integer;

end Machete.Code;
