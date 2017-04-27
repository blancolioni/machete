private with Ada.Finalization;
private with Ada.Containers.Ordered_Maps;

with Machete.Code;
with Machete.Functors;
with Machete.Memory;

package Machete.Machine is

   type Machete_Machine is
   limited new Machete.Code.Code_Interface with private;

   function Memory
     (Machine : in out Machete_Machine'Class)
      return access Machete.Memory.Machete_Memory'Class;

   function Code_Top
     (Machine : Machete_Machine'Class)
      return Code_Address;

   procedure Define
     (Machine  : in out Machete_Machine'Class;
      Functor  : Machete_Functor);

   procedure Define
     (Machine  : in out Machete_Machine'Class;
      Functor  : Machete_Functor;
      Handler  : Machete.Functors.Predicate_Handler);

   procedure Execute
     (Machine  : in out Machete_Machine'Class;
      Start    : Machete_Address;
      Continue : not null access function return Boolean;
      Success  : out Boolean);

   function Get_Register_Address
     (Machine  : Machete_Machine'Class;
      Register : Machete_Register)
      return Machete_Address;

private

   type Register_Type is
     (P, CP, E, B, A, TR, H, HB, B0, S, ARGC);

   type Register_Array is array (Register_Type) of Machete_Address;

   type Execution_Mode is (Read, Write);

   type Meta_Instruction is
     (No_Meta,
      Meta_Asserta,
      Meta_Assertz,
      Meta_Atom,
      Meta_Call,
      Meta_Var);

   type Functor_Record is
      record
         Meta      : Meta_Instruction := No_Meta;
         Primitive : Machete.Functors.Predicate_Handler;
         Address   : Machete_Address    := 0;
      end record;

   package Functor_Maps is
     new Ada.Containers.Ordered_Maps
       (Machete_Functor, Functor_Record, "<");

   type Machete_Machine is limited new Ada.Finalization.Limited_Controlled
     and Machete.Code.Code_Interface with
      record
         Memory      : aliased Machete.Memory.Machete_Memory;
         Rs          : Register_Array;
         Code_Top    : Code_Address := Code_Address'First;
         Definitions : Functor_Maps.Map;
         Fail        : Boolean;
         Stop        : Boolean;
         Mode        : Execution_Mode;
      end record;

   overriding procedure Initialize
     (Machine : in out Machete_Machine);

   overriding function Current_Code_Address
     (Machine : Machete_Machine)
      return Machete_Address
   is (Machine.Code_Top);

   overriding procedure Update_Pointer
     (Machine : in out Machete_Machine;
      Address : Machete_Address;
      Value   : Machete_Address);

   overriding procedure Log_Code
     (Machine : Machete_Machine;
      Message : String);

   overriding procedure Append
     (Machine     : in out Machete_Machine;
      Value       : Word_8);

--     overriding procedure Append_Cell
--       (Machine     : in out Machete_Machine;
--        Cell        : Machete.Cells.Cell_Type);
--
--     overriding procedure Append_Functor
--       (Machine     : in out Machete_Machine;
--        Functor     : Machete_Functor);
--
--     overriding procedure Append_Register
--       (Machine     : in out Machete_Machine;
--        Register    : Machete_Register);

   function Get_From_Register
     (Machine  : Machete_Machine'Class;
      Register : Machete_Register)
      return Machete_Address;

   function P (Machine : Machete_Machine'Class) return Machete_Address
   is (Machine.Rs (P));

   function E (Machine : Machete_Machine'Class) return Machete_Address
   is (Machine.Rs (E));

   function B (Machine : Machete_Machine'Class) return Machete_Address
   is (Machine.Rs (B));

   function New_Frame
     (Machine : in out Machete_Machine'Class)
      return Machete_Address
   is (if Machine.E > Machine.B
       then Machine.E
       + Machine.Memory.Get_Address (Machine.E, 2) * Word_Size
       + 3 * Word_Size
       else Machine.B
       + Machine.Memory.Get_Address (Machine.B) * Word_Size
       + 7 * Word_Size);

   procedure Backtrack (Machine : in out Machete_Machine'Class);

   procedure Bind
     (Machine : in out Machete_Machine'Class;
      Left, Right : Machete_Address);

   procedure Trail
     (Machine : in out Machete_Machine'Class;
      Address : Machete_Address);

   procedure Unify
     (Machine     : in out Machete_Machine'Class;
      Left, Right : Machete_Address;
      Fail        : out Boolean);

   procedure Unwind_Trail
     (Machine : in out Machete_Machine'Class;
      Saved_TR : Machete_Address);

   function Code_Top
     (Machine : Machete_Machine'Class)
      return Code_Address
   is (Machine.Code_Top);

end Machete.Machine;
