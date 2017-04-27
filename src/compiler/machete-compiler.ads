private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Fixed.Hash;

with Machete.Machine;
with Machete.Terms;

package Machete.Compiler is

   type Query_Binding is private;

   function Get_Register_Binding
     (Binding       : Query_Binding;
      Variable_Name : String)
      return Machete_Register;

   type Compile_Context is private;

   procedure New_Context
     (Context : in out Compile_Context);

   procedure Add_Term
     (Context  : in out Compile_Context;
      Term     : Machete.Terms.Machete_Term;
      Machine  : in out Machete.Machine.Machete_Machine'Class);

   procedure End_Context
     (Context  : in out Compile_Context;
      Machine  : in out Machete.Machine.Machete_Machine'Class);

   procedure Compile_Query
     (Query_Term : Machete.Terms.Machete_Term;
      Machine    : in out Machete.Machine.Machete_Machine'Class;
      Bindings   : in out Query_Binding);

   procedure Compile_Patch_Term
     (Term     : Machete.Terms.Machete_Term;
      Machine  : in out Machete.Machine.Machete_Machine'Class);

private

   type Rule_Record is
      record
         Has_Head    : Boolean;
         Has_Body    : Boolean;
         Rule_Head   : Machete.Terms.Machete_Term;
         Rule_Body   : Machete.Terms.Machete_Term;
      end record;

   package Rule_Vectors is
     new Ada.Containers.Vectors (Positive, Rule_Record);

   package Query_Vectors is
     new Ada.Containers.Vectors (Positive, Code_Address);

   type Compile_Context is
      record
         Have_Current : Boolean := False;
         Current      : Machete_Functor;
         Rules        : Rule_Vectors.Vector;
         Queries      : Query_Vectors.Vector;
      end record;

   type Variable_Record is
      record
         Defining_Term : Machete.Terms.Machete_Term;
         Register      : Machete_Register;
         Is_Permanent  : Boolean;
         Is_Set        : Boolean;
      end record;

   package Variable_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Variable_Record,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Query_Binding is
      record
         Map : Variable_Maps.Map;
      end record;

   function Get_Register_Binding
     (Binding       : Query_Binding;
      Variable_Name : String)
      return Machete_Register
   is (Binding.Map.Element (Variable_Name).Register);

end Machete.Compiler;
