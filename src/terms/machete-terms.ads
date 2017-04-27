private with Ada.Finalization;
private with Ada.Strings.Unbounded;

with Ada.Containers.Vectors;

package Machete.Terms is

   type Machete_Term is private;

   function True_Term return Machete_Term;
   function False_Term return Machete_Term;
   function Empty_List_Term return Machete_Term;

   function Is_Variable (Term : Machete_Term) return Boolean;
   function Is_Anonymous (Term : Machete_Term) return Boolean;

   function Is_Integer (Term : Machete_Term) return Boolean;
   function Is_Compound_Term (Term : Machete_Term) return Boolean;

   function Variable_Name (Term : Machete_Term) return String
     with Pre => Is_Variable (Term);

   function Integer_Value (Term : Machete_Term) return Integer
     with Pre => Is_Integer (Term);

   function Atom (Term : Machete_Term) return String
     with Pre => Is_Compound_Term (Term);

   function Arity (Term : Machete_Term) return Natural
     with Pre => Is_Compound_Term (Term);

   function Is_Atom (Term : Machete_Term) return Boolean
   is (Is_Compound_Term (Term) and then Arity (Term) = 0);

   function Functor (Term : Machete_Term) return Machete_Functor
     with Pre => Is_Compound_Term (Term);

   function Argument (Term  : Machete_Term;
                      Index : Positive)
                      return Machete_Term
     with Pre => Is_Compound_Term (Term) and then Index <= Arity (Term);

   type Array_Of_Terms is array (Positive range <>) of Machete_Term;

   function New_Variable_Term (Name : String) return Machete_Term;
   function New_Atom (Name : String) return Machete_Term;
   function New_Integer_Term (Value : Integer) return Machete_Term;
   function New_Compound_Term
     (Name      : String;
      Arguments : Array_Of_Terms)
      return Machete_Term;

   function Term_Variables
     (Term : Machete_Term)
      return Array_Of_Terms;

   function Add_Arguments
     (To_Term       : Machete_Term;
      New_Arguments : Array_Of_Terms)
      return Machete_Term;

   type Term_Vector is tagged private;

   function Element
     (Vector : Term_Vector'Class;
      Index  : Positive)
      return Machete_Term;

   function First_Element
     (Vector : Term_Vector'Class)
      return Machete_Term;

   function Last_Element
     (Vector : Term_Vector'Class)
      return Machete_Term;

   function Last_Index
     (Vector : Term_Vector'Class)
      return Natural;

   function Is_Empty
     (Vector : Term_Vector'Class)
      return Boolean;

   procedure Append
     (Vector  : in out Term_Vector'Class;
      Element : Machete_Term);

   procedure Delete_First
     (Vector  : in out Term_Vector'Class);

   procedure Delete_Last
     (Vector  : in out Term_Vector'Class);

   function New_With_Vector
     (Name      : String;
      Arguments : Term_Vector'Class)
      return Machete_Term;

private

   type Machete_Term_Record;

   type Machete_Term_Access is access all Machete_Term_Record;

   type Machete_Term is new Ada.Finalization.Controlled with
      record
         Term  : Machete_Term_Access;
      end record;

   overriding procedure Initialize (Term : in out Machete_Term);
   overriding procedure Finalize (Term : in out Machete_Term);
   overriding procedure Adjust (Term : in out Machete_Term);

   overriding function "=" (Left, Right : Machete_Term) return Boolean;

   package Term_Vectors is
     new Ada.Containers.Vectors (Positive, Machete_Term);

   type Machete_Term_Record is
      record
         Count        : Natural;
         Is_Anonymous : Boolean;
         Is_Variable  : Boolean;
         Is_Integer   : Boolean;
         Head         : Ada.Strings.Unbounded.Unbounded_String;
         Arguments    : Term_Vectors.Vector;
      end record;

   type Term_Vector is tagged
      record
         V : Term_Vectors.Vector;
      end record;

   function Element
     (Vector : Term_Vector'Class;
      Index  : Positive)
      return Machete_Term
   is (Vector.V.Element (Index));

   function First_Element
     (Vector : Term_Vector'Class)
      return Machete_Term
   is (Vector.V.First_Element);

   function Last_Element
     (Vector : Term_Vector'Class)
      return Machete_Term
   is (Vector.V.Last_Element);

   function Last_Index
     (Vector : Term_Vector'Class)
      return Natural
   is (Vector.V.Last_Index);

   function Is_Empty
     (Vector : Term_Vector'Class)
      return Boolean
   is (Vector.V.Is_Empty);

   function Is_Variable (Term : Machete_Term) return Boolean
   is (Term.Term.Is_Variable);

   function Is_Anonymous (Term : Machete_Term) return Boolean
   is (Term.Term.Is_Anonymous);

   function Is_Integer (Term : Machete_Term) return Boolean
   is (Term.Term.Is_Integer);

   function Is_Compound_Term (Term : Machete_Term) return Boolean
   is (not Term.Is_Variable);

   function Variable_Name (Term : Machete_Term) return String
   is (Ada.Strings.Unbounded.To_String (Term.Term.Head));

   function Integer_Value (Term : Machete_Term) return Integer
   is (Integer'Value (Atom (Term)));

   function Atom (Term : Machete_Term) return String
   is (Ada.Strings.Unbounded.To_String (Term.Term.Head));

   function Arity (Term : Machete_Term) return Natural
   is (Term.Term.Arguments.Last_Index);

   function Argument (Term  : Machete_Term;
                      Index : Positive)
                      return Machete_Term
   is (Term.Term.Arguments.Element (Index));

   function Allocated_Terms return Natural;
   function Active_Terms return Natural;

end Machete.Terms;
