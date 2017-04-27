with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;

with Machete.Functors;

package body Machete.Terms is

   Next_Anonymous_Variable : Positive := 1;

   function Next_Anonymous_Name return String;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   package List_Of_Free_Terms is
     new Ada.Containers.Doubly_Linked_Lists (Machete_Term_Access);

   Free_List : List_Of_Free_Terms.List;

   type Term_Allocation_Record is
      record
         Allocated : Natural := 0;
         Active    : Natural := 0;
      end record;

   Term_Allocation : Term_Allocation_Record;

   function Create
     (Rec : Machete_Term_Record)
      return Machete_Term;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Machete_Term) return Boolean is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Left.Term = Right.Term then
         return True;
      end if;

      if Left.Term.Is_Variable /= Right.Term.Is_Variable
        or else Left.Term.Is_Integer /= Right.Term.Is_Integer
        or else Left.Term.Head /= Right.Term.Head
        or else Left.Term.Arguments.Last_Index
          /= Right.Term.Arguments.Last_Index
      then
         return False;
      end if;

      for I in 1 .. Left.Term.Arguments.Last_Index loop
         if Left.Term.Arguments.Element (I)
           /= Right.Term.Arguments.Element (I)
         then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   function Active_Terms return Natural is (Term_Allocation.Active);

   -------------------
   -- Add_Arguments --
   -------------------

   function Add_Arguments
     (To_Term       : Machete_Term;
      New_Arguments : Array_Of_Terms)
      return Machete_Term
   is
      Info : Machete_Term_Record := To_Term.Term.all;
   begin
      for Arg of New_Arguments loop
         Info.Arguments.Append (Arg);
      end loop;
      return Create (Info);
   end Add_Arguments;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Term : in out Machete_Term) is
   begin
      if Term.Term /= null then
         Term.Term.Count := Term.Term.Count + 1;
      end if;
   end Adjust;

   function Allocated_Terms return Natural is (Term_Allocation.Allocated);

   ------------
   -- Append --
   ------------

   procedure Append
     (Vector  : in out Term_Vector'Class;
      Element : Machete_Term)
   is
   begin
      Vector.V.Append (Element);
   end Append;

   ------------
   -- Create --
   ------------

   function Create
     (Rec : Machete_Term_Record)
      return Machete_Term
   is
      Info : Machete_Term_Access;
   begin
      Term_Allocation.Active := Term_Allocation.Active + 1;
      if not Free_List.Is_Empty then
         Info := Free_List.First_Element;
         Free_List.Delete_First;
         Info.all := Rec;
      else
         Info := new Machete_Term_Record'(Rec);
         Term_Allocation.Allocated := Term_Allocation.Allocated + 1;
      end if;
      Info.Count := 1;
      return Term : Machete_Term do
         Term.Term := Info;
      end return;
   end Create;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Vector  : in out Term_Vector'Class)
   is
   begin
      Vector.V.Delete_First;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Vector  : in out Term_Vector'Class)
   is
   begin
      Vector.V.Delete_Last;
   end Delete_Last;

   ---------------------
   -- Empty_List_Term --
   ---------------------

   function Empty_List_Term return Machete_Term is
   begin
      return New_Atom ("[]");
   end Empty_List_Term;

   ----------------
   -- False_Term --
   ----------------

   function False_Term return Machete_Term is
   begin
      return New_Atom ("fail");
   end False_Term;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Term : in out Machete_Term) is
   begin

      if Term.Term /= null then
         if Term.Term.Count > 0 then
            Term.Term.Count := Term.Term.Count - 1;

            if Term.Term.Count = 0 then
               Term_Allocation.Active := Term_Allocation.Active - 1;
               Free_List.Append (Term.Term);
               Term.Term := null;
            end if;
         end if;
      end if;
   end Finalize;

   -------------
   -- Functor --
   -------------

   function Functor (Term : Machete_Term) return Machete_Functor is
   begin
      return Machete.Functors.Get_Functor
        (Atom (Term), Arity (Term));
   end Functor;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Term : in out Machete_Term) is
   begin
      null;
   end Initialize;

   --------------
   -- New_Atom --
   --------------

   function New_Atom (Name : String) return Machete_Term is
      Rec : constant Machete_Term_Record :=
         (Is_Variable  => False,
          Is_Anonymous => False,
          Is_Integer   => False,
          Head         => +Name,
          Arguments    => Term_Vectors.Empty_Vector,
          Count        => 1);
   begin
      return Create (Rec);
   end New_Atom;

   -----------------------
   -- New_Compound_Term --
   -----------------------

   function New_Compound_Term
     (Name      : String;
      Arguments : Array_Of_Terms)
      return Machete_Term
   is
      Result : Machete_Term_Record :=
                 (Is_Variable  => False,
                  Is_Anonymous => False,
                  Is_Integer   => False,
                  Head         => +Name,
                  Arguments    => Term_Vectors.Empty_Vector,
                  Count        => 1);
   begin
      for Arg of Arguments loop
         Result.Arguments.Append (Arg);
      end loop;
      return Create (Result);
   end New_Compound_Term;

   ----------------------
   -- New_Integer_Term --
   ----------------------

   function New_Integer_Term (Value : Integer) return Machete_Term
   is
      Rec : constant Machete_Term_Record :=
              (Is_Variable  => False,
               Is_Anonymous => False,
               Is_Integer   => True,
               Head         =>
                 +Ada.Strings.Fixed.Trim
                 (Integer'Image (Value), Ada.Strings.Left),
               Arguments    => Term_Vectors.Empty_Vector,
               Count        => 1);
   begin
      return Create (Rec);
   end New_Integer_Term;

   -----------------------
   -- New_Variable_Term --
   -----------------------

   function New_Variable_Term (Name : String) return Machete_Term
   is

      Var_Name : constant String :=
                   (if Name = "_"
                    then Next_Anonymous_Name
                    else Name);
      Rec : constant Machete_Term_Record :=
              (Is_Variable  => True,
               Is_Anonymous => Name = "_",
               Is_Integer   => False,
               Head         => +Var_Name,
               Arguments    => Term_Vectors.Empty_Vector,
               Count        => 1);
   begin
      return Create (Rec);
   end New_Variable_Term;

   ---------------------
   -- New_With_Vector --
   ---------------------

   function New_With_Vector
     (Name      : String;
      Arguments : Term_Vector'Class)
      return Machete_Term
   is
      use Ada.Strings.Unbounded;
      Result : constant Machete_Term_Record :=
                 (Is_Variable  => False,
                  Is_Anonymous => False,
                  Is_Integer   => False,
                  Head         => To_Unbounded_String (Name),
                  Arguments    => Arguments.V,
                  Count        => 1);
   begin
      return Create (Result);
   end New_With_Vector;

   -------------------------
   -- Next_Anonymous_Name --
   -------------------------

   function Next_Anonymous_Name return String is
      Result : String := Positive'Image (Next_Anonymous_Variable);
   begin
      Next_Anonymous_Variable := Next_Anonymous_Variable + 1;
      Result (Result'First) := '_';
      return Result;
   end Next_Anonymous_Name;

   --------------------
   -- Term_Variables --
   --------------------

   function Term_Variables
     (Term : Machete_Term)
      return Array_Of_Terms
   is
      Start : Array_Of_Terms (1 .. 0);

      function Scan (Item    : Machete_Term;
                     Current : Array_Of_Terms)
                     return Array_Of_Terms;

      function Scan_Clause
        (Item    : Machete_Term;
         Index   : Positive;
         Current : Array_Of_Terms)
         return Array_Of_Terms;

      ----------
      -- Scan --
      ----------

      function Scan (Item    : Machete_Term;
                     Current : Array_Of_Terms)
                     return Array_Of_Terms
      is
      begin
         if Is_Variable (Item) then
            for V of Current loop
               if Variable_Name (V) = Variable_Name (Item) then
                  return Current;
               end if;
            end loop;
            return Current & Item;
         else
            return Scan_Clause (Item, 1, Current);
         end if;
      end Scan;

      -----------------
      -- Scan_Clause --
      -----------------

      function Scan_Clause
        (Item    : Machete_Term;
         Index   : Positive;
         Current : Array_Of_Terms)
         return Array_Of_Terms
      is
      begin
         if Index <= Arity (Item) then
            return Scan_Clause
              (Item, Index + 1, Scan (Argument (Item, Index), Current));
         else
            return Current;
         end if;
      end Scan_Clause;

   begin
      return Scan (Term, Start);
   end Term_Variables;

   ---------------
   -- True_Term --
   ---------------

   function True_Term return Machete_Term is
   begin
      return New_Atom ("true");
   end True_Term;

end Machete.Terms;
