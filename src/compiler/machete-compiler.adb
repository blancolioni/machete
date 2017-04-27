with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Text_IO;

with Machete.Code;
with Machete.Functors;

with Machete.Terms.Images;

package body Machete.Compiler is

   type Term_Register_Record is
      record
         Register : Machete_Register;
         Term     : Machete.Terms.Machete_Term;
      end record;

   package Term_Register_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Term_Register_Record);

   package Variable_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => String,
        Hash                => Ada.Strings.Fixed.Hash,
        Equivalent_Elements => "=");

   procedure Compile_Fact
     (Fact_Term : Machete.Terms.Machete_Term;
      Machine   : in out Machete.Machine.Machete_Machine'Class);

   procedure Compile_Rule_Term
     (Term               : Machete.Terms.Machete_Term;
      Machine            : in out Machete.Machine.Machete_Machine'Class;
      Next_R             : in out Machete_Register;
      Variables          : in out Variable_Maps.Map;
      Clear_Temporaries  : Boolean);

   procedure Compile_Fact
     (Program_Term : Machete.Terms.Machete_Term;
      Machine      : in out Machete.Machine.Machete_Machine'Class;
      Next_R       : in out Machete_Register;
      Variables    : in out Variable_Maps.Map;
      Define       : Boolean);

   procedure Compile_Rule
     (Head         : Machete.Terms.Machete_Term;
      Clauses      : Machete.Terms.Machete_Term;
      Machine      : in out Machete.Machine.Machete_Machine'Class);

   procedure Compile_Current
     (Context : in out Compile_Context;
      Machine  : in out Machete.Machine.Machete_Machine'Class);

   procedure Execute_Query
     (Machine     : in out Machete.Machine.Machete_Machine'Class;
      Term        : Machete.Terms.Machete_Term;
      Start       : Code_Address;
      Bindings    : Machete.Compiler.Query_Binding;
      Interactive : Boolean);

   function To_DCG_Body
     (Term : Machete.Terms.Machete_Term)
      return Machete.Terms.Machete_Term;

   function To_Rule
     (Term : Machete.Terms.Machete_Term)
      return Rule_Record;

   procedure Scan_Rule
     (Rule    : Machete.Terms.Machete_Term;
      Process : not null access
        procedure (T : Machete.Terms.Machete_Term));

   --------------
   -- Add_Term --
   --------------

   procedure Add_Term
     (Context : in out Compile_Context;
      Term    : Machete.Terms.Machete_Term;
      Machine  : in out Machete.Machine.Machete_Machine'Class)
   is
      use Machete.Terms;
      Rule    : constant Rule_Record :=
                  To_Rule (Term);
   begin
      if Rule.Has_Head then
         declare
            Functor : constant Machete_Functor :=
                        Machete.Terms.Functor (Rule.Rule_Head);
         begin
            if Context.Have_Current
              and then Functor /= Context.Current
            then
               Compile_Current (Context, Machine);
            end if;

            Context.Rules.Append (Rule);
            Context.Have_Current := True;
            Context.Current := Functor;
         end;
      else
         if Context.Have_Current then
            Compile_Current (Context, Machine);
         end if;

         Context.Rules.Append
           (To_Rule (Term));
         Compile_Current (Context, Machine);
         Context.Have_Current := False;
      end if;

   end Add_Term;

   ---------------------
   -- Compile_Current --
   ---------------------

   procedure Compile_Current
     (Context : in out Compile_Context;
      Machine  : in out Machete.Machine.Machete_Machine'Class)
   is
      use Machete.Terms;
      Bindings     : Query_Binding;
      Label        : Machete_Address;
   begin

      if Context.Have_Current then
         Machine.Define (Context.Current);
      end if;

      for Rule_Index in 1 .. Context.Rules.Last_Index loop
         declare
            Rule : Rule_Record renames Context.Rules (Rule_Index);
         begin
            if Context.Rules.Last_Index > 1 then
               if Rule_Index = 1 then
                  Machine.Try_Me_Else (Label);
               elsif Rule_Index < Context.Rules.Last_Index then
                  Machine.Retry_Me_Else (Label);
               else
                  Machine.Trust_Me (Label);
               end if;
            else
               --  room for subsequent patches
               --  (should only do this if dynamic is set)
               for I in 1 .. 5 loop
                  Machine.No_Operation;
               end loop;
            end if;
            if not Rule.Has_Head then
               declare
                  Query_Start : constant Machete_Address :=
                                  Machine.Code_Top;
               begin
                  Context.Queries.Append (Query_Start);
                  Compile_Query
                    (Rule.Rule_Body, Machine, Bindings);
                  Machine.Stop;

                  Ada.Text_IO.Put_Line
                    (Machete.Terms.Images.Image
                       (Rule.Rule_Body));

                  Execute_Query
                    (Machine     => Machine,
                     Term        => Rule.Rule_Body,
                     Start       => Query_Start,
                     Bindings    => Bindings,
                     Interactive => False);
               end;
            elsif not Rule.Has_Body then
               Compile_Fact (Rule.Rule_Head, Machine);
            else
               Compile_Rule (Rule.Rule_Head, Rule.Rule_Body, Machine);
            end if;
            if Context.Have_Current then
               Machine.Proceed;
            end if;

         end;
      end loop;

      Context.Rules.Clear;
      Context.Have_Current := False;
   end Compile_Current;

   ------------------
   -- Compile_Fact --
   ------------------

   procedure Compile_Fact
     (Fact_Term : Machete.Terms.Machete_Term;
      Machine   : in out Machete.Machine.Machete_Machine'Class)
   is
      Variables : Variable_Maps.Map;
      Var_Terms : constant Machete.Terms.Array_Of_Terms :=
                    Machete.Terms.Term_Variables (Fact_Term);
      Next_R    : Machete_Register :=
                    Machete_Register (Machete.Terms.Arity (Fact_Term));
   begin
      for V of Var_Terms loop
         Variables.Insert
           (Machete.Terms.Variable_Name (V),
            (V, 0, False, False));
      end loop;
      Compile_Fact (Fact_Term, Machine, Next_R, Variables, False);
   end Compile_Fact;

   ------------------
   -- Compile_Fact --
   ------------------

   procedure Compile_Fact
     (Program_Term : Machete.Terms.Machete_Term;
      Machine      : in out Machete.Machine.Machete_Machine'Class;
      Next_R       : in out Machete_Register;
      Variables    : in out Variable_Maps.Map;
      Define       : Boolean)
   is
      use Machete.Terms;

      Temporaries : Variable_Sets.Set;
      Terms  : Term_Register_Lists.List;

      procedure Compile
        (T : Machete_Term;
         R : Machete_Register);

      procedure Scan_Variables (T : Machete_Term);

      -------------
      -- Compile --
      -------------

      procedure Compile
        (T : Machete_Term;
         R : Machete_Register)
      is
      begin
         if Is_Variable (T) then
            declare
               Name : constant String := Variable_Name (T);
            begin
               if not Variables (Name).Is_Permanent
                 and then not Temporaries.Contains (Name)
               then
                  Temporaries.Insert (Name);
                  Variables (Name).Is_Set := False;
                  Variables (Name).Register := R;
                  Variables (Name).Defining_Term := T;
               end if;
            end;

         elsif Is_Integer (T) then
            Machine.Get_Integer (R, Integer_Value (T));
         elsif Is_Atom (T) then
            Machine.Get_Constant (R, Functor (T));
         else

            declare
               Rs : array (1 .. Arity (T)) of Machete_Register;
            begin
               for I in 1 .. Arity (T) loop
                  declare
                     Arg : constant Machete_Term := Argument (T, I);
                  begin
                     if Is_Variable (Arg) then
                        declare
                           Name : constant String := Variable_Name (Arg);
                        begin
                           if Variables (Name).Is_Permanent
                             or else Temporaries.Contains (Name)
                           then
                              Rs (I) := Variables (Name).Register;
                           else
                              Next_R := Next_R + 1;
                              Rs (I) := Next_R;
                              Variables (Name).Register := Next_R;
                              Variables (Name).Is_Set := False;
                              Temporaries.Insert (Name);
                           end if;
                        end;
                     else
                        Next_R := Next_R + 1;
                        Rs (I) := Next_R;
                        Terms.Append ((Next_R, Arg));
                     end if;
                  end;
               end loop;

               Machine.Get_Structure (R, Functor (T));

               for I in 1 .. Arity (T) loop
                  if Is_Variable (Argument (T, I)) then
                     declare
                        Name    : constant String :=
                                    Variable_Name (Argument (T, I));
                        Register : constant Machete_Register :=
                                     Variables (Name).Register;
                        Is_Set   : constant Boolean :=
                                     Variables (Name).Is_Set;
                     begin
                        Variables (Name).Is_Set := True;

                        if not Is_Set then
                           Machine.Unify_Variable (Register);
                        else
                           Machine.Unify_Value (Register);
                        end if;
                     end;
                  else
                     Machine.Unify_Variable (Rs (I));
                  end if;
               end loop;

            end;
         end if;
      end Compile;

      --------------------
      -- Scan_Variables --
      --------------------

      procedure Scan_Variables (T : Machete_Term) is
      begin
         if Is_Variable (T) then
            declare
               Name : constant String := Variable_Name (T);
            begin
               if not Variables (Name).Is_Permanent
                 and then not Temporaries.Contains (Name)
               then
                  Temporaries.Insert (Name);
                  Next_R := Next_R + 1;
                  Variables (Name).Register := Next_R;
                  Variables (Name).Is_Set := False;
                  Variables (Name).Defining_Term := T;
               end if;
            end;
         else
            for I in 1 .. Arity (T) loop
               Scan_Variables (Argument (T, I));
            end loop;
         end if;
      end Scan_Variables;

   begin

--        Next_R := Machete_Register (Arity (Program_Term));

      if Define then
         Machine.Define
           (Machete.Terms.Functor (Program_Term));
      end if;

      Scan_Variables (Program_Term);

      for I in 1 .. Arity (Program_Term) loop
         declare
            Arg : constant Machete_Term := Argument (Program_Term, I);
         begin
            if Is_Variable (Arg) then
               declare
                  Name : constant String :=
                           Variable_Name (Arg);
                  Is_Set       : constant Boolean :=
                                   Variables (Name).Is_Set;
                  Register     : constant Machete_Register :=
                                   Variables (Name).Register;
               begin
                  Variables (Name).Is_Set := True;
                  if Is_Set then
                     Machine.Get_Value (Register, Machete_Register (I));
                  elsif not Is_Anonymous (Arg) then
                     Machine.Get_Variable (Register, Machete_Register (I));
                  end if;
               end;
            else
               Compile (Arg, Machete_Register (I));
            end if;
         end;

      end loop;

      while not Terms.Is_Empty loop
         declare
            Rec : constant Term_Register_Record :=
                    Terms.First_Element;
         begin
            Terms.Delete_First;
            Compile (Rec.Term, Rec.Register);
         end;
      end loop;

      if Define then
         Machine.Proceed;
      end if;

   end Compile_Fact;

   ------------------------
   -- Compile_Patch_Term --
   ------------------------

   procedure Compile_Patch_Term
     (Term     : Machete.Terms.Machete_Term;
      Machine  : in out Machete.Machine.Machete_Machine'Class)
   is
      use Machete.Terms;
   begin
      if Atom (Term) = ":-" then
         Compile_Rule
           (Argument (Term, 1), Argument (Term, 2), Machine);
      else
         Compile_Fact
           (Term, Machine);
      end if;
   end Compile_Patch_Term;

   -------------------
   -- Compile_Query --
   -------------------

   procedure Compile_Query
     (Query_Term : Machete.Terms.Machete_Term;
      Machine    : in out Machete.Machine.Machete_Machine'Class;
      Bindings   : in out Query_Binding)
   is
      Variables : Variable_Maps.Map renames Bindings.Map;
      Var_Terms : constant Machete.Terms.Array_Of_Terms :=
                    Machete.Terms.Term_Variables (Query_Term);
      Next_Y    : Machete_Register := Permanent_Register'First;
      Next_R    : Machete_Register :=
                    Machete_Register (Machete.Terms.Arity (Query_Term));
   begin
--        Machete.Logging.Log
--          ("Query: " & Machete.Terms.Images.Image (Query_Term));

      Variables.Clear;
      for V of Var_Terms loop
         Variables.Insert
           (Machete.Terms.Variable_Name (V),
            (V, Next_Y, True, False));
         Next_Y := Next_Y + 1;
      end loop;
      Machine.Allocate (Var_Terms'Length);

      Compile_Rule_Term
        (Query_Term, Machine, Next_R, Variables, True);

   end Compile_Query;

   ------------------
   -- Compile_Rule --
   ------------------

   procedure Compile_Rule
     (Head         : Machete.Terms.Machete_Term;
      Clauses      : Machete.Terms.Machete_Term;
      Machine      : in out Machete.Machine.Machete_Machine'Class)
   is
      use Machete.Terms;
      Head_Variables     : constant Array_Of_Terms :=
                             Term_Variables (Head);
      Chain_Rule         : constant Boolean :=
                             Is_Variable (Clauses)
                               or else Atom (Clauses) /= ",";
      Clause_1_Variables : constant Array_Of_Terms :=
                             (if Chain_Rule
                              then Term_Variables (Clauses)
                              else Term_Variables
                                (Argument (Clauses, 1)));
      Variables          : Variable_Maps.Map;
      Next_R             : Machete_Register := Machete_Register (Arity (Head));
      Next_Y             : Permanent_Register := Permanent_Register'First;

      First_Clause       : Boolean := True;

      procedure Add_Clause_Variables
        (Clause : Machete_Term);

      procedure Compile_Clause
        (Clause : Machete_Term);

      --------------------------
      -- Add_Clause_Variables --
      --------------------------

      procedure Add_Clause_Variables
        (Clause : Machete_Term)
      is
         Clause_Vars : constant Array_Of_Terms :=
                         Term_Variables (Clause);
         Found       : Variable_Sets.Set;
      begin
         for V of Clause_Vars loop
            declare
               Name : constant String := Variable_Name (V);
            begin
               if not Variables.Contains (Name) then
                  Variables.Insert (Name, (V, 0, False, False));
                  Found.Insert (Name);
               elsif not Found.Contains (Name) then
                  if not Variables (Name).Is_Permanent then
                     Variables (Name).Is_Permanent := True;
                     Variables (Name).Register := Next_Y;
                     Next_Y := Next_Y + 1;
                  end if;
                  Found.Insert (Name);
               end if;
            end;
         end loop;
      end Add_Clause_Variables;

      --------------------
      -- Compile_Clause --
      --------------------

      procedure Compile_Clause
        (Clause : Machete_Term)
      is
      begin
         if not First_Clause then
            Next_R :=
              Machete_Register
                (if Is_Variable (Clause) then 1 else Arity (Clause));
         end if;
         Compile_Rule_Term
           ((if Is_Variable (Clause)
            then New_Compound_Term ("call", (1 => Clause))
            else Clause),
            Machine, Next_R, Variables,
            not First_Clause);
         First_Clause := False;
      end Compile_Clause;

   begin

      for V of Head_Variables loop
         Variables.Insert (Variable_Name (V), (V, 0, False, False));
      end loop;

      for V of Clause_1_Variables loop
         if not Variables.Contains (Variable_Name (V)) then
            Variables.Insert (Variable_Name (V), (V, 0, False, False));
         end if;
      end loop;

      if not Chain_Rule then
         Scan_Rule (Argument (Clauses, 2), Add_Clause_Variables'Access);
      end if;

      Machine.Allocate
        (Natural (Next_Y - Permanent_Register'First));

      Compile_Fact (Head, Machine, Next_R, Variables, False);

      Scan_Rule (Clauses, Compile_Clause'Access);

      Machine.Deallocate;

   end Compile_Rule;

   -----------------------
   -- Compile_Rule_Term --
   -----------------------

   procedure Compile_Rule_Term
     (Term              : Machete.Terms.Machete_Term;
      Machine           : in out Machete.Machine.Machete_Machine'Class;
      Next_R            : in out Machete_Register;
      Variables         : in out Variable_Maps.Map;
      Clear_Temporaries : Boolean)
   is
      use Machete.Terms;

      Temporaries : Variable_Sets.Set;

      procedure Compile
        (T : Machete_Term;
         R : Machete_Register);

      procedure Scan_Variables (T : Machete_Term);

      -------------
      -- Compile --
      -------------

      procedure Compile
        (T : Machete_Term;
         R : Machete_Register)
      is
      begin
         if Is_Variable (T) then
            declare
               Name : constant String := Variable_Name (T);
            begin
               if (Clear_Temporaries or else Variables (Name).Register = 0)
                 and then not Variables (Name).Is_Permanent
                 and then not Temporaries.Contains (Name)
               then
                  Temporaries.Insert (Name);
                  Variables (Name).Is_Set := False;
                  Variables (Name).Register := R;
                  Variables (Name).Defining_Term := T;
               end if;
            end;

         elsif Is_Integer (T) then

            Machine.Put_Integer (R, Integer_Value (T));

         elsif Is_Atom (T) then

            Machine.Put_Constant (R, Functor (T));

         else

            declare
               Rs : array (1 .. Arity (T)) of Machete_Register;
            begin
               for I in 1 .. Arity (T) loop
                  declare
                     Arg : constant Machete_Term := Argument (T, I);
                  begin
                     if Is_Variable (Arg) then
                        declare
                           Name : constant String := Variable_Name (Arg);
                        begin
                           if Variables (Name).Is_Permanent
                             or else Temporaries.Contains (Name)
                           then
                              Rs (I) := Variables (Name).Register;
                           else
                              Next_R := Next_R + 1;
                              Rs (I) := Next_R;
                              Variables (Name).Register := Next_R;
                              Variables (Name).Is_Set := False;
                              Temporaries.Insert (Name);
                           end if;
                        end;
                     else
                        Next_R := Next_R + 1;
                        Rs (I) := Next_R;
                     end if;
                  end;
               end loop;

               for I in 1 .. Arity (T) loop
                  Compile (Argument (T, I), Rs (I));
               end loop;

               Machine.Put_Structure (R, Functor (T));

               for I in 1 .. Arity (T) loop
                  if Is_Variable (Argument (T, I)) then
                     declare
                        Name : constant String :=
                                 Variable_Name (Argument (T, I));
                     begin
                        if Variables (Name).Is_Set then
                           Machine.Set_Value (Variables (Name).Register);
                        else
                           Machine.Set_Variable (Variables (Name).Register);
                           Variables (Name).Is_Set := True;
                        end if;
                     end;
                  else
                     Machine.Set_Value (Rs (I));
                  end if;
               end loop;
            end;
         end if;
      end Compile;

      --------------------
      -- Scan_Variables --
      --------------------

      procedure Scan_Variables (T : Machete_Term) is
      begin
         if Is_Variable (T) then
            declare
               Name : constant String := Variable_Name (T);
            begin
               if (Clear_Temporaries or else Variables (Name).Register = 0)
                 and then not Variables (Name).Is_Permanent
                 and then not Temporaries.Contains (Name)
               then
                  Next_R := Next_R + 1;
                  Variables (Name).Register := Next_R;
                  Variables (Name).Is_Set := False;
                  Variables (Name).Defining_Term := T;
                  Temporaries.Insert (Name);
               end if;
            end;
         else
            for I in 1 .. Arity (T) loop
               Scan_Variables (Argument (T, I));
            end loop;
         end if;
      end Scan_Variables;

   begin
      Next_R := Machete_Register'Max (Machete_Register (Arity (Term)), Next_R);

      Scan_Variables (Term);

      for I in 1 .. Arity (Term) loop
         if Is_Variable (Argument (Term, I)) then
            declare
               Name : constant String :=
                        Variable_Name (Argument (Term, I));
            begin
               if Variables (Name).Is_Set then
                  Machine.Put_Value (Variables (Name).Register,
                                     Machete_Register (I));
               else
                  Machine.Put_Variable (Variables (Name).Register,
                                        Machete_Register (I));
                  Variables (Name).Is_Set := True;
               end if;
            end;
         else
            Compile (Argument (Term, I), Machete_Register (I));
         end if;
      end loop;

      Machine.Call (Functor (Term));

   end Compile_Rule_Term;

   -----------------
   -- End_Context --
   -----------------

   procedure End_Context
     (Context : in out Compile_Context;
      Machine  : in out Machete.Machine.Machete_Machine'Class)
   is
   begin
      if Context.Have_Current then
         Compile_Current (Context, Machine);
         Context.Have_Current := False;
      end if;
   end End_Context;

   -------------------
   -- Execute_Query --
   -------------------

   procedure Execute_Query
     (Machine     : in out Machete.Machine.Machete_Machine'Class;
      Term        : Machete.Terms.Machete_Term;
      Start       : Code_Address;
      Bindings    : Machete.Compiler.Query_Binding;
      Interactive : Boolean)
   is
      use Machete.Terms;

      Success : Boolean;
      First   : Boolean := True;

      Term_Vs : constant Array_Of_Terms :=
                  Term_Variables (Term);

      --        package Variable_Name_Vectors is
      --          new Ada.Containers.Indefinite_Vectors (Positive, String);

      function Continue return Boolean;

      --------------
      -- Continue --
      --------------

      function Continue return Boolean is
      begin
         for V of Term_Vs loop
            declare
               Name     : constant String := Variable_Name (V);
               Register : constant Machete_Register :=
                            Machete.Compiler.Get_Register_Binding
                              (Bindings, Variable_Name (V));
            begin
               if Name (Name'First) /= '_' then
                  if not First then
                     Ada.Text_IO.Put (",");
                  else
                     First := False;
                  end if;
                  Ada.Text_IO.Put
                    (Name & "="
                     & Machine.Memory.Image
                       (Machine.Get_Register_Address (Register)));
               end if;
            end;
         end loop;

         if First then
            return False;
         elsif Interactive then
            declare
               Line : constant String :=
                        Ada.Text_IO.Get_Line;
            begin
               return Line (Line'First) = ';';
            end;
         else
            Ada.Text_IO.New_Line;
            return False;
         end if;
      end Continue;

   begin

      Machine.Execute (Start, Continue'Access, Success);

      if not Success then
         Ada.Text_IO.Put_Line ("no");
      else
         Ada.Text_IO.Put_Line ("yes");
      end if;
   end Execute_Query;

   -----------------
   -- New_Context --
   -----------------

   procedure New_Context
     (Context : in out Compile_Context)
   is
   begin
      Context.Have_Current := False;
      Context.Rules.Clear;
      Context.Queries.Clear;
   end New_Context;

   ---------------
   -- Scan_Rule --
   ---------------

   procedure Scan_Rule
     (Rule    : Machete.Terms.Machete_Term;
      Process : not null access
        procedure (T : Machete.Terms.Machete_Term))
   is
      use Machete.Terms;
   begin
      if not Is_Variable (Rule) and then Atom (Rule) = "," then
         Process (Argument (Rule, 1));
         Scan_Rule (Argument (Rule, 2), Process);
      else
         Process (Rule);
      end if;
   end Scan_Rule;

   -----------------
   -- To_DCG_Body --
   -----------------

   function To_DCG_Body
     (Term : Machete.Terms.Machete_Term)
      return Machete.Terms.Machete_Term
   is
      use Machete.Terms;
      Args : Term_Vector;
      Ts   : Term_Vector;

      Index : Positive := 1;

      procedure Add_Term_Arguments
        (T : Machete_Term);

      procedure Add_Transformed_Terms
        (T : Machete_Term);

      procedure Add_Argument;

      ------------------
      -- Add_Argument --
      ------------------

      procedure Add_Argument is
         Name : String := "_DCG" & Positive'Image (Args.Last_Index);
      begin
         Name (5) := '_';
         Args.Append (New_Variable_Term (Name));
      end Add_Argument;

      ------------------------
      -- Add_Term_Arguments --
      ------------------------

      procedure Add_Term_Arguments
        (T : Machete_Term)
      is
      begin
         if Atom (T) = "." then
            declare
               It : Machete_Term := T;
            begin
               while Atom (It) = "." loop
                  Add_Argument;
                  It := Argument (It, 2);
               end loop;
            end;
         elsif Atom (T) = "[]" then
            Add_Argument;
         elsif Atom (T) = "{}" then
            null;
         else
            Add_Argument;
         end if;
      end Add_Term_Arguments;

      ---------------------------
      -- Add_Transformed_Terms --
      ---------------------------

      procedure Add_Transformed_Terms
        (T : Machete_Term)
      is
      begin
         if Atom (T) = "." then
            declare
               It : Machete_Term := T;
            begin
               while Atom (It) = "." loop
                  Ts.Append
                    (New_Compound_Term
                       ("C",
                        (Args.Element (Index),
                         Argument (It, 1),
                         Args.Element (Index + 1))));
                  Index := Index + 1;
                  It := Argument (It, 2);
               end loop;
            end;
         elsif Atom (T) = "[]" then
            Ts.Append
              (New_Compound_Term
                 ("=",
                  (Args.Element (Index),
                   Args.Element (Index + 1))));
            Index := Index + 1;
         elsif Atom (T) = "{}" then
            for I in 1 .. Arity (T) loop
               Ts.Append (Argument (T, I));
            end loop;
         else
            Ts.Append
              (Add_Arguments
                 (T,
                  (Args.Element (Index),
                   Args.Element (Index + 1))));
            Index := Index + 1;
         end if;
      end Add_Transformed_Terms;

   begin
      Args.Append (New_Variable_Term ("_IN"));

      Scan_Rule (Term, Add_Term_Arguments'Access);

      if Args.Last_Index = 1 then
         Ts.Append
           (New_Compound_Term
              ("=",
               (New_Variable_Term ("_IN"),
                New_Variable_Term ("_OUT"))));
      else
         Args.Delete_Last;
         Args.Append (New_Variable_Term ("_OUT"));
      end if;

      Scan_Rule (Term, Add_Transformed_Terms'Access);

      declare
         Result : Machete_Term;
      begin
         for I in reverse 1 .. Ts.Last_Index loop
            if I = Ts.Last_Index then
               Result := Ts.Element (I);
            else
               Result :=
                 New_Compound_Term
                   (",", (Ts.Element (I), Result));
            end if;
         end loop;
         return Result;
      end;

   end To_DCG_Body;

   -------------
   -- To_Rule --
   -------------

   function To_Rule
     (Term : Machete.Terms.Machete_Term)
      return Rule_Record
   is
      use Machete.Terms;
      Rule : Rule_Record;
   begin
      if Atom (Term) = ":-" then
         if Arity (Term) = 1 then
            Rule.Has_Head := False;
            Rule.Has_Body := True;
            Rule.Rule_Body := Argument (Term, 1);
         else
            Rule.Has_Head := True;
            Rule.Has_Body := True;
            Rule.Rule_Head := Argument (Term, 1);
            Rule.Rule_Body := Argument (Term, 2);
         end if;
      elsif Atom (Term) = "-->" then
         declare
            New_Head : constant Machete_Term :=
                         Add_Arguments
                           (Argument (Term, 1),
                            (New_Variable_Term ("_IN"),
                             New_Variable_Term ("_OUT")));
            Old_Body : constant Machete_Term :=
                         Argument (Term, 2);
            New_Body : constant Machete_Term :=
                         To_DCG_Body (Old_Body);
         begin
            return To_Rule
              (New_Compound_Term
                 (":-", (New_Head, New_Body)));
         end;

      else
         Rule.Has_Head := True;
         Rule.Has_Body := False;
         Rule.Rule_Head := Term;
      end if;
      return Rule;
   end To_Rule;

end Machete.Compiler;
