with Ada.Characters.Handling;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

--  with Machete.Code;
with Machete.Functors;
with Machete.Memory;

with Machete.Parser.Tokens;            use Machete.Parser.Tokens;
with Machete.Parser.Lexical;           use Machete.Parser.Lexical;

with Machete.Logging;
with Machete.Terms.Images;

package body Machete.Parser is

   Log_Operators : constant Boolean := False;

   type Parse_Context is
      record
         Term_Arguments : Boolean := False;
      end record;

   package Term_Array_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Machete.Terms.Array_Of_Terms, Machete.Terms."=");

   type Query_Record is
      record
         Term     : Machete.Terms.Machete_Term;
         Start    : Code_Address;
         Bindings : Machete.Compiler.Query_Binding;
      end record;

   package Query_Vectors is
     new Ada.Containers.Vectors (Positive, Query_Record);

   function Parse_Atomic_Term
     (Context : Parse_Context)
     return Machete.Terms.Machete_Term;

   function Parse_Operator_Term
     (Context : Parse_Context)
      return Machete.Terms.Machete_Term;

   function Parse_Term
     (Context : Parse_Context := (others => <>))
      return Machete.Terms.Machete_Term;

   function Is_Term_Token (Tok : Token) return Boolean
   is (Tok = Tok_Identifier or else Tok = Tok_Character_Literal
       or else Tok = Tok_Left_Bracket or else Tok = Tok_Left_Paren
       or else Tok = Tok_Left_Brace
       or else Tok = Tok_String_Literal or else Tok = Tok_Integer_Constant
       or else Tok = Tok_Cut or else Tok = Tok_Empty_List);

   function At_Term return Boolean
   is (Is_Term_Token (Tok));

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

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Machine : in out Machete.Machine.Machete_Machine'Class;
      Path    : String)
   is
      Context         : Machete.Compiler.Compile_Context;
   begin

      Machete.Compiler.New_Context (Context);

      Machete.Logging.Log ("Loading:" & Path);

      Open (Path);

      while Tok /= Tok_End_Of_File loop

         if not At_Term then
            Error ("expected term");
            while Tok_Indent > 1 loop
               Scan;
            end loop;

            exit when not At_Term;

         end if;

         declare
            use Machete.Terms;
            Term : constant Machete_Term := Parse_Term;
         begin

            Machete.Compiler.Add_Term
              (Context, Term, Machine);

            if Tok = Tok_Dot then
               Scan;
            else
               Error ("missing '.'");
            end if;
         end;

      end loop;

      Machete.Compiler.End_Context (Context, Machine);

      Close;

   end Load_File;

   -----------------------
   -- Parse_Atomic_Term --
   -----------------------

   function Parse_Atomic_Term
     (Context : Parse_Context)
      return Machete.Terms.Machete_Term
   is
      use Ada.Characters.Handling;
      use Machete.Terms;

      Argument_Context : Parse_Context := Context;

      function Parse_Arguments return Array_Of_Terms;
      function Parse_Rest_Of_List return Machete_Term;

      ---------------------
      -- Parse_Arguments --
      ---------------------

      function Parse_Arguments return Array_Of_Terms is
         Front : constant Machete_Term := Parse_Term (Argument_Context);
      begin
         if Tok = Tok_Comma then
            Scan;
            return Front & Parse_Arguments;
         else
            return (1 => Front);
         end if;
      end Parse_Arguments;

      ------------------------
      -- Parse_Rest_Of_List --
      ------------------------

      function Parse_Rest_Of_List return Machete_Term is
      begin
         if Tok = Tok_Right_Bracket then
            return Empty_List_Term;
         else
            declare
               Head : constant Machete_Term := Parse_Term (Argument_Context);
            begin
               if Tok = Tok_Comma then
                  Scan;
                  declare
                     Tail : constant Machete_Term := Parse_Rest_Of_List;
                  begin
                     return New_Compound_Term (".", (Head, Tail));
                  end;
               elsif Tok = Tok_Right_Bracket then
                  return New_Compound_Term (".", (Head, Empty_List_Term));
               elsif Tok = Tok_Vertical_Bar then
                  Scan;
                  declare
                     Tail : constant Machete_Term :=
                              Parse_Term;
                  begin
                     return New_Compound_Term (".", (Head, Tail));
                  end;
               else
                  Error ("expected ',' or ']'");
                  return New_Compound_Term (".", (Head, Empty_List_Term));
               end if;
            end;
         end if;
      end Parse_Rest_Of_List;

   begin

      Argument_Context.Term_Arguments := True;

      if Tok = Tok_Identifier
        or else Tok = Tok_Dot
        or else Tok = Tok_Character_Literal
      then
         declare
            Name : constant String := Tok_Text;
            Var  : constant Boolean :=
                     Tok = Tok_Identifier
                         and then (Is_Upper (Name (Name'First))
                                   or else Name (Name'First) = '_');
         begin
            Scan;
            if Var then
               return New_Variable_Term (Name);
            else
               if Tok = Tok_Left_Paren then
                  Scan;
                  declare
                     Args : constant Machete.Terms.Array_Of_Terms :=
                              Parse_Arguments;
                  begin
                     if Tok = Tok_Right_Paren then
                        Scan;
                     else
                        Error ("missing ')'");
                     end if;
                     return New_Compound_Term (Name, Args);
                  end;
               else
                  return New_Atom (Name);
               end if;
            end if;
         end;
      elsif Tok = Tok_Integer_Constant then
         declare
            Value : constant Integer := Integer'Value (Tok_Text);
         begin
            Scan;
            return New_Integer_Term (Value);
         end;
      elsif Tok = Tok_Empty_List then
         Scan;
         return Machete.Terms.Empty_List_Term;
      elsif Tok = Tok_Cut then
         Scan;
         return Machete.Terms.New_Atom ("!");
      elsif Tok = Tok_Left_Bracket then
         Scan;
         declare
            List : constant Machete.Terms.Machete_Term := Parse_Rest_Of_List;
         begin
            if Tok = Tok_Right_Bracket then
               Scan;
            else
               Error ("missing ']'");
            end if;
            return List;
         end;
      elsif Tok = Tok_Left_Paren then
         Scan;
         declare
            Inner : constant Machete.Terms.Machete_Term := Parse_Term;
         begin
            if Tok = Tok_Right_Paren then
               Scan;
            else
               Error ("missing ')'");
            end if;
            return Inner;
         end;
      elsif Tok = Tok_Left_Brace then
         Scan;
         declare
            Ts : Term_Vector;
         begin
            while At_Term loop
               Ts.Append (Parse_Term);
               exit when Tok /= Tok_Comma;
               Scan;
               if not At_Term then
                  if Tok = Tok_Right_Brace then
                     Error ("extra ',' ignored");
                  else
                     Error ("expected term");
                  end if;
               end if;
            end loop;

            if Tok = Tok_Right_Brace then
               Scan;
            else
               Error ("missing '}'");
            end if;
            return New_With_Vector ("{}", Ts);
         end;
      else
         Error ("syntax error");
         Scan;
         return New_Atom ("false");
      end if;
   end Parse_Atomic_Term;

   -------------------------
   -- Parse_Operator_Term --
   -------------------------

   function Parse_Operator_Term
     (Context : Parse_Context)
      return Machete.Terms.Machete_Term
   is
      use Machete.Functors;

      function At_Operator return Boolean
      is (Tok = Tok_Identifier
          or else (Tok = Tok_Comma and then not Context.Term_Arguments)
          or else Tok = Tok_Semicolon);

      Argument_Stack : Machete.Terms.Term_Vector;
      Operator_Stack : array (1 .. 20) of Operator_Type := (others => <>);
      Operator_Top   : Natural := 0;

      procedure Pop_Operator;

      procedure Push_Operator
        (Operator : Operator_Type);

      ------------------
      -- Pop_Operator --
      ------------------

      procedure Pop_Operator is
         Operator    : constant Operator_Type :=
                         Operator_Stack (Operator_Top);
         Operator_Name : constant String :=
                           Machete.Functors.Name
                             (Get_Functor (Operator));
         Argument_Count : constant Positive :=
                            (if Is_Infix (Operator)
                             then 2 else 1);
         Arguments     : Machete.Terms.Array_Of_Terms
           (1 .. Argument_Count);
      begin
         if Log_Operators then
            Machete.Logging.Log ("   pop operator: "
                               & Machete.Functors.Image
                                 (Get_Functor (Operator)));
         end if;

         Operator_Top := Operator_Top - 1;

         Arguments (Arguments'Last) := Argument_Stack.Last_Element;
         Argument_Stack.Delete_Last;

         if Is_Infix (Operator) then
            Arguments (Arguments'First) := Argument_Stack.Last_Element;
            Argument_Stack.Delete_Last;
         end if;

         Argument_Stack.Append
           (Machete.Terms.New_Compound_Term
              (Operator_Name, Arguments));
      end Pop_Operator;

      -------------------
      -- Push_Operator --
      -------------------

      procedure Push_Operator
        (Operator : Operator_Type)
      is
      begin
         if Log_Operators then
            Machete.Logging.Log ("push operator: "
                               & Machete.Functors.Image
                                 (Get_Functor (Operator)));
         end if;

         while Operator_Top > 0 loop
            declare
               Top : constant Operator_Type :=
                       Operator_Stack (Operator_Top);
               Pop : Boolean;
            begin
               if Is_Prefix (Operator) then
                  Pop := Priority (Top) >= Priority (Operator);
               else
                  Pop :=
                    (Get_Associativity (Operator) = Left
                     and then Priority (Operator) >= Priority (Top))
                    or else (Get_Associativity (Operator) /= Left
                             and then Priority (Operator) > Priority (Top));
               end if;

               if Pop then
                  Pop_Operator;
               else
                  exit;
               end if;
            end;
         end loop;

         Operator_Top := Operator_Top + 1;
         Operator_Stack (Operator_Top) := Operator;
      end Push_Operator;

   begin

      while At_Operator or else At_Term
        or else (Tok = Tok_Dot and then Next_Tok = Tok_Left_Paren)
      loop

         declare
            use Ada.Characters.Handling;
            Item_Name      : constant String := Tok_Text;
            Is_Variable    : constant Boolean :=
                               Tok = Tok_Identifier
                                   and then
                                     Is_Upper (Item_Name (Item_Name'First));
            Prefix_Functor : constant Machete_Functor :=
                               Get_Functor (Tok_Text, 1);
         begin

            if not Is_Variable
              and then Is_Operator (Prefix_Functor)
              and then (not Context.Term_Arguments
                        or else Item_Name /= ",")
            then
               Push_Operator (Get_Operator (Prefix_Functor));
               Scan;
            else
               Argument_Stack.Append (Parse_Atomic_Term (Context));
               if Log_Operators then
                  Machete.Logging.Log
                    ("argument: "
                     & Machete.Terms.Images.Image
                       (Argument_Stack.Last_Element));
               end if;

               if At_Operator then
                  declare
                     Infix_Functor : constant Machete_Functor :=
                                       Get_Functor (Tok_Text, 2);
                  begin
                     if Is_Operator (Infix_Functor) then
                        Push_Operator (Get_Operator (Infix_Functor));
                        Scan;
                     elsif Is_Term_Token (Next_Tok) then
                        Error ("'" & Tok_Text & "' not declared as operator");
                        Define_Operator (Infix_Functor,
                                         10, Infix, Left);
                        Push_Operator (Get_Operator (Infix_Functor));
                        Scan;
                     else
                        exit;
                     end if;
                  end;
               else
                  exit;
               end if;
            end if;
         end;
      end loop;

      while Operator_Top > 0 loop
         Pop_Operator;
      end loop;

      if Log_Operators then
         Machete.Logging.Log
           ("parse_operator_term: "
            & Machete.Terms.Images.Image (Argument_Stack.First_Element));
      end if;

      return Argument_Stack.First_Element;
   end Parse_Operator_Term;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String
     (Text : String)
      return Machete.Terms.Machete_Term
   is
   begin
      Open_String (Text);
      declare
         Result : constant Machete.Terms.Machete_Term :=
                    Parse_Term;
      begin
         Close;
         return Result;
      end;
   end Parse_String;

   ----------------
   -- Parse_Term --
   ----------------

   function Parse_Term (Context : Parse_Context := (others => <>))
                        return Machete.Terms.Machete_Term is
   begin
      return Parse_Operator_Term (Context);
   end Parse_Term;

end Machete.Parser;
