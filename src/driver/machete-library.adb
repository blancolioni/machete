with WL.String_Maps;

with Machete.Compiler;
with Machete.Functors;
with Machete.Parser;
with Machete.Primitives;

with Machete.Paths;

package body Machete.Library is

   type Specifier_Record is
      record
         Fixity : Machete.Functors.Operator_Fixity;
         Associativity : Machete.Functors.Operator_Associativity;
      end record;

   package Specifier_Maps is
     new WL.String_Maps (Specifier_Record);

   Specifiers : Specifier_Maps.Map;

   procedure Specifier
     (Form          : String;
      Fixity        : Machete.Functors.Operator_Fixity;
      Associativity : Machete.Functors.Operator_Associativity);

   ---------------------
   -- Define_Operator --
   ---------------------

   procedure Define_Operator
     (Name      : String;
      Arity     : Positive;
      Priority  : Positive;
      Specifier : String)
   is
   begin
      if not Specifiers.Contains (Specifier) then
         raise Constraint_Error with
           "bad fixity specifier: " & Specifier;
      end if;

      Machete.Functors.Define_Operator
        (Functor       => Machete.Functors.Get_Functor (Name, Arity),
         Priority      => Priority,
         Fixity        => Specifiers (Specifier).Fixity,
         Associativity => Specifiers (Specifier).Associativity);

   end Define_Operator;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Machine : in out Machete.Machine.Machete_Machine'Class;
      Query   : String;
      Result  : out Machete.Terms.Term_Vector)
   is
      Term : constant Machete.Terms.Machete_Term :=
               Machete.Parser.Parse_String (Query);
      Term_Vs  : constant Machete.Terms.Array_Of_Terms :=
                   Machete.Terms.Term_Variables (Term);
      Start    : constant Machete_Address := Machine.Code_Top;
      Bindings : Machete.Compiler.Query_Binding;
      Success  : Boolean;

      function Continue return Boolean;

      --------------
      -- Continue --
      --------------

      function Continue return Boolean is
      begin
         for V of Term_Vs loop
            declare
               Name     : constant String :=
                            Machete.Terms.Variable_Name (V);
               Register : constant Machete_Register :=
                            Machete.Compiler.Get_Register_Binding
                              (Bindings, Machete.Terms.Variable_Name (V));
            begin
               if Name (Name'First) /= '_' then
                  Result.Append
                    (Machine.Memory.Read_Term
                       (Machine.Get_Register_Address (Register)));
               end if;
            end;
         end loop;

         return False;
      end Continue;

   begin
      Machete.Compiler.Compile_Query
        (Query_Term => Term,
         Machine    => Machine,
         Bindings   => Bindings);
      Machine.Stop;

      Machine.Execute (Start, Continue'Access, Success);

   end Execute;

   ----------
   -- Load --
   ----------

   procedure Load
     (Machine : in out Machete.Machine.Machete_Machine'Class)
   is
      use all type Machete.Functors.Operator_Fixity;
      use all type Machete.Functors.Operator_Associativity;
   begin
      Machete.Primitives.Define_Primitives (Machine);

      Specifier ("xfx", Infix, None);
      Specifier ("fx", Prefix, None);
      Specifier ("xfy", Infix, Right);
      Specifier ("yfx", Infix, Left);

      Define_Operator (":-", 2, 1200, "xfx");
      Define_Operator ("-->", 2, 1200, "xfx");
      Define_Operator (":-", 1, 1200, "fx");
      Define_Operator ("?-", 1, 1200, "fx");
      Define_Operator (";", 2, 1100, "xfy");
      Define_Operator ("->", 2, 1050, "xfy");
      Define_Operator (",", 2, 1000, "xfy");
      Define_Operator ("=", 2, 700, "xfx");
      Define_Operator (">=", 2, 700, "xfx");
      Define_Operator ("=<", 2, 700, "xfx");
      Define_Operator ("==", 2, 700, "xfx");
      Define_Operator ("=\=", 2, 700, "xfx");
      Define_Operator ("\==", 2, 700, "xfx");
      Define_Operator ("=..", 2, 700, "xfx");
      Define_Operator ("+", 2, 500, "yfx");
      Define_Operator ("-", 2, 500, "yfx");
      Define_Operator ("*", 2, 400, "yfx");
      Define_Operator ("/", 2, 400, "yfx");
      Define_Operator ("//", 2, 400, "yfx");
      Define_Operator ("rem", 2, 400, "yfx");
      Define_Operator ("mod", 2, 400, "yfx");
      Define_Operator ("is", 2, 800, "xfx");

      Machete.Parser.Load_File
        (Machine => Machine,
         Path    => Machete.Paths.Config_File ("libraries/standard.pl"));

   end Load;

   ---------------
   -- Specifier --
   ---------------

   procedure Specifier
     (Form          : String;
      Fixity        : Machete.Functors.Operator_Fixity;
      Associativity : Machete.Functors.Operator_Associativity)
   is
   begin
      Specifiers.Insert (Form, (Fixity, Associativity));
   end Specifier;

end Machete.Library;
