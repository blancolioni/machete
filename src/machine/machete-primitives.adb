with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Machete.Functors;
with Machete.Memory;
with Machete.Terms;
with Machete.Terms.Images;

with Machete.Library;

package body Machete.Primitives is

   procedure Handle_Atom_Length
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   procedure Handle_Atom_Chars
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   procedure Handle_Fail
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   procedure Handle_NL
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   procedure Handle_Op
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   procedure Handle_True
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   procedure Handle_Write
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean);

   -----------------------
   -- Define_Primitives --
   -----------------------

   procedure Define_Primitives
     (Machine : in out Machete.Machine.Machete_Machine'Class)
   is
   begin
      Machine.Define
        (Machete.Functors.Get_Functor ("-min-atom_chars", 2),
         Handle_Atom_Chars'Access);
      Machine.Define
        (Machete.Functors.Get_Functor ("atom_length", 2),
         Handle_Atom_Length'Access);
      Machine.Define
        (Machete.Functors.Get_Functor ("fail", 0),
         Handle_Fail'Access);
      Machine.Define
        (Machete.Functors.Get_Functor ("nl", 0),
         Handle_NL'Access);
      Machine.Define
        (Machete.Functors.Get_Functor ("op", 3),
         Handle_Op'Access);
      Machine.Define
        (Machete.Functors.Get_Functor ("true", 0),
         Handle_True'Access);
      Machine.Define
        (Machete.Functors.Get_Functor ("write", 1),
         Handle_Write'Access);
   end Define_Primitives;

   -----------------------
   -- Handle_Atom_Chars --
   -----------------------

   procedure Handle_Atom_Chars
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
      use Ada.Strings.Unbounded;
      Chars     : Unbounded_String;
   begin
      if not Arguments (1).Is_Bound and then not Arguments (2).Is_Bound then
         raise Constraint_Error with
           "atom_chars: at least one argument must be bound";
      elsif Arguments (2).Is_Bound then
         declare
            use Machete.Terms;
            It : Machete_Term := Arguments (2).Value;
            Good_Element : Boolean := True;
         begin
            while Atom (It) = "." loop
               if Arity (It) /= 2
                 or else not Is_Atom (Argument (It, 1))
               then
                  Good_Element := False;
               else
                  declare
                     Head : constant String := Atom (Argument (It, 1));
                  begin
                     if Head'Length /= 1 then
                        Good_Element := False;
                     else
                        Chars := Chars & Head;
                     end if;
                  end;
               end if;

               exit when not Good_Element;
               It := Argument (It, 2);
            end loop;

            if not Good_Element
              or else Is_Variable (It)
              or else Atom (It) /= "[]"
            then
               raise Constraint_Error with
                 "atom_chars: second argument must be unbound "
                 & "or list of characters";
            end if;

         end;
      end if;

      if Arguments (1).Is_Bound then
         if not Machete.Terms.Is_Atom (Arguments (1).Value) then
            raise Constraint_Error with
              "atom_chars: first argument must be an atom";
         elsif Arguments (2).Is_Bound then
            Success := Chars = Machete.Terms.Atom (Arguments (1).Value);
         else
            declare
               use Machete.Terms;
               T : Machete_Term := Empty_List_Term;
            begin
               for Ch of reverse Atom (Arguments (1).Value) loop
                  T := New_Compound_Term
                    (".", (New_Atom ((1 => Ch)), T));
               end loop;
               Arguments (2).Bind_Value (T);
               Success := True;
            end;
         end if;
      else
         Arguments (1).Bind_Value (Machete.Terms.New_Atom (To_String (Chars)));
         Success := True;
      end if;

   end Handle_Atom_Chars;

   ------------------------
   -- Handle_Atom_Length --
   ------------------------

   procedure Handle_Atom_Length
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
      Length    : Natural;
      Atom_Term : Machete.Terms.Machete_Term;
   begin
      if not Arguments (1).Is_Bound then
         Success := False;
         return;
      end if;

      Atom_Term := Arguments (1).Value;

      if Machete.Terms.Is_Variable (Atom_Term)
        or else Machete.Terms.Arity (Atom_Term) /= 0
      then
         Success := False;
         return;
      end if;

      Length := Machete.Terms.Atom (Atom_Term)'Length;

      if Arguments (2).Is_Unbound then
         Arguments (2).Bind_Value
           (Machete.Terms.New_Integer_Term (Length));
         Success := True;
      else
         declare
            Length_Term : constant Machete.Terms.Machete_Term :=
                            Arguments (2).Value;
         begin
            Success := Machete.Terms.Is_Integer (Length_Term)
              and then Machete.Terms.Integer_Value (Length_Term) = Length;
         end;
      end if;

   end Handle_Atom_Length;

   -----------------
   -- Handle_Fail --
   -----------------

   procedure Handle_Fail
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
      pragma Unreferenced (Arguments);
   begin
      Success := False;
   end Handle_Fail;

   ---------------
   -- Handle_NL --
   ---------------

   procedure Handle_NL
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
      pragma Unreferenced (Arguments);
   begin
      Ada.Text_IO.New_Line;
      Success := True;
   end Handle_NL;

   procedure Handle_Op
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
      use Machete.Functors;
      Priority  : constant access Predicate_Argument_Interface'Class :=
                    Arguments (1);
      Specifier : constant access Predicate_Argument_Interface'Class :=
                    Arguments (2);
      Name      : constant access Predicate_Argument_Interface'Class :=
                    Arguments (3);
   begin
      if not Priority.Is_Bound
        or else not Specifier.Is_Bound
        or else not Name.Is_Bound
        or else not Machete.Terms.Is_Integer (Priority.Value)
        or else not Machete.Terms.Is_Atom (Specifier.Value)
        or else not Machete.Terms.Is_Atom (Name.Value)
      then
         Success := False;
         return;
      end if;

      Machete.Library.Define_Operator
        (Name      => Machete.Terms.Atom (Name.Value),
         Arity     => 2,
         Priority  => Machete.Terms.Integer_Value (Priority.Value),
         Specifier => Machete.Terms.Atom (Specifier.Value));
      Success := True;
   exception
      when others =>
         Success := False;

   end Handle_Op;

   -----------------
   -- Handle_True --
   -----------------

   procedure Handle_True
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
      pragma Unreferenced (Arguments);
   begin
      Success := True;
   end Handle_True;

   ------------------
   -- Handle_Write --
   ------------------

   procedure Handle_Write
     (Arguments : Machete.Functors.Predicate_Arguments;
      Success   : out Boolean)
   is
   begin
      if not Arguments (1).Is_Bound then
         Ada.Text_IO.Put ("_");
      else
         Ada.Text_IO.Put
           (Machete.Terms.Images.Image
              (Arguments (1).Value));
      end if;
      Success := True;
   end Handle_Write;

end Machete.Primitives;
