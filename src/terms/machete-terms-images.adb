with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Machete.Functors;

package body Machete.Terms.Images is

   -----------
   -- Image --
   -----------

   function Image
     (Term           : Machete_Term;
      With_Operators : Boolean := True)
      return String
   is
      function Safe_Atom_Image
        (Atom : String)
         return String;

      function Safe_Atom_Image
        (Atom : String)
         return String
      is
         use Ada.Strings.Fixed;
         Quote        : Boolean := False;
         First        : Boolean := True;
         Variable     : Boolean := False;
         Alphanumeric : Boolean := False;
         Start_Number : Boolean := False;
         Symbolic     : Boolean := False;
         Reserved     : Boolean := False;
      begin
         if Atom = "" then
            return "''";
         elsif Atom = "[]" then
            return Atom;
         end if;

         Start_Number := Atom (Atom'First) in '0' .. '9';

         for Ch of Atom loop
            if Ch in 'A' .. 'Z'
              or else Ch = '_'
            then
               if First then
                  Variable := True;
               end if;
               Alphanumeric := True;
            elsif Ch in 'a' .. 'z' then
               Alphanumeric := True;
            elsif Ch in '0' .. '9' then
               null;
            elsif Index ("#$&*+-./:<=>?@^~\", (1 => Ch)) > 0 then
               Symbolic := True;
            else
               Reserved := True;
            end if;
            First := False;
         end loop;

         if Reserved or else Variable
           or else (Symbolic and then Alphanumeric)
           or else (Start_Number and then (Symbolic or else Alphanumeric))
         then
            Quote := True;
         end if;

         if Quote then
            return "'" & Atom & "'";
         else
            return Atom;
         end if;
      end Safe_Atom_Image;

   begin
      if Is_Variable (Term) then
         return Variable_Name (Term);
      elsif Arity (Term) = 0 then
         return Safe_Atom_Image (Atom (Term));
      else
         declare
            use Ada.Strings.Unbounded;
            Arguments : Unbounded_String;
         begin
            if Atom (Term) = "." then
               declare
                  It : Machete_Term := Term;
               begin
                  while Is_Compound_Term (It)
                    and then Atom (It) = "."
                  loop
                     if Arguments = "" then
                        Arguments := To_Unbounded_String ("[");
                     else
                        Append (Arguments, ",");
                     end if;
                     Append
                       (Arguments,
                        Image (Argument (It, 1),
                          With_Operators));
                     It := Argument (It, 2);
                  end loop;
                  if Is_Atom (It) and then Atom (It) = "[]" then
                     return To_String (Arguments) & "]";
                  else
                     return To_String (Arguments) & "|"
                       & Image (It) & "]";
                  end if;
               end;
            elsif With_Operators
              and then Machete.Functors.Is_Operator (Functor (Term))
            then
               declare
                  use Machete.Functors;
                  Op : constant Operator_Type :=
                         Get_Operator (Functor (Term));
               begin
                  if Is_Prefix (Op) then
                     return Safe_Atom_Image (Atom (Term))
                       & Image (Argument (Term, 1));
                  else
                     return Image (Argument (Term, 1))
                       & Safe_Atom_Image (Atom (Term))
                       & Image (Argument (Term, 2));
                  end if;
               end;
            else
               for I in 1 .. Arity (Term) loop
                  if I > 1 then
                     Append (Arguments, ",");
                  end if;
                  Append (Arguments,
                          Image (Argument (Term, I), With_Operators));
               end loop;
               return Safe_Atom_Image (Atom (Term))
                 & "(" & To_String (Arguments) & ")";
            end if;
         end;
      end if;
   end Image;

end Machete.Terms.Images;
