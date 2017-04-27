with Ada.Strings.Fixed;

with Machete.Functors;
with Machete.Images;

package body Machete.Cells is

   -----------
   -- Image --
   -----------

   function Image (Cell : Cell_Type) return String is
--       (Reference_Tag,
--        Structure_Tag,
--        List_Tag,
--        Functor_Tag,
--        Integer_Tag,
--        Tag_6, Tag_7, Tag_8);
   begin
      case Cell.Tag is
         when Uninitialised_Tag =>
            return "*uninitialised*";
         when Reference_Tag =>
            return "@" & Machete.Images.Image (Machete_Word (Cell.Payload));
         when Structure_Tag =>
            return "$" & Machete.Images.Image (Machete_Word (Cell.Payload));
         when List_Tag =>
            return "[" & Machete.Images.Image
              (Machete_Word (Cell.Payload)) & "]";
         when Functor_Tag =>
            if Cell.Payload = 0 then
               return "nil";
            else
               return Machete.Functors.Image (Get_Functor (Cell));
            end if;
         when Integer_Tag =>
            return Ada.Strings.Fixed.Trim
              (Machete_Integer'Image (Get_Integer (Cell)),
               Ada.Strings.Left);
         when Tag_6 =>
            return "[6]" & Machete.Images.Image (Machete_Word (Cell.Payload));
         when Tag_7 =>
            return "[7]" & Machete.Images.Image (Machete_Word (Cell.Payload));
      end case;
   end Image;

end Machete.Cells;
