private with Ada.Unchecked_Conversion;

package Machete.Cells is

   type Cell_Type is private;

   subtype Machete_Integer is Integer range
     -2 ** (Cell_Payload_Bits - 1) .. 2 ** (Cell_Payload_Bits - 1) - 1;

   function Is_Reference (Cell : Cell_Type) return Boolean;
   function Is_Structure (Cell : Cell_Type) return Boolean;
   function Is_List (Cell : Cell_Type) return Boolean;
   function Is_Functor (Cell : Cell_Type) return Boolean;
   function Is_Integer (Cell : Cell_Type) return Boolean;
   function Is_Undefined (Cell : Cell_Type) return Boolean;

   function Get_Address (Cell : Cell_Type) return Machete_Address
     with Pre => Is_Reference (Cell) or else Is_Structure (Cell);

   function Get_Integer (Cell : Cell_Type) return Machete_Integer
     with Pre => Is_Integer (Cell);

   function Get_Functor (Cell : Cell_Type) return Machete_Functor
     with Pre => Is_Functor (Cell);

   function To_Reference_Cell (Address : Machete_Address) return Cell_Type;
   function To_Structure_Cell (Address : Machete_Address) return Cell_Type;
   function To_Functor_Cell (Functor : Machete_Functor) return Cell_Type;
   function To_Integer_Cell (Value : Machete_Integer) return Cell_Type;

   function To_Word (Cell : Cell_Type) return Word_32;
   function To_Cell (Word : Word_32) return Cell_Type;

   function Image (Cell : Cell_Type) return String;

private

   type Cell_Tag is
     (Uninitialised_Tag,
      Reference_Tag,
      Structure_Tag,
      List_Tag,
      Functor_Tag,
      Integer_Tag,
      Tag_6, Tag_7);

   type Cell_Payload is mod 2 ** Cell_Payload_Bits;

   type Cell_Type is
      record
         Tag     : Cell_Tag;
         Payload : Cell_Payload;
      end record
     with
       Pack, Size => Cell_Bits;

   function Is_Undefined (Cell : Cell_Type) return Boolean
   is (Cell.Tag = Uninitialised_Tag);

   function Is_Reference (Cell : Cell_Type) return Boolean
   is (Cell.Tag = Reference_Tag);

   function Is_Structure (Cell : Cell_Type) return Boolean
   is (Cell.Tag = Structure_Tag);

   function Is_List (Cell : Cell_Type) return Boolean
   is (Cell.Tag = List_Tag);

   function Is_Functor (Cell : Cell_Type) return Boolean
   is (Cell.Tag = Functor_Tag);

   function Is_Integer (Cell : Cell_Type) return Boolean
   is (Cell.Tag = Integer_Tag);

   function Get_Address (Cell : Cell_Type) return Machete_Address
   is (Machete_Address (Cell.Payload));

   function Get_Integer (Cell : Cell_Type) return Machete_Integer
   is (Machete_Integer (Cell.Payload));

   function Get_Functor (Cell : Cell_Type) return Machete_Functor
   is (Machete_Functor (Cell.Payload));

   function To_Reference_Cell (Address : Machete_Address) return Cell_Type
   is (Reference_Tag, Cell_Payload (Address));

   function To_Structure_Cell (Address : Machete_Address) return Cell_Type
   is (Structure_Tag, Cell_Payload (Address));

   function To_Functor_Cell (Functor : Machete_Functor) return Cell_Type
   is (Functor_Tag, Cell_Payload (Functor));

   function To_Integer_Cell (Value : Machete_Integer) return Cell_Type
   is (Integer_Tag, Cell_Payload (Value));

   function Cell_To_Word is
     new Ada.Unchecked_Conversion (Cell_Type, Word_32);

   function Word_To_Cell is
     new Ada.Unchecked_Conversion (Word_32, Cell_Type);

   function To_Word (Cell : Cell_Type) return Word_32
                     renames Cell_To_Word;

   function To_Cell (Word : Word_32) return Cell_Type
                     renames Word_To_Cell;

end Machete.Cells;
