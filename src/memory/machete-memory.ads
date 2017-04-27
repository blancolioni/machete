with Machete.Cells;
with Machete.Terms;

package Machete.Memory is

   type Machete_Memory is tagged limited private;

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Word_8;

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Word_16;

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Word_32;

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete_Functor;

   function Get_Cell
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete.Cells.Cell_Type;

   function Get_Address
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete_Address;

   function Get_Address
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address;
      Word_Offset : Machete_Address)
      return Machete_Address
   is (Memory.Get_Address (Address + Word_Size * Word_Offset));

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Word_8);

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Word_16);

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Word_32);

   procedure Set
     (Memory      : in out Machete_Memory'Class;
      Address     : Machete_Address;
      Word_Offset : Machete_Address;
      Value       : Word_32);

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Machete_Functor);

   procedure Set_Address
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Machete_Address);

   procedure Set_Address
     (Memory      : in out Machete_Memory'Class;
      Address     : Machete_Address;
      Word_Offset : Machete_Address;
      Value       : Machete_Address);

   procedure Set_Cell
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Machete.Cells.Cell_Type);

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Word_8);

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Word_16);

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Word_32);

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Machete_Functor);

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Machete_Address);

   procedure Append_Cell
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Machete.Cells.Cell_Type);

   function Dereference
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete_Address;

   function Image
     (Memory   : Machete_Memory'Class;
      Root     : Machete_Address)
      return String;

   procedure Write_Term
     (Memory   : in out Machete_Memory'Class;
      Address  : in out Machete_Address;
      Term     : Machete.Terms.Machete_Term);

   function Read_Term
     (Memory       : Machete_Memory'Class;
      Term_Address : Machete_Address)
      return Machete.Terms.Machete_Term;

private

   Page_Size_Bits : constant := 12;
   Page_Size      : constant := 2 ** Page_Size_Bits;

   type Page_Offset is mod Page_Size;

   type Page_Data is array (Page_Offset) of Word_8;

   type Page_Access is access Page_Data;

   type Page_Index is mod 2 ** (Cell_Payload_Bits - Page_Size_Bits);

   type Page_Table is array (Page_Index) of Page_Access;

   type Machete_Memory is tagged limited
      record
         Pages : access Page_Table;
      end record;

   function Get_Address
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete_Address
   is (Machete_Address (Word_32'(Memory.Get (Address))));

   function Get_Cell
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete.Cells.Cell_Type
   is (Machete.Cells.To_Cell (Memory.Get (Address)));

   function Hex_Image
     (Addr : Machete_Address)
      return String;

end Machete.Memory;
