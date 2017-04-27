with Ada.Containers.Doubly_Linked_Lists;

with Machete.Functors;
with Machete.Images;
--  with Machete.Logging;

with Machete.Terms.Images;

package body Machete.Memory is

   function Get_Page (Address : Machete_Address) return Page_Index
   is (Page_Index (Address / Page_Size));

   function Get_Offset (Address : Machete_Address) return Page_Offset
   is (Page_Offset (Address mod Page_Size));

   package Address_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Machete_Address);

   ------------
   -- Append --
   ------------

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Word_8)
   is
   begin
      Memory.Set (Address, Value);
      Address := Address + 1;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Word_16)
   is
   begin
      Memory.Set (Address, Value);
      Address := Address + 2;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Word_32)
   is
   begin
      Memory.Set (Address, Value);
      Address := Address + 4;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Machete_Functor)
   is
   begin
      Memory.Set (Address, Value);
      Address := Address + 4;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Machete_Address)
   is
   begin
      Memory.Set_Address (Address, Value);
      Address := Address + Word_Size;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append_Cell
     (Memory  : in out Machete_Memory'Class;
      Address : in out Machete_Address;
      Value   : Machete.Cells.Cell_Type)
   is
   begin
      Memory.Set_Cell (Address, Value);
      Address := Address + Word_Size;
   end Append_Cell;

   -----------------
   -- Dereference --
   -----------------

   function Dereference
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete_Address
   is
      use Machete.Cells;
      Cell : constant Cell_Type := Memory.Get_Cell (Address);
   begin
      if Is_Reference (Cell)
        and then Get_Address (Cell) /= Address
      then
         return Memory.Dereference (Get_Address (Cell));
      else
         return Address;
      end if;
   end Dereference;

   ---------
   -- Get --
   ---------

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Word_8
   is
      Page : constant Page_Index := Get_Page (Address);
   begin
      if Memory.Pages (Page) = null then
         return 0;
      else
         return Memory.Pages (Page) (Get_Offset (Address));
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Word_16
   is
      X : Word_8;
   begin
      return Result : Word_16 := 0 do
         X := Memory.Get (Address);
         Result := Word_16 (X);
         X := Memory.Get (Address + 1);
         Result := Result + 256 * Word_16 (X);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Word_32
   is
      X : Word_8;
   begin
      return Result : Word_32 := 0 do
         for I in reverse Machete_Address range 0 .. 3 loop
            X := Memory.Get (Address + I);
            Result := Result * 256 + Word_32 (X);
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Memory  : Machete_Memory'Class;
      Address : Machete_Address)
      return Machete_Functor
   is
      Result : constant Word_32 := Memory.Get (Address);
   begin
      return Machete_Functor (Result);
   end Get;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Addr : Machete_Address)
      return String
   is
      Ds     : constant String := "0123456789ABCDEF";
      It     : Machete_Address := Addr;
      Result : String (1 .. 6);
   begin
      for Ch of reverse Result loop
         Ch := Ds (Natural (It mod 16) + 1);
         It := It / 16;
      end loop;
      return Result;
   end Hex_Image;

   -----------
   -- Image --
   -----------

   function Image
     (Memory : Machete_Memory'Class;
      Root   : Machete_Address)
      return String
   is
   begin
      return Machete.Terms.Images.Image
        (Memory.Read_Term
           (Memory.Dereference (Root)));
   end Image;

   ---------------
   -- Read_Term --
   ---------------

   function Read_Term
     (Memory        : Machete_Memory'Class;
      Term_Address  : Machete_Address)
      return Machete.Terms.Machete_Term
   is
      use Machete.Cells;
      use Machete.Terms;

      Seen : Address_Lists.List;

      function Read
        (Address : Machete_Address)
         return Machete.Terms.Machete_Term;

      ----------
      -- Read --
      ----------

      function Read
        (Address : Machete_Address)
         return Machete.Terms.Machete_Term
      is
         Deref_Address : constant Machete_Address :=
                           Memory.Dereference (Address);
         Cell          : constant Cell_Type :=
                           Memory.Get_Cell (Deref_Address);
      begin
         if Seen.Contains (Deref_Address) then
            return New_Atom
              ("[loop:"
               & Machete.Images.Address_Image (Deref_Address)
               & "]");
         else
            Seen.Append (Deref_Address);
         end if;

         if Is_Integer (Cell) then
            return New_Integer_Term (Integer (Get_Integer (Cell)));
         elsif Is_Functor (Cell) then
            declare
               Functor   : constant Machete_Functor := Get_Functor (Cell);
               Arguments : Array_Of_Terms
                 (1 .. Machete.Functors.Arity (Functor));
            begin
               for I in Arguments'Range loop
                  Arguments (I) :=
                    Read (Deref_Address + Machete_Address (I) * Word_Size);
               end loop;
               return New_Compound_Term
                 (Machete.Functors.Name (Functor), Arguments);
            end;
         elsif Is_Structure (Cell) then
            return Read (Get_Address (Cell));
         elsif Is_Undefined (Cell) then
            return New_Atom ("*undefined*");
         elsif Is_Reference (Cell)
           and then Get_Address (Cell) = Deref_Address
         then
            declare
               Name : String :=
                        Machete_Address'Image
                          ((Deref_Address - Heap_Address'First) / Word_Size);
            begin
               Name (Name'First) := '_';
               return New_Variable_Term (Name);
            end;
         else
            return New_Atom (Machete.Cells.Image (Cell));
         end if;
      end Read;
   begin
      return Read (Term_Address);
   end Read_Term;

   ---------
   -- Set --
   ---------

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Word_8)
   is
      Page : constant Page_Index := Get_Page (Address);
   begin
      if Memory.Pages = null then
         Memory.Pages := new Page_Table;
      end if;
      if Memory.Pages (Page) = null then
         Memory.Pages (Page) := new Page_Data'(others => 0);
      end if;
      Memory.Pages (Page) (Get_Offset (Address)) := Value;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Word_16)
   is
      X : Word_8;
   begin
      X := Word_8 (Value mod 256);
      Memory.Set (Address, X);
      X := Word_8 (Value / 256);
      Memory.Set (Address + 1, X);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Word_32)
   is
      It : Word_32 := Value;
   begin
      for I in Machete_Address range 0 .. 3 loop
         Memory.Set (Address + I, Word_8 (It mod 256));
         It := It / 256;
      end loop;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Memory      : in out Machete_Memory'Class;
      Address     : Machete_Address;
      Word_Offset : Machete_Address;
      Value       : Word_32)
   is
   begin
      Memory.Set (Address + Word_Offset * Word_Size, Value);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Machete_Functor)
   is
   begin
      Memory.Set (Address, Word_32 (Value));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set_Address
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Machete_Address)
   is
   begin
      Memory.Set (Address, Word_32 (Value));
   end Set_Address;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Memory      : in out Machete_Memory'Class;
      Address     : Machete_Address;
      Word_Offset : Machete_Address;
      Value       : Machete_Address)
   is
   begin
      Memory.Set_Address (Address + Word_Offset * Word_Size, Value);
   end Set_Address;

   ---------
   -- Set --
   ---------

   procedure Set_Cell
     (Memory  : in out Machete_Memory'Class;
      Address : Machete_Address;
      Value   : Machete.Cells.Cell_Type)
   is
   begin
      pragma Assert
        (not Machete.Cells.Is_Structure (Value)
         or else Machete.Functors.Arity
           (Machete.Cells.Get_Functor
                (Memory.Get_Cell (Machete.Cells.Get_Address (Value))))
             > 0);
      Memory.Set (Address, Machete.Cells.To_Word (Value));
--        Machete.Logging.Log
--          ("M: " & Machete.Images.Address_Image (Address) & " <- "
--           & Machete.Cells.Image (Value)
--           & ": " & Memory.Image (Address));
   end Set_Cell;

   ----------------
   -- Write_Term --
   ----------------

   procedure Write_Term
     (Memory   : in out Machete_Memory'Class;
      Address  : in out Machete_Address;
      Term     : Machete.Terms.Machete_Term)
   is
      use Machete.Cells;
      use Machete.Terms;
   begin
      if Is_Variable (Term) then
         Memory.Append_Cell
           (Address, To_Reference_Cell (Address));
      elsif Is_Integer (Term) then
         Memory.Append_Cell
           (Address, To_Integer_Cell (Machete_Integer (Integer_Value (Term))));
      elsif Arity (Term) = 0 then
         Memory.Append_Cell
           (Address, To_Functor_Cell (Functor (Term)));
      else
         declare
            Start : constant Machete_Address := Address;
            Args : array (1 .. Arity (Term)) of Machete_Address;
         begin
            Address := Address + 2 * Word_Size
              + Word_Size * Machete_Address (Args'Length);

            for I in Args'Range loop
               Args (I) := Address;
               Memory.Write_Term (Address, Argument (Term, I));
            end loop;

            for I in Args'Range loop
               Memory.Set_Cell
                 (Start + Word_Size + Machete_Address (I) * Word_Size,
                  To_Reference_Cell (Args (I)));
            end loop;
            Memory.Set_Cell
              (Start + Word_Size, To_Functor_Cell (Functor (Term)));
            Memory.Set_Cell
              (Start, To_Structure_Cell (Start + Word_Size));
         end;
      end if;
   end Write_Term;

end Machete.Memory;
