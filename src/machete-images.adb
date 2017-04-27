package body Machete.Images is

   function Address_Image
     (Value       : Machete_Address)
      return String
   is
   begin
      if Value in Register_Address then
         declare
            X : String := Register_Address'Image (Value / Word_Size);
            S : constant String (1 .. 6 - X'Length) := (others => ' ');
         begin
            X (X'First) := 'X';
            return S & X;
         end;
      else
         return Image (Machete_Word (Value), 6);
      end if;
   end Address_Image;

   -----------
   -- Image --
   -----------

   function Image
     (Value       : Machete_Word;
      Digit_Count : Positive := 8)
      return String
   is
      Ds     : constant String := "0123456789ABCDEF";
      It     : Machete_Word := Value;
      Result : String (1 .. Digit_Count);
   begin
      for Ch of reverse Result loop
         Ch := Ds (Natural (It mod 16) + 1);
         It := It / 16;
      end loop;
      return Result;
   end Image;

end Machete.Images;
