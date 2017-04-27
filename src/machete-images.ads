package Machete.Images is

   function Image (Value       : Machete_Word;
                   Digit_Count : Positive := 8)
                   return String;

   function Address_Image
     (Value       : Machete_Address)
      return String;

end Machete.Images;
