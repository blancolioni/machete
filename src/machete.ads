package Machete is

   pragma Pure (Machete);

   Cell_Bits         : constant := 32;
   Word_Size         : constant := Cell_Bits / 8;

   Cell_Payload_Bits : constant := 29;

   type Word_8 is mod 256;
   type Word_16 is mod 65536;
   type Word_32 is mod 2 ** 32;

   subtype Machete_Word is Word_32;

   type Machete_Register is range 0 .. 255;

   subtype General_Purpose_Register is
     Machete_Register range 1 .. 127;

   subtype Permanent_Register is
     Machete_Register range 128 .. 255;

   type Machete_Address is mod 2 ** Cell_Payload_Bits;

   subtype Register_Address is Machete_Address range 4 .. 16#01FF#;
   subtype Code_Address is Machete_Address range 16#1000# .. 16#7FFF#;
   subtype Heap_Address is Machete_Address range 16#1_0000# .. 16#F_FFFF#;
   subtype Stack_Address is Machete_Address range 16#10_0000# .. 16#1F_FFFF#;
   subtype Trail_Address is Machete_Address range 16#20_0000# .. 16#20_FFFF#;

   type Machete_Functor is range 1 .. 65535;

end Machete;
