with Ada.Text_IO;

with Machete.Images;

package body Machete.Logging is

   Log_File : Ada.Text_IO.File_Type;
   Started  : Boolean := False;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      if not Started then
         Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File,
                             "machete.log");
         Started := True;
      end if;
      Ada.Text_IO.Put_Line (Log_File, Message);
      if False then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Address : Machete_Address;
                  Message : String)
   is
   begin
      Log (Machete.Images.Address_Image (Address) & ": "
           & Message);
   end Log;

end Machete.Logging;
