with Ada.Text_IO;
with Machete.Images;

package body Machete.Machine.Reports is

   ------------------
   -- Report_State --
   ------------------

   procedure Report_State
     (Machine : Machete.Machine.Machete_Machine'Class)
   is
   begin
      Ada.Text_IO.Put_Line
        ("Code: "
         & Machete.Images.Address_Image
           (Code_Address'First)
         & " .. "
         & Machete.Images.Address_Image
           (Machine.Code_Top));
      Ada.Text_IO.Put_Line
        ("Heap: "
         & Machete.Images.Address_Image
           (Heap_Address'First)
         & " .. "
         & Machete.Images.Address_Image
           (Machine.Rs (H)));
   end Report_State;

end Machete.Machine.Reports;
