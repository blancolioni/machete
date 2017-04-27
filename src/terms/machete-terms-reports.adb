with Ada.Text_IO;

with Machete.Logging;

package body Machete.Terms.Reports is

   ------------------
   -- Report_Terms --
   ------------------

   procedure Report_Terms is
      Message : constant String :=
                  "Allocated terms ="
                  & Natural'Image (Allocated_Terms)
                  & "; active terms ="
                  & Natural'Image (Active_Terms);
   begin
      Ada.Text_IO.Put_Line (Message);
      Machete.Logging.Log (Message);
   end Report_Terms;

end Machete.Terms.Reports;
