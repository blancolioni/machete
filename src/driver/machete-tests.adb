with Ada.Directories;

with Machete.Machine;
with Machete.Library;
with Machete.Parser;

with Machete.Machine.Reports;

package body Machete.Tests is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests
     (Folder : String)
   is
      Machine : Machete.Machine.Machete_Machine;

      procedure Run_Test
        (Item : Ada.Directories.Directory_Entry_Type);

      --------------
      -- Run_Test --
      --------------

      procedure Run_Test
        (Item : Ada.Directories.Directory_Entry_Type)
      is
         Path : constant String :=
                  Ada.Directories.Full_Name (Item);
      begin
         Machete.Parser.Load_File
           (Machine, Path);
      end Run_Test;

   begin
      Machete.Library.Load (Machine);
      Ada.Directories.Search
        (Directory => Folder,
         Pattern   => "*.pl",
         Process   => Run_Test'Access);
      Machete.Machine.Reports.Report_State (Machine);
   end Run_Tests;

end Machete.Tests;
