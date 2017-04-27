with Machete.Machine;
with Machete.Options;
with Machete.Parser;
with Machete.Paths;
with Machete.Repl;
with Machete.Tests;

with Machete.Library;

with Machete.Terms.Reports;

procedure Machete.Driver is
begin
   Machete.Options.Load_Options;

   if Machete.Options.Self_Test then
      if True then
         Machete.Tests.Run_Tests
           (Machete.Paths.Config_File ("tests"));
      else
         declare
            Machine : Machete.Machine.Machete_Machine;
         begin
            Machete.Library.Load (Machine);
            Machete.Parser.Load_File
              (Machine,
               Machete.Paths.Config_File
                 ("test-suites/inriasuite/inriasuite.pl"));
         end;
      end if;
   end if;

   if Machete.Options.Interactive then
      declare
         Machine : Machete.Machine.Machete_Machine;
      begin
         Machete.Repl.Execute (Machine);
      end;
   end if;

   Machete.Terms.Reports.Report_Terms;

end Machete.Driver;
