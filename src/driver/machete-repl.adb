with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with Machete.Terms;

with Machete.Compiler;
with Machete.Parser;

package body Machete.Repl is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Machine : in out Machete.Machine.Machete_Machine'Class)
   is
      use Ada.Text_IO;
   begin

      if Ada.Directories.Exists ("auto.pl") then
         Machete.Parser.Load_File
           (Machine, String'("auto.pl"));
      end if;

      while not End_Of_File loop
         Put ("Machete> ");
         Flush;
         declare
            Query_Text : constant String := Get_Line;
         begin
            if Query_Text'Length > 0 then
               declare
                  Term : constant Machete.Terms.Machete_Term :=
                           Machete.Parser.Parse_String (Query_Text);
                  Start : constant Machete_Address := Machine.Code_Top;
                  Bindings : Machete.Compiler.Query_Binding;
               begin
                  Machete.Compiler.Compile_Query
                    (Term, Machine, Bindings);

                  Machete.Parser.Execute_Query
                    (Machine, Term, Start, Bindings,
                     Interactive => True);
               end;
            end if;
         exception
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Put_Line ("Exiting");
   end Execute;

end Machete.Repl;
