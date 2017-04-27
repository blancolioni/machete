with Machete.Compiler;
with Machete.Machine;
with Machete.Terms;

package Machete.Parser is

   procedure Load_File
     (Machine : in out Machete.Machine.Machete_Machine'Class;
      Path    : String);

   procedure Execute_Query
     (Machine     : in out Machete.Machine.Machete_Machine'Class;
      Term        : Machete.Terms.Machete_Term;
      Start       : Code_Address;
      Bindings    : Machete.Compiler.Query_Binding;
      Interactive : Boolean);

   function Parse_String
     (Text : String)
      return Machete.Terms.Machete_Term;

end Machete.Parser;
