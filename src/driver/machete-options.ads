package Machete.Options is

   Interactive       : Boolean := False;

   Exit_Statistics   : Boolean := False;
   Profile           : Boolean := False;
   Self_Test         : Boolean := False;
   Trace_Definitions : Boolean := False;
   Trace_Evaluation  : Boolean := False;
   Trace_GC          : Boolean := False;
   Trace_Patterns    : Boolean := False;
   Trace_Stack       : Boolean := False;

   function Load_File return Boolean;
   function Load_File_Path return String;

   procedure Load_Options;

end Machete.Options;
