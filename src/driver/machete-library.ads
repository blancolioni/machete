with Machete.Machine;
with Machete.Terms;

package Machete.Library is

   procedure Load
     (Machine : in out Machete.Machine.Machete_Machine'Class);

   procedure Define_Operator
     (Name      : String;
      Arity     : Positive;
      Priority  : Positive;
      Specifier : String);

   procedure Execute
     (Machine : in out Machete.Machine.Machete_Machine'Class;
      Query   : String;
      Result  : out Machete.Terms.Term_Vector);

end Machete.Library;
