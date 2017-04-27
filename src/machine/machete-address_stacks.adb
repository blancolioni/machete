package body Machete.Address_Stacks is

   ---------
   -- Pop --
   ---------

   function Pop
     (Stack : in out Address_Stack'Class)
      return Machete_Address
   is
   begin
      return Address : constant Machete_Address := Stack.List.Last_Element do
         Stack.List.Delete_Last;
      end return;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (Stack : in out Address_Stack'Class;
      Value : Machete_Address)
   is
   begin
      Stack.List.Append (Value);
   end Push;

end Machete.Address_Stacks;
