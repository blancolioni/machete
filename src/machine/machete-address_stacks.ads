private with Ada.Containers.Doubly_Linked_Lists;

package Machete.Address_Stacks is

   type Address_Stack is tagged private;

   procedure Push
     (Stack : in out Address_Stack'Class;
      Value : Machete_Address);

   function Pop
     (Stack : in out Address_Stack'Class)
      return Machete_Address;

   function Is_Empty
     (Stack : Address_Stack'Class)
      return Boolean;

private

   package Address_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Machete_Address);

   type Address_Stack is tagged
      record
         List : Address_Lists.List;
      end record;

   function Is_Empty
     (Stack : Address_Stack'Class)
      return Boolean
   is (Stack.List.Is_Empty);

end Machete.Address_Stacks;
