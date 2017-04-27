with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;

package body Machete.Functors is

   use Ada.Strings.Unbounded;

   type Functor_Record is
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Arity    : Natural;
         Operator : Operator_Type;
      end record;

   package Functor_Vectors is
     new Ada.Containers.Vectors (Machete_Functor, Functor_Record);

   package Functor_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Unbounded_String,
        Element_Type    => Machete_Functor,
        Hash            => Hash,
        Equivalent_Keys => "=");

   Functor_Vector : Functor_Vectors.Vector;
   Functor_Map    : Functor_Maps.Map;

   function To_Key
     (Name  : String;
      Arity : Natural)
      return Unbounded_String
   is (To_Unbounded_String (Name) & Natural'Image (Arity));

   -----------
   -- Arity --
   -----------

   function Arity
     (Functor : Machete_Functor)
      return Natural
   is
   begin
      return Functor_Vector (Functor).Arity;
   end Arity;

   ---------------------
   -- Define_Operator --
   ---------------------

   procedure Define_Operator
     (Functor       : Machete_Functor;
      Priority      : Natural;
      Fixity        : Operator_Fixity;
      Associativity : Operator_Associativity)
   is
      F : Functor_Record renames Functor_Vector (Functor);
   begin
      F.Operator := (Functor, Priority, Fixity, Associativity);
   end Define_Operator;

   -----------------
   -- Get_Functor --
   -----------------

   function Get_Functor
     (Name  : String;
      Arity : Natural)
      return Machete_Functor
   is
      Key : constant Unbounded_String := To_Key (Name, Arity);
   begin
      if not Functor_Map.Contains (Key) then
         Functor_Vector.Append
           ((To_Unbounded_String (Name), Arity, others => <>));
         Functor_Map.Insert
           (Key, Machete_Functor (Functor_Vector.Last_Index));
      end if;
      return Functor_Map (Key);
   end Get_Functor;

   ------------------
   -- Get_Operator --
   ------------------

   function Get_Operator
     (Functor : Machete_Functor)
      return Operator_Type
   is
   begin
      return Functor_Vector (Functor).Operator;
   end Get_Operator;

   -----------
   -- Image --
   -----------

   function Image
     (Functor : Machete_Functor)
      return String
   is
      Rec : Functor_Record renames
              Functor_Vector (Functor);
      Arity_Image : String := Natural'Image (Rec.Arity);
   begin
      Arity_Image (Arity_Image'First) := '/';
      return To_String (Rec.Name) & Arity_Image;
   end Image;

   -----------------
   -- Is_Operator --
   -----------------

   function Is_Operator
     (Functor : Machete_Functor)
      return Boolean
   is
   begin
      return Functor <= Functor_Vector.Last_Index
        and then Functor_Vector (Functor).Operator.Priority > 0;
   end Is_Operator;

   ----------
   -- Name --
   ----------

   function Name
     (Functor : Machete_Functor)
      return String
   is
   begin
      return To_String (Functor_Vector (Functor).Name);
   end Name;

end Machete.Functors;
