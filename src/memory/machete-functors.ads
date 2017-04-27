with Machete.Terms;

package Machete.Functors is

   function Get_Functor
     (Name  : String;
      Arity : Natural)
      return Machete_Functor;

   function Image
     (Functor : Machete_Functor)
      return String;

   function Name
     (Functor : Machete_Functor)
      return String;

   function Arity
     (Functor : Machete_Functor)
      return Natural;

   type Operator_Fixity is (Prefix, Infix, Postfix);
   type Operator_Associativity is (Left, Right, None);

   type Operator_Type is private;

   function Is_Operator
     (Functor : Machete_Functor)
      return Boolean
     with Post => not Is_Operator'Result
     or else Arity (Functor) in 1 .. 2;

   function Get_Operator
     (Functor : Machete_Functor)
      return Operator_Type
     with Pre => Is_Operator (Functor);

   function Get_Functor
     (Operator : Operator_Type)
      return Machete_Functor;

   function Priority
     (Operator : Operator_Type)
      return Natural;

   function Is_Prefix
     (Operator : Operator_Type)
      return Boolean;

   function Is_Infix
     (Operator : Operator_Type)
      return Boolean;

   function Get_Associativity
     (Operator : Operator_Type)
      return Operator_Associativity;

   procedure Define_Operator
     (Functor       : Machete_Functor;
      Priority      : Natural;
      Fixity        : Operator_Fixity;
      Associativity : Operator_Associativity);

   type Predicate_Argument_Interface is interface;

   function Is_Bound
     (Argument : Predicate_Argument_Interface)
      return Boolean
      is abstract;

   function Is_Unbound
     (Argument : Predicate_Argument_Interface'Class)
      return Boolean
   is (not Argument.Is_Bound);

   function Value
     (Argument : Predicate_Argument_Interface)
      return Machete.Terms.Machete_Term
     is abstract
     with Pre'Class => Predicate_Argument_Interface'Class (Argument).Is_Bound;

   procedure Bind_Value
     (Argument : in out Predicate_Argument_Interface;
      Value    : Machete.Terms.Machete_Term)
   is abstract
     with Pre'Class => Argument.Is_Unbound;

   type Predicate_Arguments is
     array (Positive range <>) of access Predicate_Argument_Interface'Class;

   type Predicate_Handler is access
     procedure (Arguments : Predicate_Arguments;
                Success   : out Boolean);

private

   type Operator_Type is
      record
         Functor       : Machete_Functor;
         Priority      : Natural := 0;
         Fixity        : Operator_Fixity;
         Associativity : Operator_Associativity;
      end record;

   function Get_Functor
     (Operator : Operator_Type)
      return Machete_Functor
   is (Operator.Functor);

   function Priority
     (Operator : Operator_Type)
      return Natural
   is (Operator.Priority);

   function Is_Prefix
     (Operator : Operator_Type)
      return Boolean
   is (Operator.Fixity = Prefix);

   function Is_Infix
     (Operator : Operator_Type)
      return Boolean
   is (Operator.Fixity = Infix);

   function Get_Associativity
     (Operator : Operator_Type)
      return Operator_Associativity
   is (Operator.Associativity);

end Machete.Functors;
