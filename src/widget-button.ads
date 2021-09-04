with Ada.Unchecked_Deallocation;

package Widget.Button is

   subtype Parent is Widget.Instance;

   type Instance is new Parent
   with private;

   subtype Class is Instance'Class;
   type Acc is access all Instance;
   type Any_Acc is access all Class;

   type Const_Acc is access constant Instance;
   type Any_Const_Acc is access constant Class;

   overriding
   procedure Event (This : in out Instance);

   overriding
   procedure Draw (This : in out Instance);

   procedure Free (A : in out Acc);

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent
   with record
      C : Boolean := False;
   end record;

   procedure Private_Free
   is new Ada.Unchecked_Deallocation (Object => Instance,
                                      Name   => Acc);

   procedure Free (A : in out Acc) renames Private_Free;

end Widget.Button;
