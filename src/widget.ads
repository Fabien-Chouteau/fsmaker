with Ada.Unchecked_Deallocation;

package Widget is

   type Instance
   is tagged
   private;

   subtype Class is Instance'Class;
   type Acc is access all Instance;
   type Any_Acc is access all Class;

   procedure Event (This : in out Instance) is null;

   procedure Draw (This : in out Instance) is null;

   procedure Free (A : in out Acc);

private

   type Instance
   is tagged
   null record;

   procedure Private_Free
   is new Ada.Unchecked_Deallocation (Object => Instance,
                                      Name   => Acc);

   procedure Free (A : in out Acc) renames Private_Free;

end Widget;
