with System;

package FSmaker.Sink is

   type Instance is interface;
   subtype Class is Instance'Class;

   type Acc is access all Instance;
   type Acc_Any is access all Class;

   function Write (This : in out Instance;
                   Addr :        System.Address;
                   Len  :        Natural)
                   return Natural
   is abstract;

   procedure Close (This : in out Instance)
   is abstract;

end FSmaker.Sink;
