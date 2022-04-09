with System;

package FSmaker.Source is

   type Instance is interface;
   subtype Class is Instance'Class;

   type Acc is access all Instance;
   type Acc_Any is access all Class;

   function Read (This : in out Instance;
                  Addr :        System.Address;
                  Len  :        Natural)
                  return Natural
   is abstract;

end FSmaker.Source;
