with System;

package FSmaker.Block_Device is

   type Instance (Block_Size, Number_Of_Blocks : Positive)
   is abstract tagged private;

   subtype Class is Instance'Class;

   type Acc is access all Instance;
   type Acc_Any is access all Class;

   type Result is (Ok, Error);

   function Read (This            : in out Instance;
                  Block_Id        : Natural;
                  Offset_In_Block : Natural;
                  Buffer          : System.Address;
                  Size            : Natural)
                  return Result
   is abstract;

   function Program (This            : in out Instance;
                     Block_Id        : Natural;
                     Offset_In_Block : Natural;
                     Buffer          : System.Address;
                     Size            : Natural)
                     return Result
   is abstract;

   function Erase (This     : in out Instance;
                   Block_Id : Natural)
                   return Result
   is abstract;

   function Sync (This : in out Instance) return Result
   is abstract;

private

   type Instance (Block_Size, Number_Of_Blocks : Positive)
   is abstract tagged null record;

end FSmaker.Block_Device;
