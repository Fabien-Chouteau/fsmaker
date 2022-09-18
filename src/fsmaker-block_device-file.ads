with System;

with GNAT.OS_Lib;

package FSmaker.Block_Device.File is

   subtype Parent is Block_Device.Instance;
   type Instance (<>) is new Parent with private;

   type Acc is access all Instance;
   type Acc_Any is access all Class;

   function Create (FD               : GNAT.OS_Lib.File_Descriptor;
                    Block_Size       : Positive;
                    Number_Of_Blocks : Positive)
                    return Instance;

   function Create (FD               : GNAT.OS_Lib.File_Descriptor;
                    Block_Size       : Positive;
                    Number_Of_Blocks : Positive)
                    return not null Block_Device.Acc_Any;

private

   type Instance
   is new Parent
   with record
      FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   overriding
   function Read (This            : in out Instance;
                  Block_Id        : Natural;
                  Offset_In_Block : Natural;
                  Buffer          : System.Address;
                  Size            : Natural)
                  return Result;

   overriding
   function Program (This            : in out Instance;
                     Block_Id        : Natural;
                     Offset_In_Block : Natural;
                     Buffer          : System.Address;
                     Size            : Natural)
                     return Result;

   overriding
   function Erase (This     : in out Instance;
                   Block_Id : Natural)
                   return Result;

   overriding
   function Sync (This : in out Instance) return Result;

end FSmaker.Block_Device.File;
