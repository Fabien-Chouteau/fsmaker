with System;
with FSmaker.Sink;
with FSmaker.Source;

private with System.Storage_Elements;

package FSmaker.Block_Device.RAM is

   subtype Parent is Block_Device.Instance;
   type Instance (Block_Size, Number_Of_Blocks : Positive)
   is new Parent
   with private;

   type Acc is access all Instance;
   type Acc_Any is access all Class;

   procedure Write_Data (This :        Instance;
                         Dst  : in out FSmaker.Sink.Class);
   --  Write the entire content of the block device data in Dst

   procedure Read_Data (This :        Instance;
                        Src  : in out FSmaker.Source.Class);
   --  Read the entire content of the block device data from Src

private

   type Data_Array is array (Positive range <>, Positive range <>)
     of System.Storage_Elements.Storage_Element;

   type Instance (Block_Size, Number_Of_Blocks : Positive)
   is new Parent (Block_Size       => Block_Size,
                  Number_Of_Blocks => Number_Of_Blocks)
   with record
      Data : Data_Array (1 .. Number_Of_Blocks, 1 .. Block_Size);
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

end FSmaker.Block_Device.RAM;
