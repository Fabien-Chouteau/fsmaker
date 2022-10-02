with GNAT.OS_Lib;
with System.Storage_Elements;

private with Littlefs;

package FSmaker.Target.LittleFS is

   subtype Parent is Filesystem;

   type Instance (Block_Size : System.Storage_Elements.Storage_Count)
   is new Parent
   with private;

   overriding
   procedure Format (This : in out   Instance;
                     BD   : not null Block_Device.Acc_Any);

   overriding
   procedure Mount (This : in out Instance;
                    BD   : not null Block_Device.Acc_Any);

   overriding
   procedure Make_Dir (This : in out Instance;
                       Path :        Target_Path);

   overriding
   function Tree (This : in out Instance;
                  Path :        Target_Path)
                  return Directory_Tree;

   overriding
   procedure Import (This : in out Instance;
                     Path :        Target_Path;
                     Src  : in out Source.Class);

   overriding
   procedure Cat (This : in out Instance;
                  Path :        Target_Path;
                  Dst  : in out FSmaker.Sink.Class);

private

   type LFS_Config_Access is access all Standard.Littlefs.LFS_Config;

   type Instance (Block_Size : System.Storage_Elements.Storage_Count)
   is new Parent
   with record
      FD  : aliased GNAT.OS_Lib.File_Descriptor;
      LFS : aliased Standard.Littlefs.LFS_T;
      Config : LFS_Config_Access;
      Read_Buffer : System.Storage_Elements.Storage_Array (1 .. Block_Size);
      Prog_Buffer : System.Storage_Elements.Storage_Array (1 .. Block_Size);
      Lookahead_Buffer : System.Storage_Elements.Storage_Array
        (1 .. Block_Size);
   end record;

end FSmaker.Target.LittleFS;
