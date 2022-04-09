with GNAT.OS_Lib;

with FSmaker.Source;
with FSmaker.Sink;
with FSmaker.Block_Device;

package FSmaker.Target is

   type Filesystem is interface;
   type Any_Filesystem_Ref is access all Filesystem'Class;

   procedure Format (This             : in out   Filesystem;
                     BD               : not null Block_Device.Acc_Any)
   is abstract;

   procedure Mount (This : in out   Filesystem;
                    BD   : not null Block_Device.Acc_Any)
   is abstract;

   procedure Make_Dir (This      : in out Filesystem;
                       Path      :        Target_Path)
   is abstract;

   function Tree (This      : in out Filesystem;
                  Path      :        Target_Path)
                  return Directory_Tree
                  is abstract;

   procedure Import (This      : in out Filesystem;
                     Path      :        Target_Path;
                     Src       : in out Source.Class)
   is abstract;

   procedure Cat (This : in out Filesystem;
                  Path :        Target_Path;
                  Dst  : in out FSmaker.Sink.Class)
   is abstract;

end FSmaker.Target;
