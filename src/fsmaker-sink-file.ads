with GNAT.OS_Lib;

private with System;

package FSmaker.Sink.File is

   function Create (Path : String) return Sink.Class;

   function Create (FD : GNAT.OS_Lib.File_Descriptor) return Sink.Class;

private

   type Instance
   is new Sink.Instance
   with record
      FD : GNAT.OS_Lib.File_Descriptor;
      Do_Not_Close : Boolean := False;
   end record;

   function Write (This : in out Instance;
                   Addr :        System.Address;
                   Len  :        Natural)
                   return Natural;

   procedure Close (This : in out Instance);

end FSmaker.Sink.File;
