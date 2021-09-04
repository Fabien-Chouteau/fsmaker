with System;

private with GNAT.OS_Lib;

package FSmaker.Source is

   type Instance
   is tagged private;


   function Create (Src : String) return Instance;

   function Read (This : in out Instance;
                  Addr :        System.Address;
                  Len  :        Natural)
                  return Natural;

   procedure Close (This : in out Instance);

private

   type Instance
   is tagged record
      FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

end FSmaker.Source;
