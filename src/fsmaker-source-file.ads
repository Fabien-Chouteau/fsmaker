private with GNAT.OS_Lib;

package FSmaker.Source.File is

   subtype Parent is Source.Instance;
   type Instance is new Parent with private;

   function Create (Src : String) return Instance;

   procedure Close (This : in out Instance);

private

   type Instance
   is new Parent
   with record
      FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   overriding
   function Read (This : in out Instance;
                  Addr :        System.Address;
                  Len  :        Natural)
                  return Natural;

end FSmaker.Source.File;
