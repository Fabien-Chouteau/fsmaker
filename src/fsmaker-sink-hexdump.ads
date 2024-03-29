private with System;

package FSmaker.Sink.Hexdump is

   function Create (Dst : not null Sink.Acc_Any) return Sink.Class;

private

   type Instance
   is new Sink.Instance
   with record
      Dst : not null Sink.Acc_Any;
      Addr : Natural := 0;
   end record;

   overriding
   function Write (This : in out Instance;
                   Addr :        System.Address;
                   Len  :        Natural)
                   return Natural;

   overriding
   procedure Close (This : in out Instance);

end FSmaker.Sink.Hexdump;
