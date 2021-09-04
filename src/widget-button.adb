pragma Ada_2012;
package body Widget.Button is

   -----------
   -- Event --
   -----------

   overriding
   procedure Event (This : in out Instance) is
   begin
      Parent (This).Event;
   end Event;

   ----------
   -- Draw --
   ----------

   overriding
   procedure Draw (This : in out Instance) is
   begin
      Dispatch (This).Event;
   end Draw;

end Widget.Button;
