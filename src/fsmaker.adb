with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Simple_Logging;
with GNAT.OS_Lib;

package body FSmaker is

   --------------------
   -- To_Target_Path --
   --------------------

   function To_Target_Path (Str : String) return Target_Path is
   begin
      if Str'Length = 0 then
         return Empty_Path;
      elsif  Str (Str'First) /= '/' then
         raise Program_Error with "Invalid target path";
         return Empty_Path;
      else
         return AAA.Strings.Split (Str (Str'First + 1 .. Str'Last), '/');
      end if;
   end To_Target_Path;

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print (Tree   : Directory_Tree;
                           Indent : String := "|")
   is
   begin
      for Elt of Tree loop
         Simple_Logging.Always (Indent & "-" & To_String (Elt.Name));
         if Elt.Kind = Dir then
            Pretty_Print (Elt.Entries, Indent & "-|");
         end if;
      end loop;
   end Pretty_Print;


end FSmaker;
