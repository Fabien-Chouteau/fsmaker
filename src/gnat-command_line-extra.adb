with Ada.Containers.Indefinite_Ordered_Sets;
with GNAT.Strings;
with Simple_Logging;

package body GNAT.Command_Line.Extra is

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   subtype String_Set is String_Sets.Set;
   Empty_Set : String_Set renames String_Sets.Empty_Set;

   function Verify_No_Duplicates (Config : Command_Line_Configuration)
                                  return Boolean
   is
      Seen : String_Sets.Set;
      --  We track already set switches in this set; any re-appearance is
      --  reported.

      function Insert (Switch : String) return Boolean is
         --  Return True if OK; False otherwise.
      begin
         Seen.Insert (Switch);
         --  Raises Constraint_Error when element already exists.

         return True;
      exception
         when Constraint_Error =>
            Simple_Logging.Error ("Redefined switch: " & Switch);
            return False;
      end Insert;

      use all type GNAT.Strings.String_Access;
   begin
      for Switch of Config.Switches.all loop
         --  Short version
         if Switch.Switch /= null and then
           Switch.Switch.all /= "" and then
           not Insert (Switch.Switch.all)
         then
            return False;
         end if;
         --  Long version
         if Switch.Long_Switch /= null and then
           Switch.Long_Switch.all /= "" and then
           not Insert (Switch.Long_Switch.all)
         then
            return False;
         end if;
      end loop;

      --  No duplication detected
      return True;
   end Verify_No_Duplicates;

   procedure For_Each_Switch
     (Config : Command_Line_Configuration;
      Callback : not null access procedure
        (Switch, Long_Switch, Argument, Help : String))
   is
      use all type GNAT.Strings.String_Access;
   begin
      for S of Config.Switches.all loop
         Callback
           ((if S.Switch /= null then S.Switch.all else ""),
            (if S.Long_Switch /= null then S.Long_Switch.all else ""),
            (if S.Argument /= null then S.Argument.all else "ARG"),
            (if S.Help /= null then S.Help.all else ""));
      end loop;
   end For_Each_Switch;

end GNAT.Command_Line.Extra;
