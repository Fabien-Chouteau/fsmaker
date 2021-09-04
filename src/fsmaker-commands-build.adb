with FSmaker.TOML;

package body FSmaker.Commands.Build is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (This : in out Instance;
                      Args : AAA.Strings.Vector)
   is
   begin
      if Args.Count /= 2 then
         This.Failure ("takes exactly two arguments");
      end if;

      FSmaker.TOML.Build_From_TOML (Args (1), Args (2));
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (This   : in out Instance;
      Config : in out CLIC.Subcommander.Switches_Configuration)
   is
   begin
      null;
   end Setup_Switches;

end FSmaker.Commands.Build;
