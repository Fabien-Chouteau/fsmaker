package body FSmaker.Commands.Mkdir is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (This : in out Instance;
      Args : AAA.Strings.Vector)
   is
   begin
      This.Setup_Image;

      if Args.Count /= 1 then
         This.Failure ("takes exactly one argument");
      elsif not Valid_Target_Path (Args (1)) then
         This.Failure ("Invalid target path: '" & String'(Args (1)) & "'");
      else
         This.Target.Make_Dir (To_Target_Path (Args (1)));
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (This   : in out Instance;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
   begin
      null;
   end Setup_Switches;

end FSmaker.Commands.Mkdir;
