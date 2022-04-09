package body FSmaker.Commands.Tree is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute
     (This : in out Instance;
      Args : AAA.Strings.Vector)
   is
      Dir : Directory_Tree;
   begin
      This.Setup_Image;

      if Args.Count = 0 then
         Dir := This.Target.Tree (Empty_Path);
         Pretty_Print (Dir);
      elsif Args.Count /= 1 then
         This.Failure ("takes one or no argument");
      elsif not Valid_Target_Path (Args (1)) then
         This.Failure ("Invalid target path: '" & String'(Args (1)) & "'");
      else
         Dir := This.Target.Tree (To_Target_Path (Args (1)));
         Pretty_Print (Dir);
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

end FSmaker.Commands.Tree;
