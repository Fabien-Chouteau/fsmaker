with FSmaker.Source;
with FSmaker.Source.File;

package body FSmaker.Commands.Import is

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

      if Args.Count /= 2 then
         This.Failure ("takes exactly two arguments");
      elsif not Valid_Target_Path (Args (1)) then
         This.Failure ("Invalid target path: '" & String'(Args (1)) & "'");
      else
         declare
            Src : FSmaker.Source.File.Instance :=
              FSmaker.Source.File.Create (Args (2));
         begin
            This.Target.Import (To_Target_Path (Args (1)), Src);
            Src.Close;
         end;
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

end FSmaker.Commands.Import;
