with FSmaker.Source;
with FSmaker.Sink.File;
with FSmaker.Sink.Hexdump;

package body FSmaker.Commands.Export is

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

      if Args.Count /= 2 then
         This.Failure ("takes exactly two arguments");
      elsif (not Valid_Target_Path (Args (1))) then
         This.Failure ("Invalid target path: '" & String'(Args (1)) & "'");
      else
         declare
            File : Sink.Class := Sink.File.Create (Args (2));
         begin
            This.Target.Cat (To_Target_Path (Args (1)), File);
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

end FSmaker.Commands.Export;
