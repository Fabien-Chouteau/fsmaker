with Simple_Logging;

package body FSmaker.Commands.Init is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (This : in out Instance;
                      Args :        AAA.Strings.Vector)
   is
   begin

      This.Setup_Image (To_Format => True);

      case Args.Count is
         when 0 =>
            This.Failure ("Missing argument <size>");
         when 1 =>
            declare
               Arg : constant String := Args (1);
               Size : Natural;
            begin
               Size := Natural'Value (Args (1));

               Simple_Logging.Always ("Format with Size:" & Size'Img);

               This.Target.Format (This.FD, Size);
               This.Success;
            exception
               when Constraint_Error =>
                  This.Failure ("Invalid size argument: '" & Arg & "'");
            end;
         when others =>
            This.Failure ("Too many arguments");
      end case;
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

end FSmaker.Commands.Init;
