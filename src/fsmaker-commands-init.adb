with Simple_Logging;
with Interfaces.C_Streams; use Interfaces.C_Streams;

package body FSmaker.Commands.Init is

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (This : in out Instance;
                      Args :        AAA.Strings.Vector)
   is
      function ftruncate (FS : int;
                          Length : Long_Integer)
                          return int;
      pragma Import (C, ftruncate, "ftruncate");

   begin

      case Args.Count is
         when 0 =>

            This.Setup_Image (To_Format => True);
            if ftruncate (int (This.FD),
                          Long_Integer
                            (This.Block_Size * This.Number_Of_Blocks)) /= 0
            then
               raise Program_Error with "ftruncate error: " &
                 GNAT.OS_Lib.Errno_Message;
            end if;

            Simple_Logging.Always
              ("Format with Block_Size:" & This.Block_Size'Img &
                 " Number_Of_Blocks:" & This.Number_Of_Blocks'Img);

            This.Target.Format (This.BD);
            This.Success;

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
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
   begin
      null;
   end Setup_Switches;

end FSmaker.Commands.Init;
