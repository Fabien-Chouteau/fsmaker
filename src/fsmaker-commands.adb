with Simple_Logging; use Simple_Logging;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with CLIC.User_Input;
with CLIC.TTY;

with FSmaker.Commands.Init;
with FSmaker.Commands.Mkdir;
with FSmaker.Commands.Tree;
with FSmaker.Commands.Import;
with FSmaker.Commands.Cat;
with FSmaker.Commands.Hexdump;
with FSmaker.Commands.Export;
with FSmaker.Commands.Build;

with FSmaker.Target.LittleFS;

package body FSmaker.Commands is

   subtype Dispatch is Command'Class;

   No_Color : aliased Boolean := False;
   --  Force-disable color output

   No_TTY : aliased Boolean := False;
   --  Used to disable control characters in output

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommander.Switches_Configuration)
   is
      use CLIC.Subcommander;
   begin
      Define_Switch (Config,
                     Sw_Format'Access,
                     "-f=", "--format=",
                     "Filesystem format (only 'littlefs' so far)");

      Define_Switch (Config,
                     Sw_Image'Access,
                     "-i=", "--img=",
                     "Path to the image file to create/modify");

      Define_Switch (Config,
                     "-h", "--help",
                     "Display general or command-specific help");

      Define_Switch (Config,
                     Sw_Verbose'Access,
                     "-v",
                     Help => "Be more verbose (use twice for extra detail)");

      Define_Switch (Config,
                     Sw_Debug'Access,
                     "-d",
                     Long_Switch => "--debug",
                     Help        => "Enable debug-specific log messages");


      Define_Switch (Config,
                     CLIC.User_Input.Not_Interactive'Access,
                     "-n", "--non-interactive",
                     "Assume default answers for all user prompts");

      Define_Switch (Config,
                     No_Color'Access,
                     Long_Switch => "--no-color",
                     Help        => "Disables colors in output");

      Define_Switch (Config,
                     No_TTY'Access,
                     Long_Switch => "--no-tty",
                     Help        => "Disables control characters in output");

   end Set_Global_Switches;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Sub_Cmd.Parse_Global_Switches;

      if No_TTY then
         CLIC.TTY.Force_Disable_TTY;
      end if;

      if not No_Color and then not No_TTY then
         CLIC.TTY.Enable_Color (Force => False);
         --  This may still not enable color if TTY is detected to be incapable
      end if;

      Sub_Cmd.Execute;
   end Execute;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error (Str : String) is
   begin
      Simple_Logging.Error (Str);
   end Put_Error;

   -----------------
   -- Setup_Image --
   -----------------

   procedure Setup_Image (This      : in out Command;
                          To_Format :        Boolean := False)
   is
      use CLIC.User_Input;
   begin
      if Sw_Format = null or else Sw_Format.all = "" then
         This.Usage_Error ("Missing required -f/--format switch");
      elsif Sw_Format.all /= "littlefs" then
         This.Usage_Error ("Invalid format (-f/--format) must be 'littlefs'");
      end if;

      if Sw_Image = null then
         This.Usage_Error ("Missing required -i/--img switch");
      end if;

      if To_Format then

         if not GNAT.OS_Lib.Is_Regular_File (Image_Path) then

            --  The file doesn't exists, we try to create it
            This.FD := Create_File (Image_Path, Binary);

         elsif not GNAT.OS_Lib.Is_Owner_Writable_File (Image_Path) then
            This.Failure ("Image file '" & Image_Path & "' is not writable");
         else

            Always ("Existing image file '" & Image_Path &
                      "' will be overwritten.");
            if Query ("Do you want to continue?",
                      Valid    => (Yes | No => True, Always => False),
                      Default  => Yes) = Yes
              then
               This.FD := Open_Read_Write (Image_Path, Binary);
            else
               This.Failure ("Cannot overwrite existing file");
            end if;
         end if;
      else

         --  The image file should exists and be writable

         if not GNAT.OS_Lib.Is_Regular_File (Image_Path) then
            This.Failure ("Image file '" & Image_Path & "' does not exists");
         elsif not GNAT.OS_Lib.Is_Owner_Writable_File (Image_Path) then
            This.Failure ("Image file '" & Image_Path & "' is not writable");
         else
            This.FD := Open_Read_Write (Image_Path, Binary);
         end if;
      end if;

      if This.FD = Invalid_FD then
         This.Failure ("Cannot open image file '" & Image_Path & "'");
      end if;

      This.Target := new FSmaker.Target.LittleFS.Instance;

      if not To_Format then
         This.Target.Mount (This.FD);
      end if;
   end Setup_Image;

   -----------------
   -- Close_Image --
   -----------------

   procedure Close_Image (This : in out Command) is
   begin
      if This.FD /= Invalid_FD and then This.FD /= Null_FD then
         Simple_Logging.Debug ("Closing FD");
         Close (This.FD);
         This.FD := Invalid_FD;
      end if;
   end Close_Image;

   -------------
   -- Success --
   -------------

   procedure Success (This : in out Command) is
   begin
      Always (Dispatch (This).Name & " success");
      This.Close_Image;
   end Success;

   -------------
   -- Failure --
   -------------

   procedure Failure (This : in out Command; Msg : String) is
   begin
      Error (Dispatch (This).Name & " failed: " & Msg);
      This.Close_Image;
      GNAT.OS_Lib.OS_Exit (1);
   end Failure;

   -----------------
   -- Usage_Error --
   -----------------

   procedure Usage_Error (This : in out Command; Msg : String) is
   begin
      Error (Dispatch (This).Name & ": " & Msg);
      This.Close_Image;
      Sub_Cmd.Display_Usage;
      GNAT.OS_Lib.OS_Exit (1);
   end Usage_Error;

begin
   Sub_Cmd.Register (new Sub_Cmd.Builtin_Help);
   Sub_Cmd.Register (new FSmaker.Commands.Init.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Mkdir.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Tree.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Import.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Cat.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Hexdump.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Export.Instance);
   Sub_Cmd.Register (new FSmaker.Commands.Build.Instance);
end FSmaker.Commands;
