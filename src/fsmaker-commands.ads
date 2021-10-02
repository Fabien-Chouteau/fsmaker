with CLIC.Subcommand;

private with Ada.Text_IO;
private with GNAT.OS_Lib;
private with CLIC.Subcommand.Instance;
private with CLIC.TTY;
private with FSmaker.Target;

package FSmaker.Commands is

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration);

   procedure Execute;

   function Format return String;
   function Image_Path return String;

   function Verbose return Boolean;
   function Debug return Boolean;

   type Command is abstract new CLIC.Subcommand.Command with private;

   function Switch_Parsing (This : Command)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Parse_All);

   procedure Setup_Image (This      : in out Command;
                          To_Format :        Boolean := False);

   procedure Close_Image (This : in out Command);

   procedure Success (This : in out Command);

   procedure Failure (This : in out Command; Msg : String)
     with No_Return;

   procedure Usage_Error (This : in out Command; Msg : String)
     with No_Return;

private

   Sw_Format : aliased GNAT.OS_Lib.String_Access;
   Sw_Image : aliased GNAT.OS_Lib.String_Access;
   Sw_Verbose : aliased Boolean;
   Sw_Debug : aliased Boolean;

   function Format return String
   is (if GNAT.OS_Lib."=" (Sw_Format, null) then "" else Sw_Format.all);

   function Image_Path return String
   is (if GNAT.OS_Lib."=" (Sw_Image, null) then "" else Sw_Image.all);

   function Verbose return Boolean
   is (Sw_Verbose);

   function Debug return Boolean
   is (Sw_Debug);

   procedure Put_Error (Str : String);

   package Sub_Cmd is new CLIC.Subcommand.Instance
     (Main_Command_Name   => "fsmaker",
      Version             => "0.1.0-dev",
      Set_Global_Switches => Set_Global_Switches,
      Put                 => Ada.Text_IO.Put,
      Put_Line            => Ada.Text_IO.Put_Line,
      Put_Error           => Put_Error,
      Error_Exit          => GNAT.OS_Lib.OS_Exit,
      TTY_Chapter         => CLIC.TTY.Info,
      TTY_Description     => CLIC.TTY.Description,
      TTY_Version         => CLIC.TTY.Version,
      TTY_Underline       => CLIC.TTY.Underline,
      TTY_Emph            => CLIC.TTY.Emph);

   type Command is abstract new CLIC.Subcommand.Command with record
      FD     : GNAT.OS_Lib.File_Descriptor;
      Target : FSmaker.Target.Any_Filesystem_Ref := null;
   end record;

end FSmaker.Commands;
