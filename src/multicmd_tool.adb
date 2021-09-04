with Ada.Command_Line;
with GNAT.OS_Lib;
with Ada.Strings.Unbounded;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Command_Line.Extra;
with Simple_Logging;

with Ada.Containers.Indefinite_Vectors;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with AAA.Table_IO;
with AAA.Text_IO;

package body MultiCMD_Tool is

   package TTY is
      function Bold (Str : String) return String is (Str);
      function Underline (Str : String) return String is (Str);
      function Description (Str : String) return String is (Str);
      function Emph (Str : String) return String is (Str);
   end TTY;

   package Trace renames Simple_Logging;

   package Command_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Command_Access,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   package Topics_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Help_Topic_Access,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   Registerd_Commands : Command_Maps.Map;
   Registerd_Topics : Topics_Maps.Map;

   Plip : aliased GNAT.OS_Lib.String_Access;

   Command_Failed : exception;
   Wrong_Command_Arguments : exception;
   Error_No_Command : exception;

   Raw_Arguments : String_Vector;
   --  Raw arguments, first one is the command

   procedure Display_Help (Keyword : String);
   procedure Display_Options
     (Config : Command_Line_Configuration;
      Title  : String);
   procedure Display_Global_Options;
   function Highlight_Switches (Line : String) return String;
   function What_Command (Str : String := "") return not null Command_Access;
   function What_Command return String;
   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String;

   -------------
   -- Replace --
   -------------

   function Replace (Text  : String;
                     Match : String;
                     Subst : String)
                     return String
   is
      use Ada.Strings.Fixed;
      First : Natural;
   begin
      First := Index (Text, Match);
      if First = 0 then
         return Text;
      else
         return
           Text (Text'First .. First - 1)
           & Subst
           & Replace (Text (First + Match'Length .. Text'Last), Match, Subst);
      end if;
   end Replace;

   --------------
   -- Register --
   --------------

   procedure Register (Cmd : not null Command_Access) is
      Name : constant Unbounded_String := To_Unbounded_String (Cmd.Name);
   begin
      if Registerd_Commands.Contains (Name) then
         raise Program_Error;
      else
         Registerd_Commands.Insert (Name, Cmd);
      end if;
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register (Topic : not null Help_Topic_Access) is
      Name : constant Unbounded_String := To_Unbounded_String (Topic.Name);
   begin
      if Registerd_Topics.Contains (Name) then
         raise Program_Error;
      else
         Registerd_Topics.Insert (Name, Topic);
      end if;
   end Register;

   ------------------
   -- What_Command --
   ------------------

   function What_Command (Str : String := "") return not null Command_Access is
      Name : constant Unbounded_String :=
        To_Unbounded_String (if Str = "" then What_Command else Str);
   begin
      if Registerd_Commands.Contains (Name) then
         return Registerd_Commands.Element (Name);
      else
         raise Error_No_Command;
      end if;
   end What_Command;

   ------------------
   -- What_Command --
   ------------------

   function What_Command return String is
   begin
      if Raw_Arguments.Is_Empty then
         raise Error_No_Command;
      else
         return Raw_Arguments.First_Element;
      end if;
   end What_Command;

   -------------------------------
   -- Reportaise_Command_Failed --
   -------------------------------

   procedure Reportaise_Command_Failed (Message : String) is
   begin
      raise Command_Failed with Message;
   end Reportaise_Command_Failed;

   --------------------------------
   -- Reportaise_Wrong_Arguments --
   --------------------------------

   procedure Reportaise_Wrong_Arguments (Message : String) is
   begin
      raise Wrong_Command_Arguments with Message;
   end Reportaise_Wrong_Arguments;

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out GNAT.Command_Line.Command_Line_Configuration)
   is
   begin
      Define_Switch (Config,
                     Plip'Access,
                     "-c=", "--config=",
                     "Override configuration folder location");

      Define_Switch (Config,
                     Plip'Access,
                     "-f", "--force",
                     "Keep going after a recoverable troublesome situation");

      Define_Switch (Config,
                     Plip'Access,
                     "-h", "--help",
                     "Display general or command-specific help");

      Define_Switch (Config,
                     Plip'Access,
                     "-n", "--non-interactive",
                     "Assume default answers for all user prompts");

      Define_Switch (Config,
                     Plip'Access,
                     Long_Switch => "--no-color",
                     Help        => "Disables colors in output");

      Define_Switch (Config,
                     Plip'Access,
                     Long_Switch => "--no-tty",
                     Help        => "Disables control characters in output");

      Define_Switch (Config,
                     Plip'Access,
                     Long_Switch => "--prefer-oldest",
                     Help        => "Prefer oldest versions instead of " &
                       "newest when resolving dependencies");

      Define_Switch (Config,
                     Plip'Access,
                     "-q",
                     Help => "Limit output to errors");

      Define_Switch (Config,
                     Plip'Access,
                     "-v",
                     Help => "Be more verbose (use twice for extra detail)");

      Define_Switch (Config,
                     Plip'Access,
                     "-d?",
                     Long_Switch => "--debug?",
                     Help =>
                       "Enable debug-specific log messages");
   end Set_Global_Switches;

   --------------------
   -- Fill_Arguments --
   --------------------

   Fill_For_Real : Boolean := False;

   procedure Fill_Arguments (Switch    : String;
                             Parameter : String;
                             Section   : String)
   is
      --  For some reason, Get_Argument is not working.

      --  This allows capturing any unknown switch under the wildcard class as
      --  an argument.
      pragma Unreferenced (Section);
   begin
      Trace.Never ("S: " & Switch & "; P: " & Parameter);

      --  As of now, the only multiple switch that must be treated here is -X

      if Switch (Switch'First) = '-' then
         if Fill_For_Real then
            if Switch (Switch'First + 1) = 'X' then
               null;
            else
               Reportaise_Wrong_Arguments ("Unrecognized switch: " & Switch);
            end if;
         else
             --  We are only checking global command/switches, these switches
             --  are not yet interesting.
            null;
         end if;
      else
         Raw_Arguments.Append (Switch);
      end if;
   end Fill_Arguments;

   ----------------------------
   -- Display_Valid_Commands --
   ----------------------------

   procedure Display_Valid_Commands is
      Tab   : constant String (1 .. 1) := (others => ' ');
      Table : AAA.Table_IO.Table;
      use Command_Maps;

   begin
      if Registerd_Commands.Is_Empty then
         return;
      end if;

      Put_Line (TTY.Bold ("COMMANDS"));

      for Elt in Registerd_Commands.Iterate loop
         Table.New_Row;
         Table.Append (Tab);
         Table.Append (TTY.Description (To_String (Key (Elt))));
         Table.Append (Tab);
         Table.Append (Element (Elt).Short_Description);
      end loop;

      Table.Print (Separator => "  ", Put_Line => Put_Line'Access);
   end Display_Valid_Commands;

   --------------------------
   -- Display_Valid_Topics --
   --------------------------

   procedure Display_Valid_Topics is
      Tab   : constant String (1 .. 1) := (others => ' ');
      Table : AAA.Table_IO.Table;
      use Topics_Maps;

   begin
      if Registerd_Topics.Is_Empty then
         return;
      end if;

      Put_Line (TTY.Bold ("TOPICS"));

      for Elt in Registerd_Topics.Iterate loop
         Table.New_Row;
         Table.Append (Tab);
         Table.Append (TTY.Description (To_String (Key (Elt))));
         Table.Append (Tab);
         Table.Append (Element (Elt).Title);
      end loop;

      Table.Print (Separator => "  ", Put_Line =>  Put_Line'Access);
   end Display_Valid_Topics;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Cmd : not null Command_Access) is
      Config  : Command_Line_Configuration;
      Canary1 : Command_Line_Configuration;
      Canary2 : Command_Line_Configuration;
   begin
      Put_Line (TTY.Bold ("SUMMARY"));
      Put_Line ("   " & Cmd.Short_Description);

      New_Line;
      Put_Line (TTY.Bold ("USAGE"));
      Put ("   ");
      Put_Line
        (TTY.Underline ("alr") &
           " " &
         TTY.Underline (Cmd.Name) &
         " [options] " &
         Cmd.Usage_Custom_Parameters);

      --  We use the following two canaries to detect if a command is adding
      --  its own switches, in which case we need to show their specific help.

      Set_Global_Switches (Canary1); -- For comparison
      Set_Global_Switches (Canary2); -- For comparison
      Cmd.Setup_Switches (Canary1);

      if Get_Switches (Canary1) /= Get_Switches (Canary2) then
         Cmd.Setup_Switches (Config);
      end if;

      --  Without the following line, GNAT.Display_Help causes a segfault for
      --  reasons I'm unable to pinpoint. This way it prints a harmless blank
      --  line that we want anyway.

      Define_Switch (Config, " ", " ", " ", " ", " ");

      Display_Options (Config, "OPTIONS");

      Display_Global_Options;

      --  Format and print the long command description
      New_Line;
      Put_Line (TTY.Bold ("DESCRIPTION"));

      for Line of Cmd.Long_Description loop
         AAA.Text_IO.Put_Paragraph (Highlight_Switches (Line),
                                    Line_Prefix => "   ");
         --  GNATCOLL.Paragraph_Filling seems buggy at the moment, otherwise
         --  it would be the logical choice.
      end loop;
   end Display_Usage;

   -------------------
   -- Display_Usage --
   -------------------

   procedure Display_Usage (Displayed_Error : Boolean := False) is
   begin
      if not Displayed_Error then
         Put_Line ("plop ");
         New_Line;
      end if;

      Put_Line (TTY.Bold ("USAGE"));
      Put_Line ("   " & TTY.Underline ("alr") & " [global options] " &
                  "<command> [command options] [<arguments>]");
      New_Line;
      Put_Line ("   " & TTY.Underline ("alr") & " " &
                        TTY.Underline ("help") &
                        " [<command>|<topic>]");

      New_Line;
      Put_Line (TTY.Bold ("ARGUMENTS"));
      declare
         Tab   : constant String (1 .. 1) := (others => ' ');
         Table : AAA.Table_IO.Table;
      begin
         Table.New_Row;
         Table.Append (Tab);
         Table.Append (TTY.Description ("<command>"));
         Table.Append ("Command to execute");

         Table.New_Row;
         Table.Append (Tab);
         Table.Append (TTY.Description ("<arguments>"));
         Table.Append ("List of arguments for the command");

         Table.Print (Separator => "   ",
                      Put_Line  => Put_Line'Access);
      end;

      Display_Global_Options;
      Display_Valid_Commands;
      Display_Valid_Topics;
   end Display_Usage;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   --  Once this procedure returns, the command, arguments and switches will
   --  be ready for use. Otherwise, appropriate help is shown and it does not
   --  return.

      --------------------
      -- Check_For_Help --
      --------------------

      function Check_First_Nonswitch return Integer is
         use Ada.Command_Line;
         First_Nonswitch : Integer := 0;
         --  Used to store the first argument that doesn't start with '-';
         --  that would be the command for which help is being asked.
      begin
         for I in 1 .. Argument_Count loop
            declare
               Arg : constant String := Ada.Command_Line.Argument (I);
            begin
               if First_Nonswitch = 0 and then Arg (Arg'First) /= '-' then
                  First_Nonswitch := I;
               end if;
            end;
         end loop;

         return First_Nonswitch;
      end Check_First_Nonswitch;

      function Check_For_Help return Boolean is
         use Ada.Command_Line;
      begin
         return (for some I in 1 .. Argument_Count =>
                   Ada.Command_Line.Argument (I) in "-h" | "--help");
      end Check_For_Help;

      function Get_Arguments return GNAT.OS_Lib.Argument_List_Access is
         use Ada.Command_Line;

         package SU renames Ada.Strings.Unbounded;

         Arguments : SU.Unbounded_String;
      begin
         for I in 1 .. Argument_Count loop
            declare
               Arg : constant String := Ada.Command_Line.Argument (I);
            begin
               if Arg not in "-h" | "--help" then
                  SU.Append (Arguments, (if I = 1 then "" else " ") & Arg);
               end if;
            end;
         end loop;

         return GNAT.OS_Lib.Argument_String_To_List (SU.To_String (Arguments));
      end Get_Arguments;

      use all type GNAT.OS_Lib.String_Access;

      Global_Config   : Command_Line_Configuration;
      Command_Config  : Command_Line_Configuration;

      Help_Requested  : Boolean;
      First_Nonswitch : Integer;

      Arguments        : GNAT.OS_Lib.Argument_List_Access;
      Arguments_Parser : Opt_Parser;
   begin
      --  GNAT switch handling intercepts -h/--help. To have the same output
      --  for 'alr -h command' and 'alr help command', we do manual handling
      --  first in search of a -h/--help:
      Help_Requested  := Check_For_Help;
      First_Nonswitch := Check_First_Nonswitch;

      Arguments := Get_Arguments;

      Set_Global_Switches (Global_Config);

      --  To avoid erroring on command-specific switches we add the wildcard.
      --  However, if help was requested, we don't want the "[any string]" text
      --  to be displayed by Getopt below, so in that case we bypass it.
      if not Help_Requested then
         Define_Switch (Global_Config, "*");
      end if;

      Initialize_Option_Scan (Arguments_Parser, Arguments);
      Getopt (Global_Config,
              Callback => Fill_Arguments'Access,
              Parser   => Arguments_Parser);

      --  At this point the command and all unknown switches are in
      --  Raw_Arguments.

      GNAT.OS_Lib.Free (Arguments);


      --  Show either general or specific help
      if Help_Requested then
         if First_Nonswitch > 0 then
            Display_Help
              (Ada.Command_Line.Argument (First_Nonswitch));
            Error_Exit (0);
         else
            null;
            --  Nothing to do; later on GNAT switch processing will catch
            --  the -h/--help and display the general help.
         end if;
      end if;

      if Raw_Arguments.Is_Empty then
         Display_Usage;
         Error_Exit (1);
      end if;

      --  Dispatch to the appropriate command (which includes 'help')

      declare
         Cmd : not null Command_Access := What_Command;
         --  Might raise if invalid, if so we are done
      begin
         Raw_Arguments := Empty_Vector; -- Reinitialize arguments
         Set_Global_Switches (Command_Config);

         --  Specific to command
         Cmd.Setup_Switches (Command_Config);

         --  Ensure Command has not set a switch that is already global:
         if not GNAT.Command_Line.Extra.Verify_No_Duplicates (Command_Config)
         then
            raise Program_Error with "Duplicate switch definition detected";
         end if;

         --  Validate combined command + global configuration:
         Fill_For_Real := True;

         Initialize_Option_Scan;
         Getopt (Command_Config);

         --  If OK, retrieve all arguments with the final, command-specific
         --  proper configuration.
         Define_Switch (Command_Config, "*");
         Getopt (Command_Config, Callback => Fill_Arguments'Access);

         if Raw_Arguments.Is_Empty then
            raise Program_Error with "we should have at least the command name here";
         else
            Raw_Arguments.Delete_First;
         end if;

         Cmd.Execute (Raw_Arguments);
      end;

   exception
      when Exit_From_Command_Line | Invalid_Switch | Invalid_Parameter =>
         --  Getopt has already displayed some help
         New_Line;
         Put_Line ("Use ""alr help <command>"" for specific command help");
         Error_Exit (1);
      when E : Constraint_Error | Error_No_Command =>
         --  Alire.Log_Exception (E);
         if Raw_Arguments (1) (String'(Raw_Arguments (1))'First) = '-' then
            Error ("Unrecognized global option: " & Raw_Arguments (1));
         else
            Error ("Unrecognized command: " & Raw_Arguments (1));
         end if;
         New_Line;
         Display_Usage (Displayed_Error => True);
         Error_Exit (1);
      when Wrong_Command_Arguments =>
         --  Raised in here, so no need to raise up unless in debug mode
         Error_Exit (1);
   end Parse_Command_Line;

   ----------
   -- Plop --
   ----------

   procedure Plop is
   begin
      Parse_Command_Line;
   end Plop;

   -- Help command --

   ------------------------
   -- Highlight_Switches --
   ------------------------

   function Highlight_Switches (Line : String) return String is

      use AAA.Strings.Vectors;
      use AAA.Strings;

      ---------------
      -- Highlight --
      ---------------
      --  Avoid highlighting non-alphanumeric characters
      function Highlight (Word : String) return String is
         subtype Valid_Chars is Character with
           Dynamic_Predicate => Valid_Chars in '0' .. '9' | 'a' .. 'z';
         I : Natural := Word'Last; -- last char to highlight
      begin
         while I >= Word'First and then Word (I) not in Valid_Chars loop
            I := I - 1;
         end loop;

         return TTY.Emph (Word (Word'First .. I)) & Word (I + 1 .. Word'Last);
      end Highlight;

      Words : String_Vector := AAA.Strings.Split (Line, Separator => ' ');
      I, J  : Vectors.Cursor;
   begin
      I := Words.First;
      while Has_Element (I) loop
         declare
            Word : constant String := Element (I);
         begin
            J := Next (I);
            if Has_Prefix (Word, "--") and then Word'Length > 2
            then
               Words.Insert (Before => J, New_Item => Highlight (Word));
               Words.Delete (I);
            end if;
            I := J;
         end;
      end loop;
      return Flatten (Words);
   end Highlight_Switches;

   ---------------------
   -- Display_Options --
   ---------------------

   procedure Display_Options
     (Config : Command_Line_Configuration;
      Title  : String)
   is
      Tab     : constant String (1 .. 1) := (others => ' ');
      Table   : AAA.Table_IO.Table;

      Has_Printable_Rows : Boolean := False;

      function Without_Arg (Value : String) return String is
         Required_Character : constant Character := Value (Value'Last);
      begin
         return
            (if Required_Character in '=' | ':' | '!' | '?' then
               Value (Value'First .. Value'Last - 1)
             else
               Value);
      end Without_Arg;

      function With_Arg (Value, Arg : String) return String is
         Required_Character : constant Character := Value (Value'Last);
      begin
         return
            (if Required_Character in '=' | ':' | '!' | '?' then
               Replace
                 (Value,
                  "" & Required_Character,
                  (case Required_Character is
                     when '=' => "=" & Arg,
                     when ':' => "[ ] " & Arg,
                     when '!' => Arg,
                     when '?' => "[" & Arg & "]",
                     when others => raise Program_Error))
            else Value);
      end With_Arg;

      procedure Print_Row (Short_Switch, Long_Switch, Arg, Help : String) is
         Has_Short : constant Boolean := Short_Switch not in " " | "";
         Has_Long  : constant Boolean := Long_Switch not in " " | "";
      begin
         if (not Has_Short and not Has_Long) or Help = "" then
            return;
         end if;

         Table.New_Row;
         Table.Append (Tab);

         if Has_Short and Has_Long then
            Table.Append (TTY.Description (Without_Arg (Short_Switch)) &
              " (" & With_Arg (Long_Switch, Arg) & ")");
         elsif not Has_Short and Has_Long then
            Table.Append (TTY.Description (With_Arg (Long_Switch, Arg)));
         elsif Has_Short and not Has_Long then
            Table.Append (TTY.Description (With_Arg (Short_Switch, Arg)));
         end if;

         Table.Append (Help);

         Has_Printable_Rows := True;
      end Print_Row;
   begin
      GNAT.Command_Line.Extra.For_Each_Switch
        (Config, Print_Row'Access);

      if Has_Printable_Rows then
         New_Line;
         Put_Line (TTY.Bold (Title));

         Table.Print (Separator => "  ",
                      Put_Line  => Put_Line'Access);
      end if;
   end Display_Options;

   ----------------------------
   -- Display_Global_Options --
   ----------------------------

   procedure Display_Global_Options is
      Global_Config   : Command_Line_Configuration;
   begin
      Set_Global_Switches (Global_Config);
      Display_Options (Global_Config, "GLOBAL OPTIONS");
   end Display_Global_Options;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help (Keyword : String) is

      ------------
      -- Format --
      ------------

      procedure Format (Text : String_Vector) is
      begin
         for Line of Text loop
            AAA.Text_IO.Put_Paragraph (Highlight_Switches (Line),
                                       Line_Prefix => "   ");
         end loop;
      end Format;

      Ukey : constant Unbounded_String := To_Unbounded_String (Keyword);
   begin
      if Registerd_Commands.Contains (Ukey) then
         Display_Usage (What_Command (Keyword));

      elsif Registerd_Topics.Contains (Ukey) then
         Put_Line (TTY.Bold (Registerd_Topics.Element (Ukey).Title));
         Format (Registerd_Topics.Element (Ukey).Content);
      else
         Trace.Error ("No help found for: " & Keyword);
         Display_Global_Options;
         Display_Valid_Commands;
         Display_Valid_Topics;
         Error_Exit (1);
      end if;
   end Display_Help;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (This : in out Help_Instance;
                      Args :        String_Vector)
   is
      use Ada.Containers;
   begin
      if Args.Length /= 1 then
         if Args.Length > 1 then
            Trace.Error ("Please specify a single help keyword");
            New_Line;
         end if;

         Put_Line (TTY.Bold ("USAGE"));
         Put_Line ("   " & TTY.Underline ("alr") & " " &
           TTY.Underline ("help") & " [<command>|<topic>]");

         New_Line;
         Put_Line (TTY.Bold ("ARGUMENTS"));
         declare
            Tab   : constant String (1 .. 1) := (others => ' ');
            Table : AAA.Table_IO.Table;
         begin
            Table.New_Row;
            Table.Append (Tab);
            Table.Append (TTY.Description ("<command>"));
            Table.Append ("Command for which to show a description");

            Table.New_Row;
            Table.Append (Tab);
            Table.Append (TTY.Description ("<topic>"));
            Table.Append ("Topic for which to show a description");

            Table.Print (Separator => "  ",
                         Put_Line  => Put_Line'Access);
         end;

         Display_Global_Options;
         Display_Valid_Commands;
         Display_Valid_Topics;
         Error_Exit (1);
      end if;

      Display_Help (Args (1));
   end Execute;

   ----------------------
   -- Long_Description --
   ----------------------

   overriding
   function Long_Description (This : Help_Instance)
                              return MultiCMD_Tool.String_Vector
   is (Empty_Vector
       .Append ("Shows information about commands and topics.")
       .Append ("See available commands with '<> help commands'")
       .Append ("See available topics with '<> help topics'."));

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (This    : in out Help_Instance;
      Config  : in out GNAT.Command_Line.Command_Line_Configuration)
   is null;

   -----------------------
   -- Short_Description --
   -----------------------

   overriding
   function Short_Description (This : Help_Instance) return String
   is ("Shows help on the given command/topic");

   -----------------------------
   -- Usage_Custom_Parameters --
   -----------------------------

   overriding
   function Usage_Custom_Parameters (This : Help_Instance) return String
   is ("[<command>|<topic>]");

begin
   Register (new Help_Instance);
end MultiCMD_Tool;
