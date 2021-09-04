with CLIC.Subcommander;
with AAA.Strings;

package FSmaker.Commands.Cat is

   subtype Parent is FSmaker.Commands.Command;
   type Instance
   is new Parent
   with private;

   overriding
   function Name (This : Instance) return CLIC.Subcommander.Identifier
   is ("cat");

   overriding
   procedure Execute (This : in out Instance;
                      Args :        AAA.Strings.Vector);

   overriding
   function Long_Description (This : Instance)
                              return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector);

   overriding
   procedure Setup_Switches
     (This    : in out Instance;
      Config  : in out CLIC.Subcommander.Switches_Configuration);

   overriding
   function Short_Description (This : Instance) return String
   is ("Display the content of a target file on stdout");

   overriding
   function Usage_Custom_Parameters (This : Instance) return String
   is ("<target_path>");

private

   subtype Dispatch is Instance'Class;

   type Instance
   is new Parent
   with null record;

end FSmaker.Commands.Cat;
