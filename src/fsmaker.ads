with AAA.Strings;

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package FSmaker is

   function Valid_Path_Char (C : Character) return Boolean
   is (C in '/' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
            ' ' | '-' | '_' | '.');

   function Valid_Target_Path (Str : String) return Boolean
   is (Str'Length > 0
       and then Str (Str'First) = '/'
       and then (for all C of Str => Valid_Path_Char (C)));

   subtype Target_Path is AAA.Strings.Vector;
   Empty_Path : Target_Path renames AAA.Strings.Empty_Vector;

   function To_Target_Path (Str : String) return Target_Path;

   type Node_Kind is (Dir, File);

   type Node (Kind : Node_Kind);

   type Node_Access is access all Node;

   package Node_Vectors
   is new Ada.Containers.Vectors (Natural, Node_Access);

   subtype Directory_Tree is Node_Vectors.Vector;

   type Node (Kind : Node_Kind) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      case Kind is
         when Dir =>
            Entries : Directory_Tree;
         when File =>
            null;
      end case;
   end record;

   procedure Pretty_Print (Tree   : Directory_Tree;
                           Indent : String := "|");

end FSmaker;
