private with Ada.Strings.Unbounded;

package FSmaker.Source.Text_Buffer is

   subtype Parent is Source.Instance;
   type Instance is new Parent with private;

   procedure Put (This : in out Instance; C : Character);
   procedure Put (This : in out Instance; Str : String);
   procedure New_Line (This : in out Instance);
   procedure Put_Line (This : in out Instance; Str : String);

private

   type Instance
   is new Parent
   with record
      Buffer : Ada.Strings.Unbounded.Unbounded_String;
      Read_Index : Positive := 1;
   end record;

   overriding
   function Read (This : in out Instance;
                  Addr :        System.Address;
                  Len  :        Natural)
                  return Natural;

end FSmaker.Source.Text_Buffer;
