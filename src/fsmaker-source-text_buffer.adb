with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body FSmaker.Source.Text_Buffer is

   ---------
   -- Put --
   ---------

   procedure Put (This : in out Instance; C : Character) is
   begin
      Append (This.Buffer, C);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (This : in out Instance; Str : String) is
   begin
      Append (This.Buffer, Str);
   end Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (This : in out Instance) is
   begin
      Append (This.Buffer, ASCII.LF);
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : in out Instance; Str : String) is
   begin
      This.Put (Str);
      This.New_Line;
   end Put_Line;

   ----------
   -- Read --
   ----------

   overriding
   function Read (This : in out Instance;
                  Addr : System.Address;
                  Len  : Natural)
                  return Natural
   is
      Remaining : constant Natural :=
        Length (This.Buffer) - This.Read_Index + 1;

      Count : constant Natural := Natural'Min (Len, Remaining);
   begin

      declare
         Src : constant String := Slice (This.Buffer,
                                         This.Read_Index,
                                         This.Read_Index + Count - 1);
         Dst : String (Src'Range)
           with Address => Addr;
      begin
         Dst := Src;
      end;
      This.Read_Index := This.Read_Index + Count;
      return Count;
   end Read;

end FSmaker.Source.Text_Buffer;
