with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces;              use Interfaces;

package body FSmaker.Sink.Hexdump is

   ------------
   -- Create --
   ------------

   function Create (Dst : not null Sink.Acc_Any) return Sink.Class is
   begin
      return Instance'(Dst => Dst, others => <>);
   end Create;

   -----------
   -- Write --
   -----------

   overriding
   function Write (This : in out Instance;
                   Addr : System.Address;
                   Len : Natural)
                   return Natural
   is
      ---------
      -- Put --
      ---------

      procedure Put (Str : String) is
      begin
         if This.Dst.Write (Str'Address, Str'Length) /= Str'Length then
            raise Program_Error with "hexdump write error";
         end if;
      end Put;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (Str : String) is
      begin
         Put (Str & ASCII.LF);
      end Put_Line;

      --------------
      -- Hex_Dump --
      --------------

      procedure Hex_Dump (Data : Storage_Array) is

         function UInt8_To_Char (Val : Storage_Element) return Character;
         procedure Start_New_Line;

         pragma Style_Checks ("M120");
         --  Hexdump format:
         --         0000_0000_0000_0000: 57 69 6B 69 70 65 64 69 61 2C 20 74 68 65 20 66  Wikipedia, the f
         --  Addr : ^^^^^^^^^^^^^^^^^^^^
         --  Hex  :                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         --  ASCII:                                                                      ^^^^^^^^^^^^^^^^^

         Addr_Len  : constant := 16 + 3 + 1;
         Hex_Len   : constant := 3 * 16;
         ASCII_Len : constant := 1 + 16;
         Str   : String (1 .. Addr_Len + Hex_Len + ASCII_Len) := (others => ' ');
         UInt4_To_Char : constant array (Unsigned_4) of Character
           := (0 =>  '0',
               1 =>  '1',
               2 =>  '2',
               3 =>  '3',
               4 =>  '4',
               5 =>  '5',
               6 =>  '6',
               7 =>  '7',
               8 =>  '8',
               9 =>  '9',
               10 => 'A',
               11 => 'B',
               12 => 'C',
               13 => 'D',
               14 => 'E',
               15 => 'F');

         -------------------
         -- UInt8_To_Char --
         -------------------

         function UInt8_To_Char (Val : Storage_Element) return Character is
         begin
            case Val is
            when 0 .. 31 | 127 .. 255 =>
               return '.';
            when others =>
               return Character'Val (Val);
            end case;
         end UInt8_To_Char;

         Index : Natural;
         Cnt   : Natural;
         Addr  : Natural := 0;

         --------------------
         -- Start_New_Line --
         --------------------

         procedure Start_New_Line is
            Addr_Val : Interfaces.Unsigned_64 :=
              Interfaces.Unsigned_64 (This.Addr + Addr);
         begin

            --  Address
            for X in reverse 1 .. 19 loop
               if X in 5 | 10 | 15 then
                  Str (X) := '_';
               else
                  Str (X) := UInt4_To_Char (Unsigned_4 (Addr_Val and 16#0F#));
                  Addr_Val := Shift_Right (Addr_Val, 4);
               end if;
            end loop;

            Str (20) := ':';
            Str (21 .. Str'Last) := (others => ' ');

            Cnt := 0;
            Index := Str'First + Addr_Len;
         end Start_New_Line;

      begin

         Start_New_Line;

         for Elt of Data loop

            --  Hex
            Str (Index + 1) := UInt4_To_Char (Unsigned_4 (Shift_Right (Interfaces.Unsigned_8 (Elt), 4)));
            Str (Index + 2) := UInt4_To_Char (Unsigned_4 (Elt and 16#0F#));

            --  ASCII
            Str (Str'Last - (15 - Cnt)) := UInt8_To_Char (Elt);

            Index := Index + 3;
            Cnt   := Cnt + 1;
            Addr  := Addr + 1;
            if Cnt = 16 then
               Put_Line (Str);
               Start_New_Line;
            end if;
         end loop;

         if Cnt /= 0 then
            Put_Line (Str);
         end if;
         This.Addr := This.Addr + Addr;
      end Hex_Dump;

      Data : Storage_Array (1 .. Storage_Offset (Len)) with Address => Addr;
   begin
      Hex_Dump (Data);
      return Len;
   end Write;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (This : in out Instance) is
   begin
      This.Dst.Close;
   end Close;

end FSmaker.Sink.Hexdump;
