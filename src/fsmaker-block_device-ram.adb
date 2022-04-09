with System.Storage_Elements; use System.Storage_Elements;

package body FSmaker.Block_Device.RAM is

   ----------------
   -- Write_Data --
   ----------------

   procedure Write_Data (This :        Instance;
                         Dst  : in out FSmaker.Sink.Class)
   is
      Len : constant Natural := This.Data'Size / 8;
   begin
      if Dst.Write (This.Data'Address, Len) /= Len then
         raise Program_Error;
      end if;
   end Write_Data;

   ----------
   -- Read --
   ----------

   function Read (This            : in out Instance;
                  Block_Id        :        Natural;
                  Offset_In_Block :        Natural;
                  Buffer          :        System.Address;
                  Size            :        Natural)
                  return Result
   is
      Block_Index : constant Natural := This.Data'First (1) + Block_Id;
      Src_First : constant Natural := This.Data'First (2) + Offset_In_Block;

      Dst : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
      Src : Storage_Array (Dst'Range)
        with Address => This.Data (Block_Index, Src_First)'Address;
   begin
      Dst := Src;
      return Ok;
   end Read;

   -------------
   -- Program --
   -------------

   function Program (This            : in out Instance;
                     Block_Id        :        Natural;
                     Offset_In_Block :        Natural;
                     Buffer          :        System.Address;
                     Size            :        Natural)
                     return Result
   is
      Block_Index : constant Natural := This.Data'First (1) + Block_Id;
      Src_First : constant Natural := This.Data'First (2) + Offset_In_Block;

      Src : Storage_Array (1 .. Storage_Offset (Size))
        with Address => Buffer;
      Dst : Storage_Array (Src'Range)
        with Address => This.Data (Block_Index, Src_First)'Address;
   begin
      Dst := Src;
      return Ok;
   end Program;

   -----------
   -- Erase --
   -----------

   function Erase (This     : in out Instance;
                   Block_Id :        Natural)
                   return Result
   is
      Block_Index : constant Natural := This.Data'First (1) + Block_Id;

   begin
      for Index in This.Data'Range (2) loop
         This.Data (Block_Index, Index) := 16#FF#;
      end loop;
      return Ok;
   end Erase;

   ----------
   -- Sync --
   ----------

   function Sync (This : in out Instance) return Result is
   begin
      return Ok;
   end Sync;

end FSmaker.Block_Device.RAM;
