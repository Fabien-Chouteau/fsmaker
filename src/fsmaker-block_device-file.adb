with Interfaces;

with Simple_Logging;
with GNAT.Source_Info;

package body FSmaker.Block_Device.File is

   ------------
   -- Create --
   ------------

   function Create (FD               : GNAT.OS_Lib.File_Descriptor;
                    Block_Size       : Positive;
                    Number_Of_Blocks : Positive)
                    return Instance
   is
   begin
      return Instance'(Block_Size       => Block_Size,
                       Number_Of_Blocks => Number_Of_Blocks,
                       FD               => FD);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (FD               : GNAT.OS_Lib.File_Descriptor;
                    Block_Size       : Positive;
                    Number_Of_Blocks : Positive)
                    return not null Block_Device.Acc_Any
   is
   begin
      return new Instance'(Block_Size       => Block_Size,
                           Number_Of_Blocks => Number_Of_Blocks,
                           FD               => FD);
   end Create;

   ----------
   -- Read --
   ----------

   overriding
   function Read (This            : in out Instance;
                  Block_Id        :        Natural;
                  Offset_In_Block :        Natural;
                  Buffer          :        System.Address;
                  Size            :        Natural)
                  return Result
   is
      File_Offset : constant Natural :=
        Offset_In_Block + This.Block_Size * Block_Id;

   begin
      GNAT.OS_Lib.Lseek (FD     => This.FD,
                         offset => Long_Integer (File_Offset),
                         origin => GNAT.OS_Lib.Seek_Set);

      if GNAT.OS_Lib.Read (This.FD, Buffer, Size) = Size then
         return Ok;
      else
         Simple_Logging.Error (GNAT.Source_Info.Enclosing_Entity & ": " &
                                 GNAT.OS_Lib.Errno_Message);
         return Error;
      end if;
   end Read;

   -------------
   -- Program --
   -------------

   overriding
   function Program (This            : in out Instance;
                     Block_Id        :        Natural;
                     Offset_In_Block :        Natural;
                     Buffer          :        System.Address;
                     Size            :        Natural)
                     return Result
   is
      File_Offset : constant Natural :=
        Offset_In_Block + This.Block_Size * Block_Id;

   begin
      GNAT.OS_Lib.Lseek (FD     => This.FD,
                         offset => Long_Integer (File_Offset),
                         origin => GNAT.OS_Lib.Seek_Set);

      if GNAT.OS_Lib.Write (This.FD, Buffer, Size) = Size then
         return Ok;
      else
         Simple_Logging.Error (GNAT.Source_Info.Enclosing_Entity & ": " &
                                 GNAT.OS_Lib.Errno_Message);
         return Error;
      end if;

   end Program;

   -----------
   -- Erase --
   -----------

   overriding
   function Erase (This     : in out Instance;
                   Block_Id :        Natural)
                   return Result
   is
      Data : constant array (1 .. This.Block_Size) of Interfaces.Unsigned_8 :=
        (others => 16#FF#);
   begin
      return This.Program (Block_Id, 0, Data'Address, This.Block_Size);
   end Erase;

   ----------
   -- Sync --
   ----------

   overriding
   function Sync (This : in out Instance) return Result is
   begin
      return Ok;
   end Sync;

end FSmaker.Block_Device.File;
