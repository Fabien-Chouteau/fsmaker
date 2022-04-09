with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Address_To_Access_Conversions;

with GNAT.Source_Info;

with System;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with Simple_Logging;

with Littlefs; use Littlefs;

package body FSmaker.Target.LittleFS is

   package BD_Ptr
   is new System.Address_To_Access_Conversions (Block_Device.Class);

   function ftruncate (FS : int;
                       Length : Long_Integer)
                       return int;
   pragma Import (C, ftruncate, "ftruncate");

   function fsync (FS : int)
                   return int;
   pragma Import (C, fsync, "fsync");

   procedure Free is new Ada.Unchecked_Deallocation (LFS_Config,
                                                     LFS_Config_Access);

   package Backend is
      function Create (BD : BD_Ptr.Object_Pointer)
                       return LFS_Config_Access;

   end Backend;

   package body Backend is

      function Read (C      : access constant LFS_Config;
                     Block  : LFS_Block;
                     Off    : LFS_Offset;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return int
        with Convention => C;

      function Prog (C      : access constant LFS_Config;
                     Block  : LFS_Block;
                     Off    : LFS_Offset;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return int
        with Convention => C;
      function Erase (C     : access constant LFS_Config;
                      Block : LFS_Block)
                      return int
        with Convention => C;
      function Sync (C : access constant LFS_Config) return int
        with Convention => C;

      ----------
      -- Read --
      ----------

      function Read (C      : access constant LFS_Config;
                     Block  : LFS_Block;
                     Off    : LFS_Offset;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return int
      is
         BD : constant BD_Ptr.Object_Pointer := BD_Ptr.To_Pointer (C.Context);
      begin
         case BD.Read (Natural (Block),
                       Natural (Off),
                       Buffer,
                       Natural (Size))
         is
            when Block_Device.Ok =>
               return 0;
            when Block_Device.Error =>
               return LFS_ERR_IO;
         end case;
      end Read;

      ----------
      -- Prog --
      ----------

      function Prog (C      : access constant LFS_Config;
                     Block  : LFS_Block;
                     Off    : LFS_Offset;
                     Buffer : System.Address;
                     Size   : LFS_Size)
                     return int
      is
         BD : constant BD_Ptr.Object_Pointer := BD_Ptr.To_Pointer (C.Context);
      begin
         case BD.Program (Natural (Block),
                          Natural (Off),
                          Buffer,
                          Natural (Size))
         is
            when Block_Device.Ok =>
               return 0;
            when Block_Device.Error =>
               return LFS_ERR_IO;
         end case;
      end Prog;

      -----------
      -- Erase --
      -----------

      function Erase (C : access constant LFS_Config;
                      Block : LFS_Block)
                      return int
      is
         BD : constant BD_Ptr.Object_Pointer := BD_Ptr.To_Pointer (C.Context);
      begin
         case BD.Erase (Natural (Block)) is
            when Block_Device.Ok =>
               return 0;
            when Block_Device.Error =>
               return LFS_ERR_IO;
         end case;
      end Erase;

      ----------
      -- Sync --
      ----------

      function Sync (C : access constant LFS_Config) return int is
         BD : constant BD_Ptr.Object_Pointer := BD_Ptr.To_Pointer (C.Context);
      begin
         case BD.Sync is
            when Block_Device.Ok =>
               return 0;
            when Block_Device.Error =>
               return LFS_ERR_IO;
         end case;
      end Sync;

      ------------
      -- Create --
      ------------

      function Create (BD : BD_Ptr.Object_Pointer) return LFS_Config_Access
      is
         Ret : constant LFS_Config_Access := new LFS_Config;

      begin
         Ret.Context := BD_Ptr.To_Address (BD);
         Ret.Block_Size :=  LFS_Size (BD.Block_Size);

         Ret.Read := Read'Access;
         Ret.Prog := Prog'Access;
         Ret.Erase := Erase'Access;
         Ret.Sync := Sync'Access;
         Ret.Read_Size := Ret.Block_Size;
         Ret.Prog_Size := Ret.Block_Size;

         Ret.Block_Count := LFS_Size (BD.Number_Of_Blocks);

         Ret.Block_Cycles := 700;
         Ret.Cache_Size := Ret.Block_Size;
         Ret.Lookahead_Size := Ret.Block_Size;
         Ret.Read_Buffer := System.Null_Address;
         Ret.Prog_Buffer := System.Null_Address;
         Ret.Lookahead_Buffer := System.Null_Address;
         Ret.Name_Max := 0;
         Ret.File_Max := 0;
         Ret.Attr_Max := 0;
         return Ret;
      end Create;
   end Backend;

   ---------------
   -- Error_Img --
   ---------------

   function Error_Img (Err : int) return String
   is (case Err is
          when LFS_ERR_OK          => "No error",
          when LFS_ERR_IO          => "Error during device operation",
          when LFS_ERR_CORRUPT     => "Corrupted",
          when LFS_ERR_NOENT       => "No directory entry",
          when LFS_ERR_EXIST       => "Entry already exists",
          when LFS_ERR_NOTDIR      => "Entry is not a dir",
          when LFS_ERR_ISDIR       => "Entry is a dir",
          when LFS_ERR_NOTEMPTY    => "Dir is not empty",
          when LFS_ERR_BADF        => "Bad file number",
          when LFS_ERR_FBIG        => "File too large",
          when LFS_ERR_INVAL       => "Invalid parameter",
          when LFS_ERR_NOSPC       => "No space left on device",
          when LFS_ERR_NOMEM       => "No more memory available",
          when LFS_ERR_NOATTR      => "No data/attr available",
          when LFS_ERR_NAMETOOLONG => "File name too long",
          when others              => "Unknown LFS error (" & Err'Img & ")");

   ------------
   -- Format --
   ------------

   overriding
   procedure Format (This             : in out   Instance;
                     BD               : not null Block_Device.Acc_Any)
   is
      use GNAT.OS_Lib;

      Size : constant Positive := BD.Block_Size * BD.Number_Of_Blocks;
      Unused : Integer;

      Err : int;

      Config : LFS_Config_Access;
   begin
      Config := Backend.Create (BD_Ptr.Object_Pointer (BD));
      Err := Format (This.LFS, Config.all);
      Free (Config);

      if Err /= 0 then
         raise Program_Error with "format: " & Error_Img (Err);
      end if;
   end Format;

   -----------
   -- Mount --
   -----------

   overriding
   procedure Mount (This : in out   Instance;
                    BD   : not null Block_Device.Acc_Any)
   is
      Err : int;
   begin
      This.Config := Backend.Create (BD_Ptr.Object_Pointer (BD));

      Err := Mount (This.LFS, This.Config.all);
      if Err /= 0 then
         raise Program_Error with "mount: " & Error_Img (Err);
      end if;
   end Mount;

   --------------
   -- Make_Dir --
   --------------

   overriding
   procedure Make_Dir (This      : in out Instance;
                       Path      :        Target_Path)
   is
      Err : int;

      Full_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin

      for Dir of Path loop
         Full_Path := Full_Path & "/" & Dir;
         Simple_Logging.Always ("LFS: Making dir: '" &
                                  To_String (Full_Path) & "'");
         Err := Mkdir (This.LFS, To_String (Full_Path));
         if Err not in LFS_ERR_OK | LFS_ERR_EXIST then
            raise Program_Error with "mkdir: " & Error_Img (Err);
         end if;
      end loop;
   end Make_Dir;

   ----------
   -- Tree --
   ----------

   overriding
   function Tree (This      : in out Instance;
                  Path      :        Target_Path)
                  return Directory_Tree
   is

      function Tree_Rec (Dir_Path : String) return Directory_Tree is
         Dir : aliased LFS_Dir;
         Err : int;
         Info : aliased Entry_Info;

         Res : Node_Vectors.Vector;
      begin
         Simple_Logging.Always ("Listing path: '" & Dir_Path & "'");

         Err := Open (This.LFS, Dir, Dir_Path);
         if Err /= LFS_ERR_OK then
            raise Program_Error with "tree: " & Error_Img (Err);
         end if;

         loop
            Err := Read (This.LFS, Dir, Info);

            case Err is
               when LFS_ERR_OK =>
                  exit; --  End of directory
               when int'First .. -1 =>
                  raise Program_Error with "tree: " & Error_Img (Err);
               when 1 .. int'Last  =>

                  declare
                     Name : constant String := Standard.Littlefs.Name (Info);
                  begin
                     if Name /= "." and then Name /= ".." then
                        case Kind (Info) is
                        when Register =>
                           Res.Append
                             (new Node'(Kind => File,
                                        Name => To_Unbounded_String (Name)));
                        when Directory =>
                           Res.Append
                             (new Node'(Kind    => FSmaker.Dir,
                                        Name    => To_Unbounded_String (Name),
                                        Entries =>
                                           Tree_Rec (Dir_Path & Name & "/")));
                        end case;
                     end if;
                  end;
            end case;
         end loop;

         Err := Close (This.LFS, Dir);
         if Err /= LFS_ERR_OK then
            raise Program_Error with "tree: " & Error_Img (Err);
         end if;

         return Res;
      end Tree_Rec;

      Full_Path : constant String := "/" & Path.Flatten ("/") & "/";
   begin
      return Tree_Rec (Full_Path);
   end Tree;

   ------------
   -- Import --
   ------------

   overriding
   procedure Import (This      : in out Instance;
                     Path      :        Target_Path;
                     Src       : in out Source.Class)
   is
      Err : int;
      File : aliased LFS_File;

      Full_Path : constant String := "/" & Path.Flatten ("/");
      Dir_Path : Target_Path := Path;
   begin
      --  Remove filename from the path
      Dir_Path.Delete_Last;

      --  Make sure the directory hierarchy exists
      This.Make_Dir (Dir_Path);

      Err := Open (This.LFS, File, Full_Path, LFS_O_CREAT or LFS_O_WRONLY);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "import: " & Error_Img (Err);
      end if;

      declare
         Buffer : array (1 .. 4046) of Unsigned_8;
         Read_Len : Natural;
         Write_Len : Integer_32;
      begin
         loop
            Read_Len := Src.Read (Buffer'Address, Buffer'Length);

            exit when Read_Len = 0;

            Write_Len := Write (This.LFS,
                                File,
                                Buffer'Address,
                                Unsigned_32 (Read_Len));

            if Write_Len < 0 then
               raise Program_Error with "import: " &
                 Error_Img (int (Write_Len));
            end if;

            exit when Write_Len /= Integer_32 (Read_Len);

         end loop;
      end;

      Err := Close (This.LFS, File);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "import: " & Error_Img (Err);
      end if;
   end Import;

   ---------
   -- Cat --
   ---------

   overriding
   procedure Cat (This : in out Instance;
                  Path :        Target_Path;
                  Dst  : in out FSmaker.Sink.Class)
   is
      Err : int;
      File : aliased LFS_File;
      Full_Path : constant String := "/" & Path.Flatten ("/");
   begin
      Err := Open (This.LFS, File, Full_Path, LFS_O_RDONLY);
      if Err /= LFS_ERR_OK then
         raise Program_Error with "cat: " & Error_Img (Err);
      end if;

      loop
         declare
            Buffer : array (1 .. 4096) of Unsigned_8;
            Read_Len : Integer_32;
            Write_Len : Natural;
         begin
            Read_Len := Read (This.LFS, File, Buffer'Address, Buffer'Length);

            exit when Read_Len = 0;

            if Read_Len < 0 then
               raise Program_Error with "cat: " & Error_Img (Err);
            end if;

            Write_Len := Dst.Write (Buffer'Address, Integer (Read_Len));

            if Integer_32 (Write_Len) /= Read_Len then
               raise Program_Error with "Disk full?";
            end if;

            exit when Read_Len /= Buffer'Length;
         end;
      end loop;

      Dst.Close;
   end Cat;

end FSmaker.Target.LittleFS;
