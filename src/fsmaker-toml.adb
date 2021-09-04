with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with TOML; use TOML;
with TOML.File_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with CLIC.User_Input; use CLIC.User_Input;

with Simple_Logging; use Simple_Logging;

with FSmaker.Target;
with FSmaker.Target.LittleFS;
with FSmaker.Source;

package body FSmaker.TOML is

   type FS_Format is (LFS);

   type Image_Info is record
      Format : FS_Format;
      Size   : Natural;
   end record;

   function Open_Image (Path_To_Output : String) return File_Descriptor;
   function Get_Image_Info (V : TOML_Value) return Image_Info;

   ----------------
   -- Open_Image --
   ----------------

   function Open_Image (Path_To_Output : String) return File_Descriptor is
      FD : File_Descriptor;
   begin
      if not Is_Regular_File (Path_To_Output) then

         --  The file doesn't exists, we try to create it
         FD := Create_File (Path_To_Output, Binary);

      elsif not GNAT.OS_Lib.Is_Owner_Writable_File (Path_To_Output) then
         raise Program_Error
           with "Image file '" & Path_To_Output & "' is not writable";
      else

         Always ("Existing image file '" & Path_To_Output &
                   "' will be overwritten.");
         if Query ("Do you want to continue?",
                   Valid    => (Yes | No => True, Always => False),
                   Default  => Yes) = Yes
         then
            FD := Open_Read_Write (Path_To_Output, Binary);
         else
            raise Program_Error with "Cannot overwrite existing file";
         end if;
      end if;

      if FD = Invalid_FD then
         raise Program_Error with "Cannot open image file '" & Path_To_Output & "'";
      end if;

      return FD;
   end Open_Image;

   --------------------
   -- Get_Image_Info --
   --------------------

   function Get_Image_Info (V : TOML_Value) return Image_Info is
      Res : Image_Info;
   begin
      declare
         Format : constant TOML_Value := V.Get_Or_Null ("format");
      begin
         if Format.Is_Null then
            raise Program_Error with "missing 'format'";
         elsif Format.Kind /= TOML_String then
            raise Program_Error with "'format' should be a string";
         end if;

         if Format.As_String /= "littlefs" then
            raise Program_Error with "Unknown image format '" &
              Format.As_String & "' (use littlefs)";
         end if;
      end;

      declare
         Size : constant TOML_Value := V.Get_Or_Null ("size");
      begin
         if Size.Is_Null then
            raise Program_Error with "missing 'size'";
         elsif Size.Kind /= TOML_Integer then
            raise Program_Error with "'size' should be an integer";
         end if;

         Res.Size := Natural (Size.As_Integer);
      end;

      return Res;
   end Get_Image_Info;

   -----------
   -- Mkdir --
   -----------

   procedure Mkdir (T : not null Target.Any_Filesystem_Ref;
                    Mk : TOML_Value)
   is
   begin
      if Mk.Is_Null then
         Simple_Logging.Always ("No dirs to make");
         return; -- No dir to make
      elsif Mk.Kind /= TOML_Array or else Mk.Item_Kind /= TOML_String then
         raise Program_Error with
           "'mkdir' should be an array of strings (" &
           Mk.Kind'Img & ")";
      end if;

      for Index in 1 .. Mk.Length loop
         declare
            Dir : constant String := MK.Item (Index).As_String;
         begin
            if not Valid_Target_Path (Dir) then
               raise Program_Error with "Invalid target path: '" & Dir & "'";
            else
               T.Make_Dir (To_Target_Path (Dir));
            end if;
         end;
      end loop;
   end Mkdir;

   ------------
   -- Import --
   ------------

   procedure Import (T : not null Target.Any_Filesystem_Ref;
                     Imp : TOML_Value)
   is
   begin
      if Imp.Is_Null then
         Simple_Logging.Always ("No imports");
         return;
      elsif imp.Kind /= TOML_Table then
         raise Program_Error with
           "'[import]' section should be an table (" &
           Imp.Kind'Img & ")";
      end if;

      for Elt of Imp.Iterate_On_Table loop
         declare
            Key : constant String := To_String (Elt.Key);
            Val : constant TOML_Value := Elt.Value;
         begin
            if not Valid_Target_Path (Key) then
               raise Program_Error with "Invalid target path: '" & Key & "'";
            end if;

            if Val.Kind /= TOML_String then
               raise Program_Error with "Import source must be strings";
            end if;

            declare
               Src : Source.Instance := Source.Create (Val.As_String);
            begin
               T.Import (To_Target_Path (Key), Src);
            end;
         end;
      end loop;
   end Import;

   ------------------
   -- Process_TOML --
   ------------------

   procedure Process_TOML (Root : TOML_Value; FD : File_Descriptor) is
      Img_Info : Image_Info;

      T : Target.Any_Filesystem_Ref;
   begin
      if Root.Kind /= TOML_Table then
         raise Program_Error with "Invalid TOML file. Table expected";
      end if;

      Img_Info := Get_Image_Info (Root);

      T := new Target.LittleFS.Instance;
      T.Format (FD, Size => Img_Info.Size);

      T.Mount (FD);

      declare
         Mk : constant TOML_Value := Root.Get_Or_Null ("mkdir");
      begin
         Mkdir (T, Mk);
      end;

      declare
         Imp : constant TOML_Value := Root.Get_Or_Null ("import");
      begin
         Import (T, Imp);
      end;

   end Process_TOML;

   ---------------------
   -- Build_From_TOML --
   ---------------------

   procedure Build_From_TOML (Path_To_TOML, Path_To_Output : String) is

      Result : Read_Result := File_IO.Load_File (Path_To_TOML);
      FD : File_Descriptor;
   begin
      if Result.Success then
         FD := Open_Image (Path_To_Output);
         Process_TOML (Result.Value, FD);
      else
         raise Program_Error with Path_To_Toml & ":" & Format_Error (Result);
      end if;
   end Build_From_TOML;

end FSmaker.TOML;
