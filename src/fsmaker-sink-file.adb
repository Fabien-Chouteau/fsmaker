with GNAT.OS_Lib; use GNAT.OS_Lib;

package body FSmaker.Sink.File is

   ------------
   -- Create --
   ------------

   function Create (Path : String) return Sink.Class is
      Res : Instance;
   begin
      Res.FD := Open_Append (Path, Binary);

      if Res.FD = Invalid_FD then
         raise Program_Error with "Cannot open file sink '" & Path
           & "': " & Errno_Message;
      end if;

      Res.Do_Not_Close := False;
      return Res;
   end Create;

   ------------
   -- Create --
   ------------

   function Create (FD : GNAT.OS_Lib.File_Descriptor) return Sink.Class
   is
      Res : Instance;
   begin
      Res.FD := FD;
      Res.Do_Not_Close := True;
      return Res;
   end Create;

   -----------
   -- Write --
   -----------

   overriding
   function Write
     (This : in out Instance; Addr : System.Address; Len : Natural)
      return Natural
   is
   begin
      return GNAT.OS_Lib.Write (This.FD, Addr, Len);
   end Write;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (This : in out Instance) is
   begin
      if This.Do_Not_Close then
         return;
      else
         GNAT.OS_Lib.Close (This.FD);
      end if;
   end Close;

end FSmaker.Sink.File;
