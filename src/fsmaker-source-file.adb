with GNAT.OS_Lib; use GNAT.OS_Lib;

with Simple_Logging;

package body FSmaker.Source.File is

   ------------
   -- Create --
   ------------

   function Create (Src : String) return Instance is
      Res : Instance;
   begin

      --  if AAA.Strings.Has_Prefix (Src, "http") then
      --     declare
      --        Proc : Spawn.Processes.Process;
      --        Args : Spawn.String_Vectors.UTF_8_String_Vector;
      --     begin
      --        Proc.Set_Program ("wget");
      --        Args.Append (Src);
      --        Proc.Set_Arguments (Args);
      --        Proc.Start;
      --
      --        loop
      --           exit when Proc.Status = Not_Running;
      --        end loop;
      --     end;
      --  end if;

      Res.FD := Open_Read (Src, Binary);

      if Res.FD = Invalid_FD then
         raise Program_Error with "Cannot open source '" & Src & "'";
      end if;

      Simple_Logging.Always ("File open: '" & Src & "'");

      return Res;
   end Create;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance) is
   begin
      if This.FD /= Invalid_FD then
         Close (This.FD);
      end if;
   end Close;

   ----------
   -- Read --
   ----------

   overriding
   function Read
     (This : in out Instance; Addr : System.Address; Len : Natural)
      return Natural
   is
      Res : constant Integer := Read (This.FD, Addr, Len);
   begin
      if Res < 0 then
         raise Program_Error
           with "Source read error: " & GNAT.OS_Lib.Errno_Message;
      end if;

      return Res;
   end Read;

end FSmaker.Source.File;
