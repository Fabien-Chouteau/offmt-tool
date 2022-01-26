with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Atomic.Unsigned;

package body Offmt_Lib.Parser is

   function Parse_Format (Str : String) return Trace_Element;
   function Create_Plain_String (Str : String) return Trace_Element;

   package Atomic_Id_Counter is new Atomic.Unsigned (Trace_ID);

   Id_Counter : aliased Atomic_Id_Counter.Instance :=
     Atomic_Id_Counter.Init (0);

   ------------------
   -- Parse_Format --
   ------------------

   function Parse_Format (Str : String) return Trace_Element is
      Result : Format;
      Col : Natural;
   begin
      Result.Kind := Decimal;

      Put_Line ("Parsing Format: '" & Str & "'");

      Col := Index (Str, ":", Str'First);
      if Col = 0 then
         raise Program_Error with "Invalid fmt (no :)";
      end if;

      Result.Expression := To_Unbounded_String (Str (Col + 1 .. Str'Last));

      declare
         Fmt_Param : constant String := Str (Str'First .. Col - 1);
         Last : Natural := Fmt_Param'First;
      begin
         if Fmt_Param'Length = 0 then
            raise Program_Error with "Invalid fmt (no param)";
         end if;

         case Fmt_Param (Last) is
            when 'd' =>
               Result.Kind := Decimal;
               Last := Last + 1;
            when 'x' =>
               Result.Kind := Hexadecimal;
               Last := Last + 1;
            when 'b' =>
               Result.Kind := Binary;
               Last := Last + 1;
            when others =>
               null;
         end case;

         declare
            Type_Str : constant String := Fmt_Param (Last .. Fmt_Param'Last);
         begin
            if Fmt_Param'Length = 0 then
               raise Program_Error with "Invalid fmt (no format type)";
            end if;

            if Type_Str = "u8" then
               Result.Typ := Type_U8;
            elsif Type_Str = "u16" then
               Result.Typ := Type_U16;
            elsif Type_Str = "u32" then
               Result.Typ := Type_U32;
            else
               raise Program_Error with "Invalid fmt (unknown type: '" &
                 Type_Str  & "')";
            end if;
         end;
      end;

      return (Kind => Format_String, Fmt => Result);
   end Parse_Format;

   -------------------------
   -- Create_Plain_String --
   -------------------------

   function Create_Plain_String (Str : String) return Trace_Element is
   begin
      return (Kind => Plain_String, Str => To_Unbounded_String (Str));
   end Create_Plain_String;

   -----------
   -- Parse --
   -----------

   function Parse (Str : String) return Trace is
      Result : Trace;

      Last : Natural := Str'First;
      From : Natural;
      To   : Natural;
   begin
      Put_Line ("Parsing Trace: '" & Str & "'");
      Result.Original := To_Unbounded_String (Str);

      while Last in Str'Range loop
         From := Index (Str, "{", Last);

         if From /= 0 then

            if From - 1 >= Last then
               Result.List.Append
                 (Create_Plain_String (Str (Last .. From - 1)));
            end if;

            To := Index (Str, "}", From + 1);

            if To /= 0 then
               Put_Line ("Slice: " & From'Img & " .." & To'Img);
               declare
                  Fmt : constant Trace_Element :=
                    Parse_Format (Str (From + 1 .. To - 1));
               begin
                  Put_Line (Fmt.Fmt.Kind'Img);
                  Put_Line (To_String (Fmt.Fmt.Expression));
                  Result.List.Append (Fmt);
               end;

               Last := To + 1;
            else
               raise Program_Error with "unmatched '}'";
            end if;
         else
            exit;
         end if;
      end loop;

      if Last in Str'Range then
         Result.List.Append (Create_Plain_String (Str (Last .. Str'Last)));
      end if;

      Result.Id := Atomic_Id_Counter.Add_Fetch (Id_Counter, 1);
      return Result;
   end Parse;

end Offmt_Lib.Parser;
