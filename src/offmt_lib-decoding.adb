with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;

with System.Storage_Elements; use System.Storage_Elements;

with COBS.Stream.Decoder;

with Offmt_Lib.Storage;
with Offmt_Lib.Fmt_Data;

package body Offmt_Lib.Decoding is

   procedure Handle_Frame (Map : Trace_Map; Frame : in out Data_Frame);
   procedure Put_Format (Fmt : Format; Frame : in out Data_Frame);
   procedure Decode_From_File (Map : Trace_Map; File : in out File_Type);

   type COBS_Decoder is new COBS.Stream.Decoder.Instance with record
      Frame : Data_Frame;
      Map  : Trace_Map;
   end record;

   overriding
   procedure Flush (This : in out COBS_Decoder;
                    Data : Storage_Array);

   overriding
   procedure End_Of_Frame (This : in out COBS_Decoder);

   -----------
   -- Flush --
   -----------

   overriding
   procedure Flush (This : in out COBS_Decoder;
                    Data : Storage_Array)
   is
   begin
      for Elt of Data loop
         Push (This.Frame, Elt);
      end loop;
   end Flush;

   ------------------
   -- End_Of_Frame --
   ------------------

   overriding
   procedure End_Of_Frame (This : in out COBS_Decoder) is
   begin
      Handle_Frame (This.Map, This.Frame);
      Clear (This.Frame);
   end End_Of_Frame;

   ----------------
   -- Put_Format --
   ----------------

   procedure Put_Format (Fmt : Format; Frame : in out Data_Frame) is
      Data : Fmt_Data.Instance'Class := Fmt_Data.Create (Fmt.Typ);
   begin
      Data.From_Frame (Frame);
      Put (Data.Format (Fmt.Kind));
   end Put_Format;

   ------------------
   -- Handle_Frame --
   ------------------

   procedure Handle_Frame (Map : Trace_Map; Frame : in out Data_Frame) is

      function Pop_Id (Frame : in out Data_Frame) return Trace_ID;

      function Pop_Id (Frame : in out Data_Frame) return Trace_ID
      is
         Result : Trace_ID;
      begin
         if Remaining (Frame) < 2 then
            raise Frame_Error with "Frame too short for ID";
         else
            Result := Trace_ID (Pop (Frame));
            Result := Result or Shift_Left (Trace_ID (Pop (Frame)), 8);
            return Result;
         end if;
      end Pop_Id;

      Id : Trace_ID;
   begin
      --  Put_Line ("We have a frame of" & Remaining (Frame)'Img & " bytes");

      Id := Pop_Id (Frame);
      --  Put_Line ("Frame #" & Id'Img);

      declare
         T : constant Trace := Map.Element (Id);
      begin
         for Elt of T.List loop
            case Elt.Kind is
               when Plain_String =>
                  Put (To_String (Elt.Str));
               when Format_String =>
                  Put_Format (Elt.Fmt, Frame);
            end case;
         end loop;
         New_Line;
      end;

   exception
      when E : Frame_Error =>
         Put_Line ("Frame Error: " & Ada.Exceptions.Exception_Message (E));
   end Handle_Frame;

   ----------------------
   -- Decode_From_File --
   ----------------------

   procedure Decode_From_File (Map : Trace_Map; File : in out File_Type) is
      C : Character;

      Decoder : COBS_Decoder;
   begin
      Decoder.Map := Map;
      loop
         Get_Immediate (File, C);
         Decoder.Push (Storage_Element (C'Enum_Rep));
      end loop;
   exception
      when End_Error =>
         Close (File);
   end Decode_From_File;

   ------------
   -- Decode --
   ------------

   procedure Decode (Map_Filename : String) is
      Load : constant Storage.Load_Result := Storage.Load (Map_Filename);

      File : File_Type := Standard_Input;
   begin
      if Load.Success then
         Decode_From_File (Load.Map, File);
      else
         Put_Line ("Cannot open map file '" & Map_Filename & "': " &
                     To_String (Load.Msg));
         raise Program_Error;
      end if;
   end Decode;

end Offmt_Lib.Decoding;
