with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Offmt_Lib is

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print (T : Trace) is
   begin
      Put_Line ("-- Format --");
      Put_Line (" Original: '" & To_String (T.Original) & "'");
      Put_Line (" Id:" & T.Id'Img);
      Put_Line (" Elements:");

      for Elt of T.List loop
         case Elt.Kind is
            when Plain_String =>
               Put_Line (" - Plain: '" & To_String (Elt.Str) & "'");
            when Format_String =>
               declare
                  Fmt : constant Format := Elt.Fmt;
               begin
                  Put_Line (" - Format:");
                  Put_Line ("    - Expr: '" & To_String (Fmt.Expression) & "'");
                  Put_Line ("    - Type: '" & Fmt.Typ'Img & "'");
                  Put_Line ("    - Kind: '" & Fmt.Kind'Img & "'");
               end;
         end case;
      end loop;
   end Pretty_Print;

   ---------------
   -- Remaining --
   ---------------

   function Remaining (Frame : Data_Frame) return Natural
   is
   begin
      return (Natural (Frame.Data.Length) - Frame.Next);
   end Remaining;

   -----------
   -- Clear --
   -----------

   procedure Clear (Frame : in out Data_Frame) is
   begin
      Frame.Data.Clear;
      Frame.Next := 0;
   end Clear;

   ----------
   -- Push --
   ----------

   procedure Push (Frame : in out Data_Frame;
                   Elt   :        System.Storage_Elements.Storage_Element)
   is
   begin
      Frame.Data.Append (Elt);
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop (Frame : in out Data_Frame)
                 return System.Storage_Elements.Storage_Element
   is
      use System.Storage_Elements;
   begin
      if Remaining (Frame) > 0 then
         return Res : Storage_Element do
            Res := Frame.Data.Element (Frame.Data.First_Index + Frame.Next);
            Frame.Next := Frame.Next + 1;
         end return;
      else
         raise Program_Error;
      end if;
   end Pop;

end Offmt_Lib;
