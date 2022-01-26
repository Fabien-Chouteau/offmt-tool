with Ada.Text_IO;
with Ada.Strings.Fixed;
with System.Storage_Elements; use System.Storage_Elements;

package body Offmt_Lib.Fmt_Data.Unsigned_Generic is

   Size_In_Bytes : constant Natural := T'Size / 8;

   package T_IO is new Ada.Text_IO.Modular_IO (T);

   function Image (Val : T; Kind : Format_Kind) return String;

   -----------
   -- Image --
   -----------

   function Image (Val : T; Kind : Format_Kind) return String is
      Result : String (1 .. (case Kind is
                          when Decimal => Size_In_Bytes * 2 + 4,
                          when Hexadecimal => Size_In_Bytes * 2 + 4,
                          when Binary      => T'Size + 4));
   begin
      T_IO.Put (To   => Result,
                Item => Val,
                Base => (case Kind is
                            when Decimal => 10,
                            when Hexadecimal => 16,
                            when Binary      => 2));

      case Kind is
         when Decimal =>
            return Ada.Strings.Fixed.Trim (Result, Ada.Strings.Left);

         when Hexadecimal =>
            declare
               Trim : constant String
                 := Ada.Strings.Fixed.Trim (Result, Ada.Strings.Left);
            begin
               --  Remove the '16#' and '#' from the hexadecimal image
               return Trim (Trim'First + 3 .. Trim'Last - 1);
            end;

         when Binary =>
            declare
               Trim : constant String
                 := Ada.Strings.Fixed.Trim (Result, Ada.Strings.Left);
            begin
               --  Remove the '2#' and '#' from the binary image
               return Trim (Trim'First + 2 .. Trim'Last - 1);
            end;
      end case;
   end Image;

   ----------------
   -- From_Frame --
   ----------------

   overriding
   procedure From_Frame (This : in out Instance;
                         Frame : in out Data_Frame)
   is
      function Shift_Left
        (Value  : T;
         Amount : Natural) return T
        with Import, Convention => Intrinsic;

      Elt : Storage_Element;
   begin
      if Remaining (Frame) < Size_In_Bytes then
         raise Frame_Error with "Frame too short for U" & T'Size'Img;
      else
         This.Val := 0;
         for Cnt in 0 .. Size_In_Bytes - 1 loop
            Elt := Pop (Frame);
            This.Val := This.Val or Shift_Left (T (Elt), 8 * Cnt);
         end loop;
      end if;
   end From_Frame;

   ------------
   -- Format --
   ------------

   overriding
   function Format (This : Instance;
                    Kind : Format_Kind)
                    return String
   is
   begin
      return Image (This.Val, Kind);
   end Format;

   ---------
   -- Put --
   ---------

   overriding
   procedure Put (This : Instance; Kind : Format_Kind) is
   begin
      Ada.Text_IO.Put (This.Format (Kind));
   end Put;

   -----------
   -- Value --
   -----------

   function Value (This : Instance) return T is
   begin
      return This.Val;
   end Value;

end Offmt_Lib.Fmt_Data.Unsigned_Generic;
