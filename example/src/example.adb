with Offmt; use Offmt;

procedure Example is
   X : constant Integer := 42;
   pragma Unreferenced (X);

   Str : constant String := "lol";
begin

   Offmt.Log ("Test X: {u32:X}");
   Offmt.Log ("Test X + 1: {u32:X + 1}");
   Log ("Test X * 10: {u32:X * 10}");
   Log ("Test hex: 0x{xu32:X - 1}");
   Log ("Test binary: 0b{bu32:X - 1}");
   Log ("Test hex u16: 0x{xu16:X}");
   Log ("Test hex u8: 0x{xu8:X - 1}");
   Log ("Is that a literal? " & Str);
   Log ("Multiple formats X:{u32:X} X - 1:{u32:X - 1} X + 1:{u32:X + 1}");
   Log ("A string without format");
   Log ("{u32:X}"); --  Only a format
   Log ("{u32:X} format at the beginning");
   Log ("Format at the end {u32:X}");
end Example;
