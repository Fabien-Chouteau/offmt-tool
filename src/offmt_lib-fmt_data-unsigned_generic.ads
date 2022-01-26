generic
   type T is mod <>;
package Offmt_Lib.Fmt_Data.Unsigned_Generic is

   subtype Parent is Fmt_Data.Instance;

   type Instance
   is new Parent with private;

   overriding
   procedure From_Frame (This  : in out Instance;
                         Frame : in out Data_Frame);

   overriding
   function Format (This : Instance;
                    Kind : Format_Kind)
                    return String;

   overriding
   procedure Put (This : Instance;
                  Kind : Format_Kind);

   function Value (This : Instance) return T;

private

   type Instance
   is new Parent with record
      Val : T;
      Loaded : Boolean := False;
   end record;

end Offmt_Lib.Fmt_Data.Unsigned_Generic;
