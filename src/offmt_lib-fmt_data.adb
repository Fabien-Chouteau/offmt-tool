with Offmt_Lib.Fmt_Data.Unsigned_8;
with Offmt_Lib.Fmt_Data.Unsigned_16;
with Offmt_Lib.Fmt_Data.Unsigned_32;

package body Offmt_Lib.Fmt_Data is

   ------------
   -- Create --
   ------------

   function Create (Typ : Format_Type) return Instance'Class is
   begin
      case Typ is
         when Type_U8 =>
            declare
               Ret : Fmt_Data.Unsigned_8.Instance;
            begin
               return Ret;
            end;
         when Type_U16 =>
            declare
               Ret : Fmt_Data.Unsigned_16.Instance;
            begin
               return Ret;
            end;
         when Type_U32 =>
            declare
               Ret : Fmt_Data.Unsigned_32.Instance;
            begin
               return Ret;
            end;
      end case;
   end Create;

end Offmt_Lib.Fmt_Data;
