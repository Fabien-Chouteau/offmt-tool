package Offmt_Lib.Fmt_Data is

   type Instance
   is abstract tagged private;

   procedure From_Frame (This  : in out Instance;
                         Frame : in out Data_Frame)
   is abstract;

   function Format (This : Instance;
                    Kind : Format_Kind)
                    return String
   is abstract;

   procedure Put (This : Instance;
                  Kind : Format_Kind)
   is abstract;

   function Create (Typ : Format_Type) return Instance'Class;

private

   type Instance
   is abstract tagged null record;

end Offmt_Lib.Fmt_Data;
